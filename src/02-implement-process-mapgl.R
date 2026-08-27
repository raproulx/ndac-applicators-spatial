# mapgl (MapLibre GL) equivalent of the Leaflet map in src/01-implement-process.R
# Companion script: run src/01-implement-process.R first. This reuses
# `dat_leaflet`, `title_ndac` and `add_google_analytics()` from that script and
# writes to index-mapgl.html so the deployed index.html is untouched.

library(tidyverse)
library(sf)
library(mapgl)
library(htmlwidgets)
library(htmltools)

stopifnot(
  exists("dat_leaflet"),
  exists("title_ndac"),
  exists("add_google_analytics")
)

# prepare data for mapgl --------------------------------------------------
# mapgl reads popup content from a feature property, so the HTML that Leaflet
# built inline with a formula has to become a column first.
pal_ndac <- c(Manned = "#658849", Unmanned = "#34499B")

dat_mapgl <- dat_leaflet |>
  mutate(
    # fmt: skip
    popup_html = str_c(
      "<b>", `BUSINESS NAME`, "</b><br><br>",
      "<b>OWNER/OPERATOR: </b>", `OWNER/OPERATOR`, "<br>",
      "<b>EMAIL: </b><a href='mailto:", `EMAIL`, "'>", `EMAIL`, "</a><br>",
      "<b>PHONE: </b><a href='tel:", `PHONE`, "'>", `PHONE`, "</a><br>",
      "<b>CITY/STATE: </b>", `CITY`, ", ", `STATE`, "<br>",
      "<b>CHIEF PILOT: </b>", `CHIEF PILOT (RESPONSIBLE FOR ALL PILOTS)`, "<br>",
      "<b>ADDL PILOTS: </b>", `ADDL PILOTS`, "<br>",
      "<b>TYPE OF LICENSE: </b>", `TYPE OF LICENSE`, "<br>"
    )
  )

style_standard <- carto_style("voyager")
style_light <- carto_style("positron")

# create mapgl map --------------------------------------------------------
# Two filtered layers rather than one layer with a colour ramp: the layer ids
# are what add_layers_control() toggles, so this reproduces the Leaflet
# overlayGroups. Circle styling mirrors the addCircleMarkers() defaults
# (radius 10, weight 5, opacity 0.5, fillOpacity 0.2).
m <- maplibre(
  style = style_standard,
  # mapgl defaults to a globe; Leaflet was flat Web Mercator
  projection = "mercator",
  # Leaflet cannot rotate, and there is no compass to reset with
  dragRotate = FALSE,
  bounds = dat_mapgl,
  minZoom = 5,
  maxZoom = 12,
  width = "100%",
  height = "100vh"
) |>
  add_source(
    id = "applicators",
    data = dat_mapgl
  ) |>
  add_circle_layer(
    id = "Manned",
    source = "applicators",
    filter = list("==", get_column("TYPE OF LICENSE"), "Manned"),
    circle_color = pal_ndac[["Manned"]],
    circle_opacity = 0.2,
    circle_radius = 10,
    circle_stroke_color = pal_ndac[["Manned"]],
    circle_stroke_width = 5,
    circle_stroke_opacity = 0.5,
    popup = "popup_html"
  ) |>
  add_circle_layer(
    id = "Unmanned",
    source = "applicators",
    filter = list("==", get_column("TYPE OF LICENSE"), "Unmanned"),
    circle_color = pal_ndac[["Unmanned"]],
    circle_opacity = 0.2,
    circle_radius = 10,
    circle_stroke_color = pal_ndac[["Unmanned"]],
    circle_stroke_width = 5,
    circle_stroke_opacity = 0.5,
    popup = "popup_html"
  ) |>
  add_navigation_control(
    position = "top-left",
    # Leaflet's default zoom control is zoom-only, and the map does not rotate
    show_compass = FALSE
  ) |>
  add_fullscreen_control(
    position = "top-left"
  ) |>
  add_geocoder_control(
    position = "top-left",
    placeholder = "Address Search..."
  ) |>
  add_scale_control(
    position = "bottom-left",
    unit = "imperial",
    max_width = 100
  ) |>
  add_categorical_legend(
    legend_title = title_ndac,
    values = c("Manned", "Unmanned"),
    colors = unname(pal_ndac),
    patch_shape = "circle",
    position = "top-right",
    # The legend, the layers control and the basemap switcher are positioned
    # independently of each other, so stack them by hand down the top-right:
    # legend (36-156), basemap switcher (166-242, pushed down by the
    # .maplibregl-ctrl-top-right margin below), layers control (252-392).
    margin_top = 10,
    margin_right = 10
  ) |>
  add_layers_control(
    position = "top-right",
    layers = c("Manned", "Unmanned"),
    collapsible = TRUE,
    use_icon = FALSE,
    margin_top = 236,
    margin_right = 10
  ) |>
  add_control(
    # fmt: skip
    html = str_c(
      "<div class='ndac-basemap'>",
      "<div class='ndac-basemap-title'>Basemap</div>",
      "<label><input type='radio' name='ndac-basemap' value='standard' checked> Standard</label>",
      "<label><input type='radio' name='ndac-basemap' value='light'> Light</label>",
      "</div>"
    ),
    position = "top-right",
    className = "maplibregl-ctrl maplibregl-ctrl-group",
    id = "basemap"
  )

# m

# capture the MapLibre map instance ---------------------------------------
# mapgl keeps the map inside the widget's renderValue closure and does not
# register it anywhere reachable, so wrap the constructor before
# HTMLWidgets.staticRender() runs. This is the one undocumented hook here.
js_capture_map <- HTML(
  "
  (function () {
    function patch() {
      if (!window.maplibregl || window.maplibregl.__ndacPatched) return false;
      window.maplibregl.__ndacPatched = true;
      var Orig = window.maplibregl.Map;
      window.maplibregl.Map = function (opts) {
        var m = new Orig(opts);
        window._ndacMap = m;
        return m;
      };
      window.maplibregl.Map.prototype = Orig.prototype;
      return true;
    }
    // Must patch synchronously: HTMLWidgets renders on DOMContentLoaded, which
    // beats any timer, so a polled patch lands after the map is constructed.
    if (!patch()) {
      var iv = setInterval(function () {
        if (patch() || window.maplibregl) clearInterval(iv);
      }, 5);
    }
  })();
"
)

# basemap switcher --------------------------------------------------------
# setStyle() discards user-added sources and layers, so snapshot and re-add
# them. The layer-scoped click handlers mapgl registered for the popups live on
# the map rather than the style, so they resume once the layer ids exist again.
js_basemap_switch <- sprintf(
  "
  function (el, x) {
    var STYLES = { standard: '%s', light: '%s' };
    var IDS = ['Manned', 'Unmanned'];

    function swap(url) {
      var map = window._ndacMap;
      if (!map) return;

      var src = map.getStyle().sources['applicators'];
      var lyr = map.getStyle().layers.filter(function (l) {
        return IDS.indexOf(l.id) > -1;
      });
      var vis = {};
      lyr.forEach(function (l) {
        vis[l.id] = map.getLayoutProperty(l.id, 'visibility');
      });

      map.setStyle(url);
      map.once('style.load', function () {
        map.addSource('applicators', src);
        lyr.forEach(function (l) {
          map.addLayer(l);
          if (vis[l.id]) {
            map.setLayoutProperty(l.id, 'visibility', vis[l.id]);
          }
        });
      });
    }

    // Delegated: mapgl adds its controls on map load, after onRender runs, so
    // the radios do not exist yet at this point.
    el.addEventListener('change', function (e) {
      var t = e.target;
      if (t && t.name === 'ndac-basemap' && STYLES[t.value]) {
        swap(STYLES[t.value]);
      }
    });
  }
",
  style_standard,
  style_light
)

# write mapgl map to HTML -------------------------------------------------
saveWidget(
  prependContent(
    m,
    tags$script(js_capture_map),
    tags$style(HTML(
      "
      .map-caption {
        position: fixed;
        bottom: 10px;
        left: 50%;
        transform: translateX(-50%);
        z-index: 1000;
        background: rgba(255,255,255,0.75);
        color: #555;
        font-family: Helvetica Neue, Arial, Helvetica, sans-serif;
        font-weight: bold;
        font-size: 11px;
        padding: 4px 10px;
        border-radius: 3px;
        white-space: nowrap;
        pointer-events: none;

        /* Ensures it never exceeds viewport, and only wraps if it truly must */
        max-width: 100vw;
        box-sizing: border-box;
        overflow-wrap: break-word;
      }

      .maplibregl-popup-content {
        font-size: 14px;
        line-height: 1.5;
        min-width: 200px;
     }
      .maplibregl-popup-content b { font-size: 15px; }
      .maplibregl-popup-content a { font-size: 14px; }

      /* push the basemap switcher below the legend (only it sits in this
         corner; the legend and layers control position themselves) */
      .maplibregl-ctrl-top-right { margin-top: 140px; }

      .ndac-basemap {
        padding: 6px 10px;
        font-family: Helvetica Neue, Arial, Helvetica, sans-serif;
        font-size: 12px;
        line-height: 1.6;
      }
      .ndac-basemap-title { font-weight: bold; }
      .ndac-basemap label { display: block; font-weight: normal; }

      .ndac-layers-title {
        font-family: Helvetica Neue, Arial, Helvetica, sans-serif;
        font-size: 12px;
        font-weight: bold;
        text-align: left;
        padding: 0 10px;
      }

      @media (max-width: 768px) {
        .map-caption {
          font-size: 13px;
          bottom: 24px;
        }

        .maplibregl-popup-content {
          font-size: 16px;
          line-height: 1.6;
          min-width: 220px;
        }
        .maplibregl-popup-content b { font-size: 17px; }
        .maplibregl-popup-content a { font-size: 16px; }
      }
    "
    )),
    tags$div(
      class = "map-caption",
      "*locations are approximate to maintain privacy*"
    ),
    tags$head(tags$link(
      rel = "stylesheet",
      href = "https://unpkg.com"
    )),
    tags$head(tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
    ))
  ) |>
    onRender(
      "
    function(el, x) {
      document.title = 'ND Licensed Aerial Applicators';
    }
  "
    ) |>
    onRender(js_basemap_switch) |>
    onRender(
      "
  function(el, x) {
    var supportsFullscreen = document.fullscreenEnabled ||
                             document.webkitFullscreenEnabled;
    var isTouchPrimary = window.matchMedia('(pointer: coarse)').matches;

    // Fullscreen button behavior
    var btn = document.querySelector('.maplibregl-ctrl-fullscreen');
    if (btn && (!supportsFullscreen || isTouchPrimary)) {
      btn.addEventListener('click', function(e) {
        e.stopPropagation();
        e.preventDefault();
        window.open(window.location.href, '_blank');
      });
    }

    // Poll until the layers control is available in the DOM. mapgl renders it
    // as .layers-control (a child of the widget div, not a .maplibregl-ctrl),
    // holding a .toggle-button and a .layers-list of <a> toggles.
    var interval = setInterval(function() {
      var layersControl = document.querySelector('.layers-control');
      var layersList = layersControl &&
        layersControl.querySelector('.layers-list');
      if (!layersList) return;
      clearInterval(interval);

      layersList.insertAdjacentHTML(
        'beforebegin',
        '<div class=\"ndac-layers-title\">Applicators</div>'
      );

      // Keep expanded on desktop; leave mapgl's collapsed default on touch
      if (!isTouchPrimary && layersList.offsetParent === null) {
        var toggle = layersControl.querySelector('.toggle-button');
        if (toggle) toggle.click();
      }
    }, 50);
  }
"
    ),
  file = "index-mapgl.html"
)


# append Google Analytics to HTML -----------------------------------------
add_google_analytics(
  "index-mapgl.html",
  measurement_id = Sys.getenv("NDACMAP_GA_MEASUREMENT_ID")
)


# functionality that does not port from Leaflet ---------------------------
# Needs custom JS (no mapgl API):
#   1. Basemap switching in static HTML. add_layers_control() toggles overlay
#      layers only; set_style() outside Shiny only sets the initial style.
#   2. Reaching the map instance from onRender(). Not exposed by mapgl -- the
#      constructor patch above is undocumented and is the piece most likely to
#      break on a mapgl or MapLibre upgrade. If it does, only the basemap
#      switcher is affected. It must run synchronously: HTMLWidgets renders on
#      DOMContentLoaded, so a polled patch lands after the map is built.
#   3. Group headings in the layers control. mapgl renders it as
#      .layers-control (a child of the widget div, NOT a .maplibregl-ctrl),
#      holding a .toggle-button and a .layers-list of <a> toggles, and adds it
#      on map load -- after onRender runs. Hence the poll here, and the
#      delegated change handler for the basemap radios.
#   4. Stacking the top-right corner. Leaflet stacks its controls in one corner
#      container; mapgl's legend, layers control and custom controls are each
#      positioned independently and overlap by default, so the legend and
#      layers control carry hand-tuned margin_top values.
#
# No equivalent, dropped:
#   5. addFullscreenControl(pseudoFullscreen = FALSE). MapLibre's fullscreen
#      control has no pseudo-fullscreen mode; the open-in-new-tab fallback for
#      touch devices is ported and still applies.
#   6. scaleBarOptions(updateWhenIdle = TRUE). add_scale_control() exposes only
#      position, unit and max_width.
#   7. addSearchOSM() search options. add_geocoder_control() uses Nominatim for
#      MapLibre maps but exposes only position, placeholder, collapsed,
#      provider and maptiler_api_key. The countrycodes=us restriction, the
#      custom propertyName/propertyLoc, and the styled result marker (radius,
#      weight, color = "#e03", animate) have no documented equivalent, so
#      results are worldwide rather than US-only.
#   8. The CARTO ?key= parameter. It applies to the raster tile URLs, not to
#      the GL style JSON, which is served unauthenticated. Attribution is
#      carried inside the vector style, so the explicit attribution strings are
#      no longer needed.
#   9. {s} subdomain rotation and {r} retina suffixes -- meaningless for vector
#      tiles, dropped with no loss.
#
# Different but equivalent:
#  10. colorFactor() -> a named colour vector plus a per-layer filter.
#  11. Inline popup formula -> a precomputed popup_html property. mapgl uses
#      setHTML(), so the mailto/tel links and <b> markup render as before.
#  12. Legend title HTML. mapgl inserts legend_title raw into the legend <h2>,
#      so the <br> in title_ndac survives.
#  13. maplibre() defaults to projection = "globe"; set to "mercator" above to
#      match Leaflet's flat Web Mercator view.
#  14. clusterOptions = NULL -> cluster_options() exists in mapgl; unused here,
#      same as before.
