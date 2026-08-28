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

# Index for the search control. mapgl has no attribute-search control, so the
# search runs client-side over this rather than poking at MapLibre internals.
# Coordinates come from dat_mapgl, so they are the same jittered points plotted.
coords_mapgl <- st_coordinates(dat_mapgl)

search_index <- jsonlite::toJSON(
  tibble(
    name = dat_mapgl$`BUSINESS NAME`,
    owner = dat_mapgl$`OWNER/OPERATOR`,
    city = dat_mapgl$CITY,
    state = dat_mapgl$STATE,
    type = dat_mapgl$`TYPE OF LICENSE`,
    lon = coords_mapgl[, "X"],
    lat = coords_mapgl[, "Y"],
    popup = dat_mapgl$popup_html
  ),
  auto_unbox = TRUE
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
  # MapLibre collapses the attribution to an (i) button on narrow maps;
  # compact = FALSE keeps the full credit line visible and drops the toggle
  attributionControl = list(compact = FALSE),
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
    # Clicking a patch toggles that category, replacing the separate layers
    # control. mapgl composes this filter with each layer's own filter rather
    # than replacing it, so the TYPE OF LICENSE filters below still apply.
    interactive = TRUE,
    layer_id = c("Manned", "Unmanned"),
    filter_column = "TYPE OF LICENSE",
    filter_values = c("Manned", "Unmanned"),
    # mapgl's default legend background is 50% white
    style = legend_style(background_color = "#ffffff", background_opacity = 1),
    margin_top = 10,
    margin_right = 10
  ) |>
  add_control(
    # fmt: skip
    html = str_c(
      "<div class='ndac-search'>",
      "<input type='text' class='ndac-search-input' ",
      "placeholder='Search company, city, address...' ",
      "autocomplete='off' aria-label='Search applicators or addresses'>",
      "<div class='ndac-search-results' hidden></div>",
      "</div>"
    ),
    position = "top-left",
    className = "maplibregl-ctrl maplibregl-ctrl-group",
    id = "search"
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
# setStyle() discards user-added sources and layers, but mapgl re-adds them
# itself on every style.load. It also reseeds each layer's base filter from the
# widget payload, which drops whatever the interactive legend had filtered, so
# recompose base + legend once the layers are back.
js_basemap_switch <- sprintf(
  "
  function (el, x) {
    var STYLES = { standard: '%s', light: '%s' };
    var IDS = ['Manned', 'Unmanned'];

    function swap(url) {
      var map = window._ndacMap;
      if (!map) return;

      map.setStyle(url);
      map.once('style.load', function () {
        var tries = 0;
        var iv = setInterval(function () {
          if (IDS.every(function (id) { return map.getLayer(id); })) {
            clearInterval(iv);
            IDS.forEach(function (id) {
              if (typeof window._mapglComposeFilter === 'function') {
                window._mapglComposeFilter(map, id);
              }
            });
          } else if (++tries > 40) {
            clearInterval(iv);
          }
        }, 25);
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

# hybrid search -----------------------------------------------------------
# Applicator matches (BUSINESS NAME / CITY / OWNER-OPERATOR) come from the
# embedded search_index; below them, Nominatim supplies US addresses, which is
# what add_geocoder_control() could not be restricted to.
js_search <- "
  function (el, x) {
    var IDX = null;
    var timer = null;
    var seq = 0;
    var apps = [];
    var places = [];
    var searchPopup = null;

    function index() {
      if (IDX === null) {
        var node = document.getElementById('ndac-search-index');
        try {
          IDX = node ? JSON.parse(node.textContent) : [];
        } catch (err) {
          IDX = [];
        }
      }
      return IDX;
    }

    function box() {
      return el.querySelector('.ndac-search-results');
    }

    function hide() {
      // Bump the sequence too: a geocode already in flight would otherwise
      // render its results and re-open the box the user just dismissed.
      seq++;
      var b = box();
      if (b) b.hidden = true;
    }

    // Nominatim labels are third-party text, so escape everything rendered.
    function esc(s) {
      return String(s == null ? '' : s)
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/\"/g, '&quot;');
    }

    function match(q) {
      var needle = q.toLowerCase();
      var hits = [];
      index().forEach(function (r, i) {
        var name = (r.name || '').toLowerCase();
        var city = (r.city || '').toLowerCase();
        var owner = (r.owner || '').toLowerCase();
        var rank = -1;
        if (name.indexOf(needle) === 0) rank = 0;
        else if (name.indexOf(needle) > 0) rank = 1;
        else if (city.indexOf(needle) === 0) rank = 2;
        else if (city.indexOf(needle) > 0) rank = 3;
        else if (owner.indexOf(needle) > -1) rank = 4;
        if (rank > -1) hits.push({ i: i, rank: rank, name: name });
      });
      hits.sort(function (a, b) {
        return a.rank - b.rank ||
          (a.name < b.name ? -1 : a.name > b.name ? 1 : 0);
      });
      return hits.slice(0, 8).map(function (h) { return h.i; });
    }

    function render() {
      var b = box();
      if (!b) return;
      var html = '';
      if (apps.length) {
        html += '<div class=\"ndac-search-group\">Applicators</div>';
        apps.forEach(function (i) {
          var r = index()[i];
          html += '<div class=\"ndac-search-row\" data-kind=\"app\" data-i=\"' +
            i + '\"><div class=\"ndac-search-name\">' + esc(r.name) +
            '</div><div class=\"ndac-search-meta\">' + esc(r.type) +
            ' \\u00b7 ' + esc(r.city) + ', ' + esc(r.state) + '</div></div>';
        });
      }
      if (places.length) {
        html += '<div class=\"ndac-search-group\">Places</div>';
        places.forEach(function (p) {
          html += '<div class=\"ndac-search-row\" data-kind=\"place\" data-lon=\"' +
            p.lon + '\" data-lat=\"' + p.lat + '\">' +
            '<div class=\"ndac-search-name\">' + esc(p.label) + '</div></div>';
        });
      }
      if (!html) html = '<div class=\"ndac-search-empty\">No matches</div>';
      b.innerHTML = html;
      b.hidden = false;
    }

    function geocode(q, mySeq) {
      var url = 'https://nominatim.openstreetmap.org/search?format=jsonv2' +
        '&countrycodes=us&limit=3&q=' + encodeURIComponent(q);
      fetch(url)
        .then(function (r) { return r.ok ? r.json() : []; })
        .then(function (data) {
          // Ignore a slow response for a query the user has moved on from.
          if (mySeq !== seq) return;
          places = (data || []).map(function (d) {
            return { label: d.display_name, lon: +d.lon, lat: +d.lat };
          });
          render();
        })
        .catch(function () {});
    }

    function select(row) {
      var map = window._ndacMap;
      if (!map || !row) return;
      var lon, lat, popupHtml = null;
      if (row.getAttribute('data-kind') === 'app') {
        var r = index()[+row.getAttribute('data-i')];
        if (!r) return;
        lon = r.lon;
        lat = r.lat;
        popupHtml = r.popup;
      } else {
        lon = +row.getAttribute('data-lon');
        lat = +row.getAttribute('data-lat');
      }
      map.flyTo({ center: [lon, lat], zoom: 11 });
      if (searchPopup) {
        searchPopup.remove();
        searchPopup = null;
      }
      if (popupHtml) {
        searchPopup = new maplibregl.Popup()
          .setLngLat([lon, lat])
          .setHTML(popupHtml)
          .addTo(map);
      }
      hide();
    }

    // Delegated: mapgl adds its controls on map load, after onRender runs, so
    // the input does not exist yet at this point.
    el.addEventListener('input', function (e) {
      if (!e.target.classList.contains('ndac-search-input')) return;
      var q = e.target.value.trim();
      seq++;
      var mySeq = seq;
      places = [];
      clearTimeout(timer);
      if (q.length < 2) {
        apps = [];
        hide();
        return;
      }
      timer = setTimeout(function () {
        apps = match(q);
        render();
        if (q.length >= 3) geocode(q, mySeq);
      }, 250);
    });

    el.addEventListener('keydown', function (e) {
      if (!e.target.classList.contains('ndac-search-input')) return;
      var b = box();
      if (!b) return;
      if (e.key === 'Escape') {
        e.target.value = '';
        apps = [];
        places = [];
        hide();
        return;
      }
      var rows = b.querySelectorAll('.ndac-search-row');
      if (b.hidden || !rows.length) return;
      var cur = b.querySelector('.ndac-search-row.is-active');
      var i = Array.prototype.indexOf.call(rows, cur);
      if (e.key === 'ArrowDown') {
        e.preventDefault();
        i = (i + 1) % rows.length;
      } else if (e.key === 'ArrowUp') {
        e.preventDefault();
        i = (i <= 0 ? rows.length : i) - 1;
      } else if (e.key === 'Enter') {
        e.preventDefault();
        select(cur || rows[0]);
        return;
      } else {
        return;
      }
      if (cur) cur.classList.remove('is-active');
      rows[i].classList.add('is-active');
      rows[i].scrollIntoView({ block: 'nearest' });
    });

    // mousedown, not click: preventDefault keeps focus in the input so the
    // row is still there when the selection runs.
    el.addEventListener('mousedown', function (e) {
      var row = e.target.closest && e.target.closest('.ndac-search-row');
      if (row) {
        e.preventDefault();
        select(row);
      }
    });

    el.addEventListener('focusin', function (e) {
      if (e.target.classList.contains('ndac-search-input') &&
          (apps.length || places.length)) {
        render();
      }
    });

    // Every .maplibregl-ctrl carries transform: translate(0), so each is its
    // own stacking context and they paint in DOM order. The search is added
    // first, so lift its wrapper above the fullscreen and zoom controls or the
    // results panel renders underneath them.
    var lifts = 0;
    var liftIv = setInterval(function () {
      var node = el.querySelector('.ndac-search');
      var ctrl = node && node.closest('.maplibregl-ctrl');
      if (ctrl) {
        clearInterval(liftIv);
        ctrl.style.position = 'relative';
        ctrl.style.zIndex = '5';
      } else if (++lifts > 100) {
        clearInterval(liftIv);
      }
    }, 50);
  }
"

# write mapgl map to HTML -------------------------------------------------
saveWidget(
  prependContent(
    m,
    tags$script(js_capture_map),
    tags$script(
      type = "application/json",
      id = "ndac-search-index",
      HTML(search_index)
    ),
    tags$style(HTML(
      "
      /* Keep the 100vh widget inside the viewport so no scrollbar appears and
         the attribution stays visible at the bottom. Two things push it down:
         saveWidget's plain <body> (default 8px margin), and mapgl's
         layers-control stylesheet, which it emits wrapped in a <p>. A <p> is
         illegal in <head>, so the parser relocates it to the top of the body
         ahead of the widget, where its 16px margin shifts everything down --
         hence taking the widget out of flow rather than just zeroing margins. */
      html, body {
        margin: 0;
        padding: 0;
        overflow: hidden;
      }
      #htmlwidget_container {
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
      }

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
         corner; the legend positions itself). A starting value only -- the
         real offset is measured from the legend at runtime, because the
         interactive legend grows when its Reset button appears. */
      .maplibregl-ctrl-top-right { margin-top: 140px; }

      .ndac-basemap {
        padding: 6px 10px;
        font-family: Helvetica Neue, Arial, Helvetica, sans-serif;
        font-size: 12px;
        line-height: 1.6;
      }
      .ndac-basemap-title { font-weight: bold; }
      .ndac-basemap label { display: block; font-weight: normal; }

      /* Belt and braces for attributionControl = list(compact = FALSE): if a
         future MapLibre collapses it anyway, keep the credit line showing and
         the toggle button gone. */
      .maplibregl-ctrl-attrib.maplibregl-compact
        .maplibregl-ctrl-attrib-inner { display: block; }
      .maplibregl-ctrl-attrib-button { display: none !important; }
      .maplibregl-ctrl-attrib.maplibregl-compact {
        padding: 2px 8px;
        min-height: 0;
      }
      .maplibregl-ctrl-attrib.maplibregl-compact:after { display: none; }

      .ndac-search {
        position: relative;
        overflow: visible;
        /* paint above the zoom and fullscreen controls below it */
        z-index: 4;
        font-family: Helvetica Neue, Arial, Helvetica, sans-serif;
      }
      .ndac-search-input {
        width: 260px;
        max-width: 60vw;
        box-sizing: border-box;
        border: 0;
        border-radius: 4px;
        padding: 8px 10px;
        font-family: inherit;
        font-size: 13px;
        outline: none;
      }
      .ndac-search-results {
        position: absolute;
        top: calc(100% + 4px);
        left: 0;
        width: 100%;
        max-height: 320px;
        overflow-y: auto;
        background: #fff;
        border-radius: 4px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.3);
        z-index: 3;
      }
      .ndac-search-group {
        font-size: 10px;
        font-weight: bold;
        letter-spacing: 0.06em;
        text-transform: uppercase;
        color: #888;
        padding: 6px 10px 2px;
      }
      .ndac-search-row {
        padding: 5px 10px;
        cursor: pointer;
        border-top: 1px solid #eee;
      }
      .ndac-search-group + .ndac-search-row { border-top: 0; }
      .ndac-search-row:hover,
      .ndac-search-row.is-active { background: #f0f2f5; }
      .ndac-search-name {
        font-size: 13px;
        font-weight: bold;
        color: #222;
      }
      .ndac-search-meta {
        font-size: 11px;
        color: #666;
      }
      .ndac-search-empty {
        padding: 8px 10px;
        font-size: 12px;
        color: #888;
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

        /* 16px keeps iOS from zooming the page when the input takes focus */
        .ndac-search-input {
          width: 70vw;
          font-size: 16px;
        }
        .ndac-search-name { font-size: 15px; }
        .ndac-search-meta { font-size: 13px; }
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
    onRender(js_search) |>
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

    // The legend and the top-right control corner are positioned
    // independently, so keep the basemap switcher below the legend. Measured
    // rather than hard-coded: the legend grows when its Reset button appears.
    var corner = el.querySelector('.maplibregl-ctrl-top-right');
    var tries = 0;
    var interval = setInterval(function() {
      // mapgl wraps the legend in a second .mapboxgl-legend div of zero
      // height, so take the last match and wait for it to have a height.
      var all = el.querySelectorAll('.mapboxgl-legend');
      var legend = all.length ? all[all.length - 1] : null;
      if (!corner || !legend || !legend.offsetHeight) {
        if (++tries > 100) clearInterval(interval);
        return;
      }
      clearInterval(interval);

      function place() {
        corner.style.marginTop = (legend.offsetHeight + 20) + 'px';
      }
      place();
      if (typeof ResizeObserver === 'function') {
        new ResizeObserver(place).observe(legend);
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
#   3. mapgl adds its controls on map load, after onRender runs, so nothing
#      inside onRender can bind to a control directly. Hence the delegated
#      listeners for the basemap radios and the search input, and the poll for
#      the legend below.
#   4. Stacking the top-right corner. Leaflet stacks its controls in one corner
#      container; mapgl's legend and custom controls are positioned
#      independently and overlap by default, so the basemap switcher's offset
#      is measured off the legend at runtime.
#   5. Searching the applicators. mapgl has no attribute-search control of any
#      kind, so the search box, its typeahead and the Nominatim fallback are
#      hand-written against an embedded JSON index of the plotted features.
#      Selecting a result opens a popup built by hand rather than reusing the
#      layer's own popup handler, which is only reachable by a real click.
#   6. Re-applying the legend filter after a basemap swap. mapgl re-adds the
#      source and layers itself on every style.load and reseeds each layer's
#      base filter from the widget payload, silently dropping the interactive
#      legend's filter, so the swap recomposes it via _mapglComposeFilter.
#
# No equivalent, dropped:
#   7. addFullscreenControl(pseudoFullscreen = FALSE). MapLibre's fullscreen
#      control has no pseudo-fullscreen mode; the open-in-new-tab fallback for
#      touch devices is ported and still applies.
#   8. scaleBarOptions(updateWhenIdle = TRUE). add_scale_control() exposes only
#      position, unit and max_width.
#   9. addSearchOSM()'s styled result marker (radius, weight, color = "#e03",
#      animate). add_geocoder_control() was dropped for the custom control
#      above, which does restore the countrycodes=us restriction the geocoder
#      could not express, but a selected place is only flown to, not marked.
#  10. The CARTO ?key= parameter. It applies to the raster tile URLs, not to
#      the GL style JSON, which is served unauthenticated. Attribution is
#      carried inside the vector style, so the explicit attribution strings are
#      no longer needed.
#  11. {s} subdomain rotation and {r} retina suffixes -- meaningless for vector
#      tiles, dropped with no loss.
#
# Different but equivalent:
#  12. colorFactor() -> a named colour vector plus a per-layer filter.
#  13. Inline popup formula -> a precomputed popup_html property. mapgl uses
#      setHTML(), so the mailto/tel links and <b> markup render as before.
#  14. Legend title HTML. mapgl inserts legend_title raw into the legend <h2>,
#      so the <br> in title_ndac survives.
#  15. maplibre() defaults to projection = "globe"; set to "mercator" above to
#      match Leaflet's flat Web Mercator view.
#  16. clusterOptions = NULL -> cluster_options() exists in mapgl; unused here,
#      same as before.
#  17. The Leaflet overlay layers control -> the interactive legend. mapgl
#      toggles the clicked category off and greys its patch, rather than
#      isolating it the way dcmap.us does; with two categories that reads the
#      same, and a Reset button appears while either is off.
