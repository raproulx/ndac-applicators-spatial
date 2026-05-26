library(tidyverse)
library(rvest)
library(sf)
# library(nanoparquet)
library(tidygeocoder)
library(fs)
library(pushoverr)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(htmltools)

conflicted::conflicts_prefer(dplyr::filter)

source("./fun/read_ndac_directory.R")
ndac_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSVKqNEfTonjBJmfEtT6c2md4W0jXvJZ6vQPVEBBSIAOEAXeKvhw5T_pMJC1jNXK6hxZcpAzxeeGvpp/pubhtml"

# read NDAC tabular directory ---------------------------------------------
dat_ndac <- read_ndac_directory(
  input = ndac_url
)

title_ndac <- str_c(
  "ND ",
  read_html(ndac_url) |>
    html_element(xpath = "body/div[1]/div/span") |>
    html_text()
) %>%
  str_replace(
    "\\d{8}$",
    str_extract(string = ., pattern = "\\d{8}$") |>
      mdy() |>
      format("%B %d, %Y")
  ) |>
  str_replace(" : ", "<br>")

# read existing georeferenced directory -----------------------------------
dat_geo_saved <- read_sf("results/ndac-directory-georeferenced.geojson")

# geocode new directory entries and alert to any errors -------------------
dat_new <- dat_ndac |>
  anti_join(
    dat_geo_saved |>
      select(`BUSINESS NAME`, `OWNER/OPERATOR`)
  )

if (nrow(dat_new) > 0) {
  dat_geo_new <- dat_new |>
    geocode(city = CITY, state = STATE, method = "osm")

  dat_geo_errors <- dat_geo_new |>
    filter(is.na(lat) | is.na(long))

  dat_geo_new_jittered <- dat_geo_new |>
    filter_out(is.na(lat) | is.na(long)) |>
    st_as_sf(coords = c("long", "lat"), crs = 4326) |>
    st_jitter(factor = 0.004)

  if (nrow(dat_geo_errors) > 0) {
    con <- textConnection("msg", open = "w")

    dat_geo_new |>
      filter(is.na(lat) | is.na(long)) |>
      select(`BUSINESS NAME`, CITY, STATE) |>
      write.csv(con, row.names = FALSE)

    close(con)

    pushover(
      message = str_c(msg, collapse = "\n"),
      title = "NDAC Directory geocoding error!"
    )
  }
}

# rewrite all geocoded entries to geojson -------------------------------
rbind(
  dat_geo_saved |>
    inner_join(
      dat_ndac |>
        select(`BUSINESS NAME`, `OWNER/OPERATOR`)
    ),
  dat_geo_new_jittered
) |>
  arrange(`BUSINESS NAME`) |>
  write_sf(
    "results/ndac-directory-georeferenced.geojson",
    delete_dsn = TRUE
  )

# create Leaflet map ------------------------------------------------------
dat_leaflet <- read_sf("results/ndac-directory-georeferenced.geojson") |>
  arrange(`BUSINESS NAME`) |>
  filter_out(`TYPE OF LICENSE` == "PRIVATE ONLY") |>
  mutate(across(
    c(
      `OWNER/OPERATOR`,
      CITY,
      `CHIEF PILOT (RESPONSIBLE FOR ALL PILOTS)`,
      `ADDL PILOTS`,
      `TYPE OF LICENSE`
    ),
    ~ str_to_title(.x)
  )) |>
  mutate(
    `ADDL PILOTS` = if_else(
      `ADDL PILOTS` == "None Listed",
      "N/A",
      `ADDL PILOTS`
    )
  )

pal <- colorFactor(c("#658849", "#34499B"), domain = c("Manned", "Unmanned"))

m <- leaflet(
  data = dat_leaflet,
  width = "100%",
  height = "100vh",
  options = leafletOptions(
    minZoom = 5,
    maxZoom = 11
  )
) |>
  addTiles(
    urlTemplate = "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}{r}.png",
    attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>',
    group = "Standard"
  ) |>
  addProviderTiles(
    providers$CartoDB.Positron,
    group = "Light"
  ) |>
  addCircleMarkers(
    data = dat_leaflet,
    group = ~`TYPE OF LICENSE`,
    color = ~ pal(`TYPE OF LICENSE`),
    # fmt: skip
    popup = ~ str_c(
      "<b>", `BUSINESS NAME`, "</b><br><br>",
      "<b>OWNER/OPERATOR: </b>", `OWNER/OPERATOR`, "<br>",
      "<b>EMAIL: </b><a href='mailto:", `EMAIL`, "'>", `EMAIL`, "</a><br>",
      "<b>PHONE: </b><a href='tel:", `PHONE`, "'>", `PHONE`, "</a><br>",
      "<b>CITY/STATE: </b>", `CITY`, ", ", `STATE`, "<br>",
      "<b>CHIEF PILOT: </b>", `CHIEF PILOT (RESPONSIBLE FOR ALL PILOTS)`, "<br>",
      "<b>ADDL PILOTS: </b>", `ADDL PILOTS`, "<br>",
      "<b>TYPE OF LICENSE: </b>", `TYPE OF LICENSE`, "<br>"
    ),
    clusterOptions = NULL
  ) |>
  addLegend(
    position = "topright",
    pal = pal,
    values = ~`TYPE OF LICENSE`,
    title = title_ndac
  ) |>
  addLayersControl(
    baseGroups = c("Standard", "Light"),
    overlayGroups = c("Manned", "Unmanned"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addSearchOSM(searchOptions(
    marker = list(
      icon = NULL,
      animate = TRUE,
      circle = list(
        radius = 5,
        weight = 5,
        color = "#e03",
        stroke = TRUE,
        fill = TRUE
      )
    ),
    textPlaceholder = "Address Search...",
  )) |>
  addScaleBar(
    position = "bottomleft",
    options = scaleBarOptions(
      maxWidth = 100,
      imperial = TRUE,
      updateWhenIdle = TRUE
    )
  )

m

# write Leaflet map to HTML -----------------------------------------------
saveWidget(
  prependContent(
    m,
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
      
      .leaflet-popup-content {
        font-size: 14px;
        line-height: 1.5;
        min-width: 200px;
     }
      .leaflet-popup-content b { font-size: 15px; }
      .leaflet-popup-content a { font-size: 14px; }

      @media (max-width: 768px) {
        .map-caption {
          font-size: 13px;
          bottom: 24px;
        }
        
        .leaflet-popup-content {
          font-size: 16px;
          line-height: 1.6;
          min-width: 220px;
        }
        .leaflet-popup-content b { font-size: 17px; }
        .leaflet-popup-content a { font-size: 16px; }
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
    onRender(
      "
    function(el, x) {
      // Prepend a title to the base layers section
      $('.leaflet-control-layers-base').prepend('<label style=\"text-align:left; font-weight:bold\">Basemap</label>');
      // Prepend a title to the overlay layers section
      $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:left; font-weight:bold\">Applicators</label>');
    }
  "
    ),
  file = "index.html"
)
