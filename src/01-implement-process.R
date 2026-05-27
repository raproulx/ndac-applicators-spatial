library(tidyverse)
library(rvest)
library(sf)
# library(nanoparquet)
library(tidygeocoder)
library(packcircles)
library(fs)
library(pushoverr)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(htmltools)

conflicted::conflicts_prefer(dplyr::filter)

source("./fun/read_ndac_directory.R")
ndac_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSVKqNEfTonjBJmfEtT6c2md4W0jXvJZ6vQPVEBBSIAOEAXeKvhw5T_pMJC1jNXK6hxZcpAzxeeGvpp/pubhtml"

# create helper functions -------------------------------------------------
# from Claude Sonnet 4.6
spiral_jitter <- function(pts, radius, epsg, rings = 3) {
  original_crs <- st_crs(pts)

  # Store original coordinates as columns before transforming
  orig_coords <- st_coordinates(pts)
  pts$orig_x <- orig_coords[, "X"]
  pts$orig_y <- orig_coords[, "Y"]

  # Project to working CRS
  pts_proj <- st_transform(pts, epsg)
  coords <- st_coordinates(pts_proj)

  df <- cbind(st_drop_geometry(pts_proj), as.data.frame(coords))
  golden_angle <- pi * (3 - sqrt(5))

  df <- df %>%
    mutate(gid = paste(round(X, 6), round(Y, 6), sep = "_")) %>%
    group_by(gid) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    mutate(
      r = ifelse(rank == 1, 0, radius * sqrt(rank - 1) / sqrt(rings * 6)),
      theta = (rank - 1) * golden_angle,
      X = X + r * cos(theta),
      Y = Y + r * sin(theta)
    ) %>%
    select(-gid, -rank, -r, -theta)

  result_proj <- st_as_sf(df, coords = c("X", "Y"), crs = epsg)

  # Transform back to original CRS
  st_transform(result_proj, original_crs)
}

# from Claude Sonnet 4.6
pack_points <- function(pts, min_dist, epsg, max_iter = 500, seed = 42) {
  original_crs <- st_crs(pts)

  # Store original coordinates as columns before transforming
  orig_coords <- st_coordinates(pts)
  pts$orig_x <- orig_coords[, "X"]
  pts$orig_y <- orig_coords[, "Y"]

  # Project to working CRS
  pts_proj <- st_transform(pts, epsg)
  coords <- st_coordinates(pts_proj)
  radius <- min_dist / 2

  # Pre-jitter coordinates by a fraction of the radius to seed 2D repulsion
  set.seed(seed)
  coords_jittered <- coords
  coords_jittered[, "X"] <- coords[, "X"] +
    runif(nrow(coords), -radius * 0.1, radius * 0.1)
  coords_jittered[, "Y"] <- coords[, "Y"] +
    runif(nrow(coords), -radius * 0.1, radius * 0.1)

  layout <- circleRepelLayout(
    cbind(coords_jittered, rep(radius, nrow(coords_jittered))),
    xysizecols = 1:3,
    sizetype = "radius",
    maxiter = max_iter,
    wrap = FALSE
  )

  new_coords <- layout$layout[, c("x", "y")]

  # Rebuild sf object with updated coordinates, keeping all columns
  result_proj <- st_as_sf(
    cbind(st_drop_geometry(pts_proj), new_coords),
    coords = c("x", "y"),
    crs = epsg
  )

  # Transform back to original CRS
  st_transform(result_proj, original_crs)
}

# from Claude Sonnet 4.6
add_google_analytics <- function(html_file, measurement_id) {
  ga_script <- sprintf(
    '
<!-- Google tag (gtag.js) -->
<script async src="https://www.googletagmanager.com/gtag/js?id=%s"></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag("js", new Date());
  gtag("config", "%s");
</script>',
    measurement_id,
    measurement_id
  )

  html_content <- readLines(html_file, warn = FALSE)
  html_text <- paste(html_content, collapse = "\n")

  modified_html <- sub(
    pattern = "(<head[^>]*>)",
    replacement = paste0("\\1", ga_script),
    x = html_text
  )

  writeLines(modified_html, html_file)
  message("Google Analytics tag inserted into: ", html_file)
}

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
dat_geo_saved <- if (
  file_exists("results/ndac-directory-georeferenced.geojson")
) {
  read_sf("results/ndac-directory-georeferenced.geojson")
} else (NULL)

# geocode new directory entries and alert to any errors -------------------
dat_new <- if (is.null(dat_geo_saved)) {
  dat_ndac
} else {
  dat_ndac |>
    anti_join(
      dat_geo_saved |>
        select(`BUSINESS NAME`, `OWNER/OPERATOR`)
    )
}

if (nrow(dat_new) > 0) {
  dat_geo_new <- dat_new |>
    geocode(city = CITY, state = STATE, method = "osm")

  dat_geo_errors <- dat_geo_new |>
    filter(is.na(lat) | is.na(long))

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
} else {
  dat_geo_new <- NULL
}

# rewrite all geocoded entries to geojson -------------------------------
rbind(
  if (!is.null(dat_geo_saved)) {
    dat_geo_saved |>
      inner_join(
        dat_ndac |>
          select(`BUSINESS NAME`, `OWNER/OPERATOR`)
      )
  },
  dat_geo_new
) |>
  arrange(`BUSINESS NAME`) |>
  filter_out(is.na(lat) | is.na(long)) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
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
  ) |>
  pack_points(min_dist = 3500, epsg = 3857)
# st_jitter(factor = 0.004)
# spiral_jitter(radius = 3500, epsg = 3857, rings = 1)

pal <- colorFactor(c("#658849", "#34499B"), domain = c("Manned", "Unmanned"))

m <- leaflet(
  data = dat_leaflet,
  width = "100%",
  height = "100vh",
  options = leafletOptions(
    minZoom = 5,
    maxZoom = 12
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

# m

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


# append Google Analytics to HTML -----------------------------------------
add_google_analytics(
  "index.html",
  measurement_id = Sys.getenv("NDACMAP_GA_MEASUREMENT_ID")
)
