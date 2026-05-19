library(tidyverse)
library(rvest)
library(sf)
library(nanoparquet)
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
  str_replace(" :", "<br>") |>
  str_c(
    "<br><span style=font-size: 10px;>*locations are approximate to maintain privacy*</span>"
  )

# read existing georeferenced directory -----------------------------------
dat_geo_saved <- read_parquet("results/ndac-directory-georeferenced.parquet")

# geocode new directory entries -------------------------------------------
dat_new <- dat_ndac |>
  anti_join(dat_geo_saved)

dat_geo_new <- dat_new |>
  geocode(city = CITY, state = STATE, method = "osm")

# identify and alert georeferencing errors --------------------------------
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

# write all geocoded entries to parquet ----------------------------------
write_parquet(
  dat_geo_saved |>
    right_join(dat_ndac) |>
    bind_rows(
      dat_geo_new |>
        filter_out(is.na(lat) | is.na(long))
    ) |>
    arrange(`BUSINESS NAME`),
  "results/ndac-directory-georeferenced.parquet"
)


# create Leaflet map ------------------------------------------------------
dat_leaflet <- read_parquet("results/ndac-directory-georeferenced.parquet") |>
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

ndac_entries <- dat_leaflet |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  st_jitter(amount = 0.05)

pal <- colorFactor(c("#658849", "#34499B"), domain = c("Manned", "Unmanned"))

tag.map.title <- tags$style(HTML(
  "
  .leaflet-control.map-title { 
    transform: translate(-50%,0%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px;
    color: #555;
    background: rgba(255,255,255,0.75);
    font-family: Helvetica Neue, Arial, Helvetica, sans-serif;
    font-weight: bold;
    font-size: 14px;
  }
"
))

title <- tags$div(
  tag.map.title,
  HTML(title_ndac)
)

m <- leaflet(
  data = ndac_entries,
  options = leafletOptions(
    minZoom = 5,
    maxZoom = 10
  )
) |>
  addTiles() |>
  addCircleMarkers(
    data = ndac_entries,
    group = ndac_entries$`TYPE OF LICENSE`,
    color = ~ pal(ndac_entries$`TYPE OF LICENSE`),
    # fmt: skip
    popup = str_c(
      "<b>", ndac_entries$`BUSINESS NAME`, "</b><br><br>",
      "<b>OWNER/OPERATOR: </b>", ndac_entries$`OWNER/OPERATOR`, "<br>",
      "<b>EMAIL: </b><a href='mailto:", ndac_entries$`EMAIL`, "'>", ndac_entries$`EMAIL`, "</a><br>",
      "<b>PHONE: </b><a href='tel:", ndac_entries$`PHONE`, "'>", ndac_entries$`PHONE`, "</a><br>",
      "<b>CITY/STATE: </b>", ndac_entries$`CITY`, ", ", ndac_entries$`STATE`, "<br>",
      "<b>CHIEF PILOT: </b>", ndac_entries$`CHIEF PILOT (RESPONSIBLE FOR ALL PILOTS)`, "<br>",
      "<b>ADDL PILOTS: </b>", ndac_entries$`ADDL PILOTS`, "<br>",
      "<b>TYPE OF LICENSE: </b>", ndac_entries$`TYPE OF LICENSE`, "<br>"
    ),
    clusterOptions = NULL
  ) |>
  addLayersControl(
    overlayGroups = c("Manned", "Unmanned"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addSearchOSM(searchOptions(
    marker = list(
      icon = NULL,
      animate = TRUE,
      circle = list(
        radius = 10,
        weight = 3,
        color = "#e03",
        stroke = FALSE,
        fill = FALSE
      )
    )
  ))
  ) |>
  addControl(title, position = "topright", className = "map-title")
m

# write Leaflet map to HTML -----------------------------------------------
saveWidget(m, file = "index.html")
