require(readr)
require(fs)
require(stringr)
require(dplyr)

read_ndac_directory <- function(
  input = NULL
) {
  if (is.null(input)) {
    stop("Must provide Google Sheets URL or path to Excel file")
  }

  if (path_ext(input) %in% c("xls", "xlsx")) {
    dat <- read_excel(
      input,
      col_names = c(
        "BUSINESS NAME",
        "OWNER/OPERATOR",
        "EMAIL",
        "PHONE",
        "CITY",
        "STATE",
        "CHIEF PILOT",
        "ADDL PILOTS",
        "TYPE OF LICENSE"
      ),
      skip = 3
    )
  }

  if (path_ext(input) == "") {
    url <- str_c(
      str_replace(input, "/pubhtml", "/pub"),
      "?output=csv"
    )

    dat <- read_csv(I(read_file(url))) |>
      slice(-1) |>
      mutate(across(
        c(`BUSINESS NAME`, `ADDL PILOTS`),
        ~ str_replace_all(.x, "\n", ", ")
      )) |>
      rename_with(
        ~ replace_values(
          .x,
          "CITY                    STATE" ~ "CITY",
          "...6" ~ "STATE"
        )
      )
  }

  dat
}
