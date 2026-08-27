read_ndac_directory <- function(
  input = NULL
) {
  require(readr)
  require(fs)
  require(stringr)
  require(dplyr)
  require(tidyr)
  require(readxl)
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

    dat <- read_csv(
      I(read_file(url)),
      name_repair = ~ vctrs::vec_as_names(
        str_squish(str_replace_all(.x, "[\r\n]+", " ")),
        repair = "unique",
        quiet = TRUE
      )
    ) |>
      mutate(
        .rest_is_empty = rowSums(!is.na(across(-1)) & across(-1, ~ .x != "")) ==
          0,
        `TYPE OF LICENSE` = if_else(
          .rest_is_empty,
          str_extract(`BUSINESS NAME`, "^(UN)?MANNED"),
          NA_character_
        )
      ) |>
      fill(`TYPE OF LICENSE`, .direction = "down") |>
      dplyr::filter(!.rest_is_empty) |>
      mutate(across(
        c(`BUSINESS NAME`, `ADDL PILOTS`),
        ~ str_replace_all(.x, "\n", ", ") |> str_squish()
      )) |>
      rename_with(
        ~ replace_values(
          .x,
          "CITY STATE" ~ "CITY",
          "...6" ~ "STATE"
        )
      ) |>
      select(-.rest_is_empty)
  }

  dat
}
