library(daff)
library(rsdmx)
library(tidyverse)
library(diffdf)
library(arsenal)
library(kableExtra)

# initial idea is
# for each dataflow
# retrieve _all_ indicators
# retrieve _all_ countries
# for each datafow x indicator x country
# check if there were changes
# if there were changes
# produces a patch with daff and print to html


# alternative to consider is going full datatable

prod_base <- "https://stats-sdmx-disseminate.pacificdata.org/rest/data/"
stag_base <- "https://stats-sdmx-disseminate-staging.pacificdata.org/rest/data/"


get_prod_stag_data <- function(data_url, old_base = prod_base, new_base = stag_base) {
  
  old_data <- readSDMX(
      paste0(old_base,data_url)
    ) |>
    as_tibble() |>
    arrange_all()

  new_data <- readSDMX(
      paste0(new_base,data_url)
    ) |>
    as_tibble() |>
    arrange_all()

  return(list(base = old_data, new = new_data))
}

produce_diffs <- function(old_data, new_data, dimensions) {
  this_diffs <- comparedf(
      old_data,new_data,
      by = dimensions
    ) |>
    diffs() |>
    select(-var.x, -var.y, -FREQ, -GEO_PICT, -INDICATOR, -row.x, -row.y)

  return(this_diffs)
}

table_to_tex <- function(out_table, indicator_name, geo_name) {
  out_table |>
  kbl(
    caption = str_glue("{indicator_name} data for {geo_name}"),
    booktabs = T,
    longtable = TRUE,
    format = "latex") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    latex_options = c("repeat_header")) %>%
    save_kable("./output/chunk_test.tex")

} 

