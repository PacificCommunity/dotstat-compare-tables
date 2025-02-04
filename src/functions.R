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

##

build_sdmx_artefact_url <- function(base_url, object, agency, obj_id, version, references) {
  
  base_url <- str_remove(base_url,"/$")
  
  query_url <- str_glue(
    "{base_url}/{object}/{agency}/{obj_id}/{version}"
  )

  query_url <- if_else(is_empty(references),
    query_url,
    paste0(query_url,"?references=",references)
  )

  return(query_url)
}

# Objects returned by rsdmx are often in S4 classes
# this is an helper function that helps transforming
# an S4 object to a flat tibble
# it often needs to be used in unison with map_df to extract all elements
# from a list of S4 objects (such as a codelist, ...)
S4_to_tibble <- function(s4obj) {
  nms <- slotNames(s4obj)

  lst <- map(nms, \(nm) slot(s4obj, nm)) |>
    setNames(nms) |>
    unlist()

  return( bind_rows(lst) )
}


df_to_components_tbl <- function(df_id,
  base_url = "https://stats-sdmx-disseminate.pacificdata.org/rest",
  agency = "SPC", version = "latest", references = "all",
  all_details = FALSE) {

  
  df_art <- build_sdmx_artefact_url(base_url,"dataflow",agency,df_id,version, references) |>
    readSDMX()

  # info is stored into the component of the associated DSD
  # we'll need to pack this into a good function
  dsd_art <- df_art@datastructures@datastructures[[1]]
  
  dimensions <- dsd_art@Components@Dimensions |>
    map_df(S4_to_tibble)

  time_dimension <- dsd_art@Components@TimeDimension |>
    S4_to_tibble()

  primary_measure <- dsd_art@Components@PrimaryMeasure |>
    S4_to_tibble()

  attributes <- dsd_art@Components@Attributes |>
    map_df(S4_to_tibble)

  components_df <- bind_rows(
    dimensions,
    time_dimension,
    primary_measure,
    attributes,
    .id = "type"
  ) |>
    mutate(
      type = as_factor(type),
      type = fct_recode(type,
        dimension = "1",
        time_dimension = "2",
        measure = "3",
        attribute = "4"
      )
    )
  
  if(!all_details){
    components_df <- components_df |>
      select(
        type,
        conceptRef, conceptVersion,  conceptAgency,
        codelist,   codelistVersion, codelistAgency
      )
  }
}


# this builds a query ONLY for proper dimensions
# time_dimension follows another syntax, and we don't need it yet
# (it gets appended after the query in the other specificatrions TBB)
build_sdmx_query <- function(dimensions, dim_filters) {

  query_string <- ""

  for(dimension in dimensions) {
    if(dimension %in% names(dim_filters)) {
      query_string <- paste0(query_string, dim_filters[dimension], ".")
    } else {
      query_string <- paste0(query_string, ".")
    }
  }

  # we remove the last dot of the query url because dots
  # are used as _internal_ delimiters only (last one is not present)
  query_string <- str_remove(query_string,".$")

  return(query_string)

}


walker_query <- function(geo, ind, df_id, dsd_components, agency = "SPC", version = "latest") {
  this_df_dims <- dsd_components |> filter(
    type %in% c("dimension")
  ) %>%
  pluck("conceptRef")

  this_filters <- c(GEO_PICT = geo, INDICATOR = ind)

  sdmx_query <- build_sdmx_query(this_df_dims, this_filters)
  query_url <- str_glue("{agency},{df_id},{version}/{sdmx_query}?dimensionAtObservation=AllDimensions")
  return(query_url)
}

cl_to_tbl <- function(cl_id,
  base_url = "https://stats-sdmx-disseminate.pacificdata.org/rest",
  agency = "SPC", version = "latest", references = NULL,
  all_details = FALSE) {

  cl_art <- build_sdmx_artefact_url(base_url,"codelist",agency,cl_id,version,references) |>
    readSDMX()
  # info is stored into the component of the associated DSD
  # we'll need to pack this into a good function
  codes <- cl_art@codelists[[1]]@Code |>
  map_df(S4_to_tibble)
}


get_prod_stag_data <- function(data_url, old_base = prod_base, new_base = stag_base) {
  old_data <- readSDMX(
    paste0(old_base, data_url)
  ) |>
    as_tibble() |>
    arrange_all()

  new_data <- readSDMX(
    paste0(new_base, data_url)
  ) |>
    as_tibble() |>
    arrange_all()

  return(list(base = old_data, new = new_data))
}

produce_diffs <- function(old_data, new_data, dimensions) {
  this_diffs <- comparedf(
    old_data, new_data,
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
      format = "latex"
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      latex_options = c("repeat_header")
    ) %>%
    save_kable(str_glue("./output/{indicator_name}_{geo_name}.tex"))
}

get_diffs_chunk_for_df <- function(data_url, old_base, new_base) {

  data_to_diff <- get_prod_stag_data(data_url, old_base, new_base)
  this_diffs <- produce_diffs
  
  chunk_name <- 

  table_to_tex

}