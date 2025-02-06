source("./src/functions.R")

spc_base <- "https://stats-sdmx-disseminate.pacificdata.org/rest"


df_ids <- c(
    "DF_NMDI",
    "DF_IMTS",
    "DF_BP50",
    "DF_POP_LECZ",
    "DF_POP_PROJ",
    "DF_NATIONAL_ACCOUNTS"
)

for (df_id in df_ids) {
  this_components <- df_id |>
    df_to_component_tbl()

  ind_cl <- this_components |>
    filter(conceptRef == "INDICATOR")

  indicators <- cl_to_tbl(ind_cl$codelist, agency = ind_cl$codelistAgency, version = ind_cl$codelistVersion)$id

  if ("GEO_PICT" %in% this_components$conceptRef) {
    geo_cl <- this_components |>
      filter(conceptRef == "GEO_PICT")
        geos <- cl_to_tbl(geo_cl$codelist, agency = geo_cl$codelistAgency, version = geo_cl$codelistVersion)$id
  }

  crossing(geos, indicators) |>
    pwalk(~ safe_get_diffs_chunk_for_df(..1, ..2, df_id, version = "1.0", dsd_components = this_components))

}
