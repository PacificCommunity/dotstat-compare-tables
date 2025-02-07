source("./src/functions.R")

spc_base <- "https://stats-sdmx-disseminate.pacificdata.org/rest"


target_df_ids <- c(
    "DF_NMDI",
    "DF_IMTS",
    "DF_BP50",
    "DF_POP_LECZ",
    "DF_POP_PROJ",
    "DF_NATIONAL_ACCOUNTS"
)

for (this_df_id in target_df_ids) {
  this_components <- this_df_id |>
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
    pwalk(~ safe_get_diffs_chunk_for_df(..1, ..2, this_df_id, version = "latest", dsd_components = this_components))

}

get_diffs_chunk_for_df(geos[7], indicators[2], df_id, version = df_id |> df_latest_version(), dsd_components = this_components )
# https://stats-sdmx-disseminate.pacificdata.org/rest/data/SPC,DF_IMTS,4.0/.AS.NMDI0001.....?dimensionAtObservation=AllDimensions

df_id
