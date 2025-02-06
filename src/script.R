source("./src/functions.R")

spc_base <- "https://stats-sdmx-disseminate.pacificdata.org/rest"

# let's work on something we know a bit
df_id <- "DF_POP_LECZ"

# next goal is to retrieve all the necessary metadata to produce
# a good table

for(df_id in df_ids) {

this_components <- df_id |>
    df_to_component_tbl()

ind_cl <- this_components |>
    filter(conceptRef == "INDICATOR")

indicators <- cl_to_tbl(ind_cl$codelist, agency = ind_cl$codelistAgency, version = ind_cl$codelistVersion)$id

if("GEO_PICT" %in% this_components$conceptRef){
    geo_cl <- this_components |>
        filter(conceptRef == "GEO_PICT")
    geos <- cl_to_tbl(geo_cl$codelist, agency = geo_cl$codelistAgency, version = geo_cl$codelistVersion)$id
}

agency <- version <- df_id <- NULL
crossing(geos,indicators) |>
pwalk(~ 
    safe_get_diffs_chunk_for_df(..1, ..2, df_id, version = "1.0", dsd_components = this_components)
)

}


ppp <- safe_get_diffs_chunk_for_df("WS","LECZPOPAF",df_id, version = "1.0", dsd_components = this_components)
