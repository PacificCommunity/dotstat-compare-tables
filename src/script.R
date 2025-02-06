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


ppp <- get_diffs_chunk_for_df("WS","LECZPOPAF",df_id, version = "1.0", dsd_components = this_components)

t_dimensions <- this_components |>
  filter(type %in% c("dimension", "time_dimension")) |>
  pluck("conceptRef")

data_d$new
data_d$base
ppp

data_d$new |>
    anti_join(data_d$base, by = t_dimensions) |>
    remove_boring_columns(c("FREQ","GEO_PICT","INDICATOR")) |>
    rename(new_value = obsValue) |>
    mutate(old_value = NA) |>
    select(any_of(names(ppp)))

data_d$base |>
    anti_join(data_d$new, by = t_dimensions) |>
    remove_boring_columns(c("FREQ","GEO_PICT","INDICATOR")) |>
    rename(old_value = obsValue) |>
    mutate(new_value = NA) |>
    select(any_of(names(ppp)))



glimpse(ppp)
