source("./src/functions.R")

spc_base <- "https://stats-sdmx-disseminate.pacificdata.org/rest"

# let's work on something we know a bit
this_df <- "DF_POP_LECZ"

# next goal is to retrieve all the necessary metadata to produce
# a good table

this_components <- this_df |>
    df_to_dsd_components()


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
pwalk(~ print(
    walker_query(..1, ..2, "DF_POP_LECZ", this_components)
))

query <- "https://stats-sdmx-disseminate.pacificdata.org/rest/codelist/SPC/CL_BOP_INDICATORS/latest"
data_url <- 
cl <- readSDMX(query)

geo <- "AS"
indicator <- "this_indicator"
dimensions <-  c("TIME_PERIOD", "FREQ", "GEO_PICT", "INDICATOR", "ELEVATION")