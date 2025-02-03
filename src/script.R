source("./src/functions.R")

# let's work on something we know a bit
this_df <- "DF_POP_LECZ"

# next goal is to retrieve all the necessary metadata to produce
# a good table

# dataflow info requires only the DF name (we always care about the latest version)
df_art <- readSDMX("https://stats-sdmx-disseminate.pacificdata.org/rest/dataflow/SPC/df_pop_age/latest?references=all")

# info is stored into the component of the associated DSD
# we'll need to pack this into a good function
dsd_art <- df_art@datastructures@datastructures[[1]]
dimensions <- dsd_art@Components@Dimensions
time_dimension <- dsd_art@Components@TimeDimension
primary_measure <- dsd_art@Components@PrimaryMeasure
attributes <- dsd_art@Components@Attributes


data_url <- "SPC,DF_POP_LECZ,1.0/A.AS..?dimensionAtObservation=AllDimensions"

geo <- "AS"
indicator <- "this_indicator"
dimensions <-  c("TIME_PERIOD", "FREQ", "GEO_PICT", "INDICATOR", "ELEVATION")