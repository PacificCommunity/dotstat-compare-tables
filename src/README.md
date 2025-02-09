# Code organization

Functions are in `functions.R`, and they depend on the `tidyverse` framework plus a bit of other libraries (`arsenal` does some job in creating diffs table, but we might want to remove it; `kblExtra` knits the table into `.tex` files; `rsdmx` and `xml2` extract data and sdmx artefacts)

The micro vignette in `script.R` offers an example of usage, and produce ***a lot*** of tables in `/output/temp`.
