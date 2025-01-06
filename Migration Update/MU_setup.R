# setup.R
packs <- c("tidyverse","httr",
      "ggthemes", "ggrepel", "patchwork", "grid","ggdist",
      "ggalluvial", "janitor", "gt", "pdftools", "hacksaw", "styler",
      "here","rnaturalearth", "treemapify", "see", "packcircles",
      "geomtextpath", "scales","gghighlight",
      "lubridate", "Rfast", "flextable", "gtsummary",
      "readxl", "stringi", "ggpp",
      "sparkline", "patchwork", "ggtext", "ggalt", "sf",
      "countrycode", "svglite","ggstream",
      "jimjam-slam/ggflags",
      "jthomasmock/gtExtras",
      "hrbrmstr/ggchicklet",
      "glitr")

install_and_load <- function(pkgs) {
   # pkgs can be a character vector of packages
   pak::pkg_install(pkgs)
   
   # Then load each one
   for (pkg in pkgs) {
      pkg_name <- sub(".*/", "", pkg)
      library(pkg_name, character.only = TRUE)
   }
}

install_and_load(packs)
# Load custom themes and functions
source(here("utils","usaidtheme.R"))

# Change some functions 
filter <- dplyr::filter
select <- dplyr::select
`%$%` <- magrittr::`%$%`


swb_lac <- c(
   'BRAZIL',
   'COLOMBIA',
   'CUBA',
   'ECUADOR',
   'EL SALVADOR',
   'GUATEMALA',
   'HAITI',
   'HONDURAS',
   'MEXICO',
   'NICARAGUA',
   'OTHER',
   'PERU',
   'VENEZUELA'
)
chnv <- c("CUBA", "NICARAGUA", "VENEZUELA", "HAITI")
nca <- c("GUATEMALA", "HONDURAS", "EL SALVADOR")


load_captions <- function(){
   caption_blurb <- c("USAID/LAC analysis of CBP data <br>
                   All information is self-reported.")
   caption_blurb_dap <- c("USAID/LAC analysis of Migración Panama data")
   caption_alt <- c("USAID/LAC analysis of CBP data")
   return(list(swb = caption_blurb,
               dar = caption_blurb_dap,
               swb_alt = caption_alt))
}
cap <- load_captions()









