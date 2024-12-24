# setup.R
library(librarian)
shelf(tidyverse, httr, ggthemes, jimjam - slam / ggflags, ggrepel, patchwork, grid,
      ggalluvial, janitor, gt, pdftools, hacksaw, styler,here,
      rnaturalearth, treemapify, see, packcircles, jthomasmock / gtExtras,
      geomtextpath, scales, hrbrmstr / ggchicklet, gghighlight,
      lubridate, Rfast, flextable, gtsummary, readxl, stringi, ggpp,
      sparkline, patchwork, ggtext, ggalt, sf, countrycode, svglite,ggstream,
      quiet = TRUE)

geom_text_repel <- function(..., family = "Source Sans 3") {
   ggrepel::geom_text_repel(
      family = family,
      ...
   )
}
# Load custom themes and functions
source(here("utils","usaidtheme.R"))

# Change some functions 
filter <- dplyr::filter
select <- dplyr::select
`%$%` <- magrittr::`%$%`


#load parameters

base_family <- "Gill Sans"
main_color_1 <- "#002F6C"
main_color_2 <- "#BA2D42"


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
   caption_blurb_dap <- c("USAID/LAC analysis of Migración Panama data <br>
                       All information is self-reported")
   caption_alt <- c("USAID/LAC analysis of CBP data")
   return(list(swb = caption_blurb, dar = caption_blurb_dap,
               swb_alt = caption_alt))
}
cap <- load_captions()









