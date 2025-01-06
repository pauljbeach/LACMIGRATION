# data_prep.R




load_supp_data <- function() {
  # Load datasets
  cc_raw <- read_csv(here("Supp Data", "cc_raw.csv"), show_col_types = F) %>% clean_names()
  
  # Prepare datasets
  fips <- cc_raw %>%
    select(country = official_name_en, fips = iso3166_1_alpha_2) %>%
    mutate(
      citizenship = toupper(country),
      fips = tolower(fips),
      citizenship = case_when(fips == "ve" ~ "VENEZUELA", TRUE ~ citizenship)
    )
  
  cflag <- read_csv(here("Supp Data", "Country_Flags.csv")) %>% 
    select(citizenship = Country, img = ImageURL) %>%
    bind_rows(tibble(
      citizenship = c("Total", "Other"),
      img = c(
        here("emoji", "un.png"),
        here("emoji", "other.png")
      )
    ))
  wb_pop <- read_csv(here("Population Data","WB","wb_pop_2024.csv"),
                     show_col_types = FALSE) %>% 
    clean_names()
  
  pop23 <- wb_pop %>%
    filter(series_code == "SP.POP.TOTL") %>%
    select(citizenship = country_name, pop = x2024_yr2024) %>%
    mutate(
      pop = as.numeric(pop),
      citizenship = toupper(citizenship),
      citizenship = str_remove(citizenship, "(?<=\\,).*") %>%
        str_remove(",") %>% trimws()
    )
  list(fips = fips, cflag = cflag,pop23 = pop23)
  
  
}


swb_14 <- pull_swb_total()
nca_lts <- read.csv(here("Supp Data","nca.lts.csv"))
