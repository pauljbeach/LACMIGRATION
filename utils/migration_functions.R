#####
# Migration Functions
# Paul Beach pbeach@usaid.gov
# Functions to download Migration things
## Darien Encounters, US Visas, UNHCR Refugee, UNDESA Population,

#####

 
library(httr)
library(ggdist)
library(janitor)
library(tidyverse)
library(readxl)
library(countrycode)
library(rvest)
library(pdftools)
library(hacksaw)

# Functions ---------------------------------------------------------------

# Pull Colombia ----------------------------------------------------------

compile_colombia <- function(variables) {
  
  months_es <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  
  months_en <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")
  
  months_map <- setNames(months_en, months_es)
  
  reg23 <- map2_df(c("Arrival_Data_2024.csv",
                 "Arrival_Data_2023.csv"),
                 c(2024,2023),
               ~read_csv(here("Encounter Data", "Colombia",.x ),
                         show_col_types = FALSE) %>% 
                 mutate(year := .y) %>% 
                 clean_names()
  ) %>% 
    rename(count = cantidad_de_filas_agregadas,
           nationality = pais_nacionalidad,
    ) %>% 
    mutate(month = str_replace_all(meses1, months_map),
           date = paste(year,month) %>% ym() )
  
  enc23 <- read_csv(here("Encounter Data", "Colombia",
                         "Encounter_Data.csv"),
                    show_col_types = FALSE) %>% 
    clean_names() %>% 
    rename(year = year_of_fecha_copia,
           nationality = nacionalidad,
           month = month_of_fecha,
           count = total) %>% 
    mutate(date = paste(year,month) %>% ym())
  
return(list(regular = reg23,encounter = enc23))
  
}


# Pull CBP ----------------------------------------------------------------


pull_cbp <- function(month,year = 2024,swb = T,force = F,fy = F, hardlink = NULL, state = F){
  fixed <- stringr::fixed
  month <- tolower(month)
  # month numeric
  m1 <- which(month.abb == str_to_title(month))+1
  #add 0 to m1 if one digit
  if(m1 < 10) m1 <- paste0("0",m1)
  # assuming we are pulling AOR data
  st_e <- "-aor.csv"
  # If we are pulling state data
  if(state == T) st_e <- "-state.csv"
  if(fy == T) month <- ""
  if(fy == T) st_e <- str_remove(st_e,"-")
  # link to stat page
  link = paste0("https://www.cbp.gov/sites/default/files/",
                year,"-",m1,
                "/nationwide-encounters-fy22-fy25-",
                month,
                st_e)
  file  <- ifelse(file.exists(paste0("swb_",month,".csv")) & force == F,
                  paste0("swb_",month,".csv"),
                  link)
  
  if(!is.null(hardlink)) file <- hardlink
  
  cbp <- read_csv(file,show_col_types = F) |>
    clean_names() %>% {
      if(swb == T) dplyr::filter(.,land_border_region == "Southwest Land Border") else .}|>
    mutate(fiscal_year = str_remove_all(fiscal_year, fixed("(FYTD)")) %>% str_trim(),
             #ifelse(fiscal_year %in% "2023 (FYTD)", "2023", fiscal_year),
           year = ifelse(month_abbv %in% c("DEC", "NOV", "OCT"),
                         as.numeric(fiscal_year) - 1,
                         as.numeric(fiscal_year)),
           date = ym(paste(year,"-",month_abbv))) |>
    arrange(citizenship,-as.numeric(date)) |>
    as_tibble() %>% 
    mutate(across(fiscal_year,as.double))
  
  dr <- range(cbp$date)
  
  old <- read_csv("Encounter Data/cbp_old.csv",show_col_types = F) %>% select(-1)%>% dplyr::filter(date < dr[1]) %>% {
    if(swb == T) dplyr::filter(.,land_border_region == "Southwest Land Border") else .}
  cbp2 <- bind_rows(old,cbp) %>% arrange(citizenship,desc(date))
  return(cbp2)
}


# Pull GOM ----------------------------------------------------------------

pull_mex <- function(year = 2024,manuallink = NULL,...){
  skip <- 5
  if(year == 2020) skip <- 4
  mexlink <- paste0("http://www.politicamigratoria.gob.mx/work/models/PoliticaMigratoria/CEM/Estadisticas/Boletines_Estadisticos/",
                    year,
                    "/Cuadros",
                    year,
                    "/cuadro",
                    c("_3.1.1.xls","3.1.1_.xls", "3.1.1.xls"))
  #override with manual link
  if(!is.null(manuallink)) mexlink <- manuallink
  #which month it is
  # n <- which(month.abb == month)
  ml_live <- mexlink[!map_lgl(mexlink,httr::http_error)]
  ml_live <- ml_live[1]
  
  GET(ml_live, write_disk(tf <- tempfile(fileext = ".xls")))
  
  
  df <- read_excel(tf,skip = skip) |> clean_names() %>%
    {if(year >=2021) select(.,x1, contains("subtotal")) else select(.,1:13)}
  n <- ncol(df)-1
  df |> rename_with(~c("country_es",month.abb[1:n])) |>
    drop_na() %>%
    select(country_es,everything()) |>
    pivot_longer(-c(country_es), names_to = "month", values_to = "encounters") |>
    filter(!country_es %in% c(
      "Total general",
      "América",
      "América del Sur",
      "América del Norte",
      "Asia",
      "Europa",
      "Oceania",
      "África",
      "América Central",
      "Islas del Caribe")) |>
    mutate(date = ym(paste(year, month)),
           country_es = case_when(country_es == "Dominicana, Rep." ~"República Dominicana",
                                  T ~ country_es),
           
           encounters = as.character(encounters) %>% parse_number())
}


pull_mex_returns <- function(year = 2023, manuallink = NULL,...){
  skip <- 3
  "http://www.politicamigratoria.gob.mx/work/models/PoliticaMigratoria/CEM/Estadisticas/Boletines_Estadisticos/2024/Cuadros2024/cuadro_3.2.1.xls"
  if(year == 2020) skip <- 4
  mexlink <- paste0("http://www.politicamigratoria.gob.mx/work/models/PoliticaMigratoria/CEM/Estadisticas/Boletines_Estadisticos/",
                    year,
                    "/Cuadros",
                    year,
                    "/cuadro",
                    c("_3.2.1.xls","3.2.1_.xls", "3.2.1.xls"))
  ov <- "http://www.politicamigratoria.gob.mx/work/models/PoliticaMigratoria/CEM/Estadisticas/Boletines_Estadisticos/2023/Cuadros2023/cuadro_3.2.1.xls"
  #override with manual link
  if(!is.null(manuallink)) mexlink <- manuallink
  #which month it is
  # n <- which(month.abb == month)
  ml_live <- mexlink[!map_lgl(mexlink,http_error)]
  ml_live <- ml_live[1]
  #if(year == 2023) ml_live <- ov
  #if(year == 2024 ml_live <- ov
  GET(ml_live, write_disk(tf <- tempfile(fileext = ".xls")))
  
  
  df <- read_excel(tf,skip = skip) |> clean_names() %>%
    {if(year == 2024) select(.,-any_of(c("x11","total"))) else select(.,1:13)}
  n <- ncol(df)-1
  df |> rename_with(~c("country_es",month.abb[1:n])) |>
    drop_na(country_es) %>%
    select(country_es,everything()) |>
    pivot_longer(-c(country_es), names_to = "month", values_to = "encounters") |>
    drop_na(encounters) %>% 
    filter(!country_es %in% c(
      "Total general",
      "América",
      "América del Sur",
      "América del Norte",
      "Asia",
      "Europa",
      "Oceania",
      "África",
      "América Central",
      "Islas del Caribe")) |>
    mutate(date = ym(paste(year, month)),
           country_es = case_when(country_es == "Dominicana, Rep." ~"República Dominicana",
                                  T ~ country_es))
}

pull_mrf <- function(year = c(2019:2024)) {
  map_df(year,~suppressMessages(pull_mex_returns(.)), .progress = T) %>%
    mutate(across(
      country_es,
      ~ case_when(
        str_detect(., "Estados Unidos") ~ "United States",
        str_detect(., "Irak") ~ "Iraq",
        str_detect(., "Irán") ~ "Iran",
        str_detect(., "Centroafricana, Rep.") ~ "Central African Republic",
        . == "Corea, Rep. (Sur)" ~ "South Korea",
        str_detect(., "Checa, Rep.") ~ "Czech Republic",
        str_detect(., "Fiyi") ~ "Fiji",
        . =="Martinica (Terr. Ultram. Francia)" ~ "Martinique",
        . == "Mianmar (Birmania)" ~ "Burma",
        #str_detect(., "Micronesia, Rep.") ~ "Other",
        str_detect(., "Oceanía") ~ "Other",
        . == "Países Bajos (Holanda)" ~"Holland",
        str_detect(., "Perú") ~ "Peru",
        #str_detect(., "Apátridas") ~ "Stateless",
        
        T ~ country_es
      )
    ),
    citizenship = countryname(country_es,warn = F),
    citizenship = case_when(str_detect(country_es, "Micronesia, Rep.") ~ "Other",
                            str_detect(country_es, "Apátridas") ~ "Stateless",
                            T ~ citizenship)) %>%
    summarise(encounter_count = sum(encounters), .by = c(citizenship,date)) %>%
    mutate(citizenship = str_to_upper(citizenship),
           country_of_entry = "MEXICO")
}
#this is the one for all year
pull_mex_full <- function(year = c(2019:2024)) {
  map_df(year,~suppressMessages(pull_mex(.)), .progress = T) %>%
    mutate(across(
      country_es,
      ~ case_when(
        str_detect(., "Estados Unidos") ~ "United States",
        str_detect(., "Irak") ~ "Iraq",
        str_detect(., "Irán") ~ "Iran",
        str_detect(., "Centroafricana, Rep.") ~ "Central African Republic",
        . == "Corea, Rep. (Sur)" ~ "South Korea",
        str_detect(., "Checa, Rep.") ~ "Czech Republic",
        str_detect(., "Fiyi") ~ "Fiji",
        . =="Martinica (Terr. Ultram. Francia)" ~ "Martinique",
        . == "Mianmar (Birmania)" ~ "Burma",
        #str_detect(., "Micronesia, Rep.") ~ "Other",
        str_detect(., "Oceanía") ~ "Other",
        . == "Países Bajos (Holanda)" ~"Holland",
        str_detect(., "Perú") ~ "Peru",
        #str_detect(., "Apátridas") ~ "Stateless",
        
        T ~ country_es
      )
    ),
    citizenship = countryname(country_es,warn = F),
    citizenship = case_when(str_detect(country_es, "Micronesia, Rep.") ~ "Other",
                            str_detect(country_es, "Apátridas") ~ "Stateless",
                            T ~ citizenship)) %>%
    summarise(encounter_count = sum(encounters), .by = c(citizenship,date)) %>%
    mutate(citizenship = str_to_upper(citizenship),
           country_of_entry = "MEXICO") %>% 
    arrange(desc(date))
}



# Pull Other GOM
pull_us_returns <- function(year = 2022, manuallink = NULL,...){
  skip <- 3
  if(year == 2020) skip <- 4
  mexlink <- paste0("http://www.politicamigratoria.gob.mx/work/models/PoliticaMigratoria/CEM/Estadisticas/Boletines_Estadisticos/",
                    year,
                    "/Cuadros",
                    year,
                    "/cuadro",
                    c("5.1.xls","_5.1.xls", "5.1_.xls"))
  ov <- "http://www.politicamigratoria.gob.mx/work/models/PoliticaMigratoria/CEM/Estadisticas/Boletines_Estadisticos/2023/Cuadros2023/cuadro_5.1.xls"
  #override with manual link
  if(!is.null(manuallink)) mexlink <- manuallink
  #which month it is
  # n <- which(month.abb == month)
  ml_live <- mexlink[!map_lgl(mexlink,http_error)]
  ml_live <- ml_live[1]
  #if(year == 2023) ml_live <- ov
  GET(ml_live, write_disk(tf <- tempfile(fileext = ".xls")))
  
  
  df <- read_excel(tf,skip = skip) |> clean_names() %>%
    {if(year == 2024) select(.,-any_of(c("x11","total"))) else select(.,1:13)}
  n <- ncol(df)-1
  df |> rename_with(~c("country_es",month.abb[1:n])) |>
    drop_na(country_es) %>%
    select(country_es,everything()) |>
    pivot_longer(-c(country_es), names_to = "month", values_to = "encounters") |>
    drop_na(encounters) %>% 
    filter(!country_es %in% c(
      "Total general",
      "América",
      "América del Sur",
      "América del Norte",
      "Asia",
      "Europa",
      "Oceania",
      "África",
      "América Central",
      "Islas del Caribe")) |>
    mutate(date = ym(paste(year, month)),
           country_es = case_when(country_es == "Dominicana, Rep." ~"República Dominicana",
                                  T ~ country_es)) %>% 
    rename(department = country_es, returns = encounters)
}

pull_usmrf <- function(year = c(2019:2023)) {
  map_df(year,~suppressMessages(pull_us_returns(.)), .progress = T) %>%
    summarise(return_count = sum(returns), .by = c(department,date))
}


# Pull Darien ----------------------------------------------------------------


pull_dar <- function(year = 2024,
                     month = NULL,
                      manuallink = NULL,
                      new_pdf = NULL){
  
  olde <- read_csv("Encounter Data/Darien/Darien20.23.csv",show_col_types = F)

  urlpan <- paste0("https://migracion.gob.pa/wp-content/uploads/IRREGULARES-POR-DARIEN-",
                   toupper(month),
                   ".pdf")
                   
  # Create a temporary file with a .pdf extension
  temp_file <- tempfile(fileext = ".pdf")
  
  # Use GET to download the file with headers mimicking a Chrome request
  response <- GET(
    urlpan,
    write_disk(temp_file, overwrite = TRUE),
    add_headers(
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/85.0.4183.121 Safari/537.36",
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
      "Accept-Language" = "en-US,en;q=0.9",
      "Connection" = "keep-alive"
    ),
    config = config(ssl_verifypeer = FALSE)  # Ignore SSL certificate issues if necessary
  )
  
  # Check if the request was successful
  if (response$status_code == 200) {
    message("Download successful, file is not empty.")
  } else {
    message("Failed to download file. Status code: ", response$status_code)
  }

  
  # read pdf

  fl1 <- temp_file %>% 
    pdf_text() %>% 
    .[2] %>% 
    str_split("\n") %>% 
    pluck(1) %>% 
    trimws() %>% 
    str_remove_all(fixed("(1)")) 
first_pais_pos <- which(grepl("País", fl1))[1]
  
  # Remove all elements before the one containing "pais"
fl2 <- fl1[first_pais_pos:length(fl1)] %>%  
    str_split_fixed(" {1,}",14) %>% 
    #remove the first three rows 
    .[-c(2),] %>% 
    as_tibble() %>% 
    row_to_names(1) %>%
    clean_names() %>%
    .[1:which(.$pais == "Otros"),] %>% 
    filter(pais != "") %>% 
    mutate(total = ifelse(str_detect(total,"[:digit:]"),total,NA),
           across(-pais, ~ifelse(. == "", NA,.)),
           across(-c(pais), ~str_remove_all(.,"[:punct:]+") %>% as.numeric()),
           across(-c(pais,total), ~replace_na(.,0)))
    #remove columns that sum to 0
   
    
# find row where the first column = otros

  
   dar <- bind_cols(fl2[1],shift_row_values(fl2[,-1], at = which(is.na(fl2[,2])))) %>%
    select(-total) %>% 
    filter(!str_detect(pais,"Hijos de ciudadanos"),
           pais != "Total") %>% 
    # .[,1:(which(month.abb == str_to_title(month))+1)] %>% 
     
    mutate(across(-pais, ~ifelse(. == "", NA,.)),
           across(-pais, ~str_remove_all(.,"[:punct:]+") %>% as.numeric()),
           across(-pais, ~replace_na(.,0))) %>% 
     select(where(~ !is.numeric(.) || sum(.,na.rm = T) != 0)) %>% 
    pivot_longer(-pais,names_to = "month", values_to = "encounter_count") %>% 
    mutate(year = year,
           month = case_when(month == "ene" ~ "jan",
                             month == "ago" ~ "aug",
                             month == "abr" ~ "apr",
                             month == "dic" ~ "dec",
                             T ~ month),
           date = ym(paste0(year,month)),
           pais = gsub(r"{\s*\([^\)]+\)}","",pais) %>%  str_trim(),
           pais = case_when(pais == "Perú" ~ "Peru",
                            pais == "Rep." ~"Dominican Republic",
                            pais == "Irán" ~ "Iran",
                            pais == "Sri" ~ "Sri Lanka",
                            T ~ pais,
                            ),
           
           citizenship = countryname(pais,warn = F),
           citizenship = case_when(pais == "Otros"~ "Other",
                                   pais == "África" ~ "Africa",
                                   T ~ citizenship)) %>% 
    summarise(across(encounter_count,sum), .by = c(date,citizenship)) %>%
    mutate(citizenship = str_to_upper(citizenship),
           country_of_entry = "PANAMA") %>%
    bind_rows(olde %>% filter(year(date)< year)) %>% 
    arrange(desc(date))
  return(dar)
}

# Pull UNHCR --------------------------------------------------------------


pull_unhcr <- function(orig = "all", dest = "all", idmc = F){
  # Since unhcr is updated every 6 months or so (June, Jan) just going to point
  # this to a file and clean and load the refugees package
  # remember to replace, otherwise will pick the newest
  # the data from the UNHCR online portal is sometimes newer than the r package
  
 library(refugees)
 orig <- ifelse(orig == "LAC", "Latin America and the Caribbean",orig)
 dest <- ifelse(dest == "LAC", "Latin America and the Caribbean",dest)

if(orig != "all"){
orig_reg <- map(colnames(refugees::countries),
     ~.x[orig %in% unique(countries[[.x]])]) %>% 
   unlist() %>% 
  .[1]
orig_iso <- countries$iso_code[(countries[[orig_reg]] == orig )]
orig_iso <- orig_iso[!is.na(orig_iso)]
}
 
 
if(dest != "all"){
dest_reg <- map(colnames(refugees::countries),
                ~.x[dest %in% unique(countries[[.x]])]) %>% 
  unlist() %>% 
  .[1]
dest_iso <- countries$iso_code[(countries[[dest_reg]] == dest )]
dest_iso <- dest_iso[!is.na(dest_iso)]

}

 fl <- list.files("Population Data/UNHCR",full.names = T)
 fl <- file.info(fl) %>% arrange(desc(ctime)) %>% rownames()
 unhcr <- suppressMessages(read_csv(fl[1], skip = 14,show_col_types = F) %>% as_tibble %>% clean_names())
 u2 <- unhcr %>% rename(coo_name = country_of_origin,
                  coo_iso = country_of_origin_iso,
                  coa_name = country_of_asylum,
                  coa_iso = country_of_asylum_iso,
                  refugees = refugees_under_unhc_rs_mandate,
                  stateless = stateless_persons,
                  hst = host_community,
                  ooc = others_of_concern,
                  oip = other_people_in_need_of_international_protection,
                  idps = id_ps_of_concern_to_unhcr) %>%
   select(coo_iso,coa_iso,year,refugees,stateless,hst,ooc,oip,idps,asylum_seekers) %>% 
   full_join(refugees::unrwa %>% 
               select(year,coa_iso,coo_iso,pal_ref = total),
             by = c("year", "coo_iso", "coa_iso"),multiple =  "any") %>% 
   mutate(across(c(refugees, stateless, asylum_seekers, hst, ooc, oip, idps,pal_ref),
                 ~str_remove(., "[:punct:]+") %>% 
                   as.numeric() %>% 
                   replace_na(0)),
          fdp = refugees + oip +asylum_seekers + idps +pal_ref,
          fidp = fdp - idps) %>% 
   arrange(desc(year))
 
 if(idmc == T){
   u2 <- 
     u2 %>% 
     full_join(
       refugees::idmc %>% select(year,coa_iso,coo_iso,idmc_ref = total),
       by = c("year", "coa_iso", "coo_iso")
     ) %>% 
     mutate(across(where(is.numeric),~replace_na(.,0))) %>% 
     mutate(fdp = refugees + oip + asylum_seekers + idmc_ref + pal_ref,
            fidp = fdp - idmc_ref) %>% 
     arrange(desc(year))
 } else u2
if(orig != "all"){
  u2 <- u2 %>% filter(coo_iso %in% orig_iso)
}
 
if(dest != "all"){
   u2 <- u2 %>% filter(coa_iso %in% dest_iso)
 }

u2 <- u2 %>% 
  left_join(countries %>% select(coa_iso = iso_code, coa_name = name),
            by = c("coa_iso")) %>% 
  left_join(countries %>% select(coo_iso = iso_code, coo_name = name),
            by = c("coo_iso"))

return(u2)
}


# Pull Honduras #####
compile_honduras <- function(csv){
  #csv <- "Honduras's Migrant Encounters Since August 2022 - Sheet1.csv"
  hn <- read_csv(csv,show_col_types = F) %>% 
    clean_names() %>%
    select(nationality,contains(month.abb %>% tolower())) %>% 
    pivot_longer(-nationality) 
  hn2 <- hn %>% 
    
    mutate(across(nationality, ~ifelse(str_detect(.,","),
                                       str_extract(.,".*(?=\\,)"),
                                       .)
    )
    ) %>%
    mutate(citizenship = countryname(nationality,
                                     warn = F),
           citizenship = case_when(nationality == "Perú" ~"Peru",
                                   is.na(citizenship)~nationality,
                                   T ~ citizenship),
           across(citizenship, ~stri_trans_general(., id = "Latin-ASCII") %>% toupper())
    ) %>% 
    mutate(month = str_extract(name,paste(month.abb %>% tolower, collapse = "|")),
           year = parse_number(name)+2000,
           date = paste(month,year) %>% my()) %>% 
    filter(nationality != "Total") %>% 
    select(-c(nationality,name,month,year)) %>%
    rename(nationality = citizenship,
           encounter_count = value,
    ) %>% 
    select(nationality,date,encounter_count) %>% 
    mutate(encounter_count = replace_na(encounter_count,0)) %>% 
    arrange(desc(date))
  
  return(hn2)
}



# Pull CBP HUB ####
update_hub <- function(month = NULL, year = 2024){

month.abb
urlhub <- "https://ohss.dhs.gov/topics/immigration/enforcement/legal-processes-monthly-tables"
month <- tolower(month)
  # use rvest to scrape the links and identify the (likely correct one)
  hubsite <- urlhub %>% 
    httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
    rvest::read_html()  
  
  hublinks <- hubsite %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>% 
    .[str_detect(.,"ohss_immigration-enforcement-and-legal-processes-tables")] %>% 
    paste0("https://ohss.dhs.gov",.)
  
  extract_month_year <- function(link) {
    # Use a regular expression to capture the month-year pattern at the end of the link
    match <- regmatches(link, regexpr("(january|february|march|april|may|june|july|august|september|october|november|december)-\\d{4}", link, ignore.case = TRUE))
    if (length(match) > 0) {
      # Convert the extracted string to a date object for comparison
      as.Date(paste0("01-", match), format = "%d-%B-%Y")
    } else {
      NA
    }
  }
  
  # Apply the month-year extraction function to all links
  dates <- sapply(hublinks, extract_month_year)
  
  # Find the index of the most recent date
  most_recent_index <- which.max(dates)
  
  # Get the most recent link
  most_recent_link <- hublinks[most_recent_index]
  
  # Output the result
  cat("The most recent link is:\n", dates[which.max(dates)] %>% as.Date() %>% format("%B %Y"), "\n")  
  download_file <- function(url, path = here("Encounter Data", "cbp_hub.xlsx")) {
    tryCatch(
      {
        GET(url, write_disk(path,overwrite = T))
        message("File downloaded successfully")
      },
      error = function(e) {
        message("Failed to download,\nTrying fallback URL...")
        return(FALSE) # Return FALSE if download fails
      }
    )
  }
  
ret <- download_file(most_recent_link)
}




pull_swblt <- function(){
  cbp_swb_total_raw <- read_excel(here("Encounter Data", "cbp_hub.xlsx"),
                                  sheet = "SWB Encounters by Citizenship",
                                  range = "A4:N200") %>% 
    clean_names() %>%
    fill(fiscal_year) %>% 
    filter(month != "Total",
           month != "") %>% 
    mutate(across(c(fiscal_year),
                  parse_number),
           year = ifelse(month %in% month.name[10:12], fiscal_year -1,fiscal_year),
           date = my(paste(month,year))
    ) %>% 
    select(-c(total,month,fiscal_year,year)) %>% 
    pivot_longer(-date,names_to = "citizenship",
                 values_to = "encounter_count") %>%
    filter(!str_detect(citizenship,"all_other")) %>%
    mutate(across(citizenship, ~toupper(.) %>% str_replace_all("_"," "))) 
  
  return(cbp_swb_total_raw)
}

pull_usbp <- function(){
  cbp_swb_total_raw <- read_excel(here("Encounter Data","OUT","cbp_hub.xlsx"),
                                  sheet = "Nationwide USBP",
                                  skip = 2) %>% 
    clean_names() %>%
    fill(fiscal_year) %>% 
    filter(month != "Total") %>% 
    mutate(across(c(fiscal_year),
                  parse_number),
           year = ifelse(month %in% month.name[10:12], fiscal_year -1,fiscal_year),
           date = my(paste(month,year))
    ) %>% 
    select(-c(total,month,fiscal_year,year)) %>% 
    pivot_longer(-date,names_to = "citizenship",
                 values_to = "encounter_count") %>%
    filter(!str_detect(citizenship,"all_other")) %>%
    mutate(across(citizenship, ~toupper(.) %>% str_replace_all("_"," "))) %>% 
    mutate(component = "U.S. Border Patrol")
  
  return(cbp_swb_total_raw)
}
pull_ofo <- function(){
  cbp_swb_total_raw <- read_excel(here("Encounter Data","OUT", "cbp_hub.xlsx"),
                                  sheet = "Nationwide OFO",
                                  skip = 2) %>% 
    clean_names() %>%
    fill(fiscal_year) %>% 
    filter(month != "Total") %>% 
    mutate(across(c(fiscal_year),
                  parse_number),
           year = ifelse(month %in% month.name[10:12], fiscal_year -1,fiscal_year),
           date = my(paste(month,year))
    ) %>% 
    select(-c(total,month,fiscal_year,year)) %>% 
    pivot_longer(-date,names_to = "citizenship",
                 values_to = "encounter_count") %>%
    filter(!str_detect(citizenship,"all_other")) %>%
    mutate(across(citizenship, ~toupper(.) %>% str_replace_all("_"," "))) %>% 
    mutate(component = "Office of Field Operations")
  
  return(cbp_swb_total_raw)
}

pull_swb_total <- function(){
  
  cbp_swb_total_raw <- read_excel(here("Encounter Data","OUT","cbp_hub.xlsx"),
                                  sheet = "CBP Encounter by Type + Region",
                                  skip = 3) %>% 
    clean_names() %>%
    select(fiscal_year,month,encounter_count = sw_land_border_4) %>% 
    fill(fiscal_year) %>% 
    filter(month != "Total") %>% 
    mutate(across(c(fiscal_year),
                  parse_number),
           year = ifelse(month %in% month.name[10:12], fiscal_year -1,fiscal_year),
           date = my(paste(month,year))
    ) %>% 
    select(date,encounter_count)
  #get the max date
  max_date <- max(cbp_swb_total_raw$date)
  
  ne <- read_csv(here("Migration Update", "fallback_data", "fallback_cbp.csv")) %>% 
    filter(date > max_date) %>% 
    summarise(across(encounter_count,sum),.by = date)
  
  
  bind_rows(cbp_swb_total_raw,ne) %>% 
    arrange(desc(date))
}

pull_chnv <- function(){
  cbp_raw <- read_excel(here("Encounter Data","OUT","cbp_hub.xlsx"),
                        sheet = "CHNV Paroles",
                        skip = 2)
  
  cbp_raw %>% 
    clean_names() %>%
    fill(fiscal_year) %>% 
    filter(month != "Total") %>% 
    mutate(across(c(-month),
                  ~str_remove(.,"x|X") %>% 
                    parse_number()),
           year = ifelse(month %in% month.name[10:12], fiscal_year -1,fiscal_year),
           date = my(paste(month,year))
    ) %>% 
    select(-c(month,year,fiscal_year,total)) %>% 
    pivot_longer(-date,names_to = "citizenship",
                 values_to = "parole_count") %>% 
    drop_na(parole_count) %>% 
    mutate(across(citizenship, toupper))
}

pull_cbpone<- function(){
  cbp_raw <- read_excel(here("Encounter Data","OUT","cbp_hub.xlsx"),
                        sheet = "CBP One Appointments",
                        skip = 2)
  
  cbp_raw %>% 
    clean_names() %>%
    fill(fiscal_year) %>% 
    filter(month != "Total") %>% 
    mutate(across(c(-month),
                  ~str_remove(.,"x|X") %>% 
                    parse_number()),
           year = ifelse(month %in% month.name[10:12], fiscal_year -1,fiscal_year),
           date = my(paste(month,year))
    ) %>% 
    select(-c(month,year,fiscal_year,total)) %>% 
    pivot_longer(-date,names_to = "citizenship",
                 values_to = "cbp1_count") %>% 
    drop_na(cbp1_count) %>% 
    mutate(across(citizenship, ~toupper(.) %>% str_replace_all("_"," ")))
}

pull_cf_result <-  function(){
  cbp_raw <- read_excel(here("Encounter Data" ,"cbp_hub.xlsx"),
                        sheet = "Nationwide CF by Result",
                        skip = 2)
  
  cbp_raw %>% 
    clean_names() %>%
    fill(fiscal_year) %>% 
    filter(month != "Total") %>% 
    mutate(across(c(-month),
                  ~str_remove(.,"x|X") %>% 
                    parse_number()),
           year = ifelse(month %in% month.name[10:12], fiscal_year -1,fiscal_year),
           date = my(paste(month,year))
    ) %>% 
    select(-c(month,year,fiscal_year,total)) %>% 
    pivot_longer(-date,names_to = "fear_category",
                 values_to = "count") %>% 
    drop_na(count) %>% 
    mutate(across(fear_category, ~toupper(.) %>% str_replace_all("_FEAR","")))
}

pull_cf_citizenship <-  function(){
  cbp_raw <- read_excel(here("Encounter Data" ,"cbp_hub.xlsx"),
                        sheet = "Nationwide CF Filed by Citp",
                        skip = 2)
  
  cbp_raw %>% 
    clean_names() %>%
    fill(fiscal_year) %>% 
    filter(month != "Total") %>% 
    mutate(across(c(-month),
                  ~str_remove(.,"x|X") %>% 
                    parse_number()),
           year = ifelse(month %in% month.name[10:12], fiscal_year -1,fiscal_year),
           date = my(paste(month,year))
    ) %>% 
    select(-c(month,year,fiscal_year,total)) %>% 
    pivot_longer(-date,names_to = "fear_category",
                 values_to = "count") %>% 
    drop_na(count) %>% 
    mutate(across(fear_category, ~toupper(.) %>% str_replace_all("_"," ")))
}

pull_out_citp <- function(){
  out_raw <- read_excel(here("Encounter Data","OUT","cbp_hub.xlsx"),
                        sheet = "CBP SWB Book-Outs by Citp",
                        range = "A4:DD139")
  out <- out_raw %>% 
    clean_names() %>% 
    mutate(across(fiscal_year, as.character)) %>% 
    fill(fiscal_year) %>% 
    pivot_longer(-c(month,fiscal_year)) %>% 
    mutate(col = str_extract(name, "(\\d+$)") %>% parse_number(),
           citizenship = case_when(col %in% 3:11 ~"Total",
                                   col %in% (c(3:11)+(9*1)) ~"Mexico",
                                   col %in% (c(3:11)+(9*2)) ~"Venezuela",
                                   col %in% (c(3:11)+(9*3)) ~"Guatemala",
                                   col %in% (c(3:11)+(9*4)) ~"Honduras",
                                   col %in% (c(3:11)+(9*5)) ~"Colombia",
                                   col %in% (c(3:11)+(9*6)) ~"Ecuador",
                                   col %in% (c(3:11)+(9*7)) ~"Cuba",
                                   col %in% (c(3:11)+(9*8)) ~"Haiti",
                                   col %in% (c(3:11)+(9*9)) ~"El Salvador",
                                   col %in% (c(3:11)+(9*10)) ~"Peru",
                                   col %in% (c(3:11)+(9*11)) ~"Other"),
           across(name,~str_remove(., "(\\_\\d+$)") %>% str_remove("(\\d$)")),
           across(fiscal_year,parse_number),
           year = ifelse(month %in% c("October", "November", "December"), fiscal_year -1, fiscal_year),
    ) %>% 
    filter(value !="X",
           citizenship != "Total",
           month != "Total",
           !str_detect(name,"total")) %>% 
    mutate(date = paste(year,month) %>% ym(),
           across(value,parse_number)) %>% 
    select(date,citizenship,cat = name, count = value) %>% 
    mutate(n = n(),.by = -count,
           across(count, ~ifelse(n >1,./2,.))) %>% 
    summarise(across(count,sum),.by = -count)
  return(out)
}

pull_out_total <- function(){
    out_raw <- read_excel(here("Encounter Data","OUT","cbp_hub.xlsx"),
                          sheet = "CBP SWB Book-Outs by Agency",
                          range = "A4:K146")
    out <- out_raw %>% 
      clean_names() %>% 
      mutate(
              #across(fiscal_year, as.character),
             across(-c(month, where(is.numeric)), ~parse_number(.))) %>% 
      fill(fiscal_year) %>% 
      pivot_longer(-c(month,fiscal_year)) %>% 
      mutate(across(name,~str_remove(., "(\\_\\d+$)") %>% str_remove("(\\d$)")),
             year = ifelse(month %in% c("October", "November", "December"), fiscal_year -1, fiscal_year),
      ) %>% 
      filter(!is.na(value),
             name != "total_encounters",
             month != "Total") %>% 
      mutate(date = paste(year,month) %>% ym()) %>% 
      select(date,cat = name, count = value) %>% 
      summarise(across(count,sum),.by = -count)
    return(out)
  }

pull_out_fam <- function(){
  out_fam_raw <- 
    read_excel("cbp_hub.xlsx",
               sheet = "CBP SWB Book-Outs by Family",
               range = "A4:AE139")
  out_fam <- out_fam_raw %>% 
    clean_names() %>% 
    mutate(across(fiscal_year, as.character)) %>% 
    fill(fiscal_year) %>% 
    pivot_longer(-c(month,fiscal_year)) %>% 
    mutate(col = str_extract(name, "(\\d+$)") %>% parse_number(),
           demographic = case_when(col %in% 3:11 ~"Total",
                                   col %in% c(12:19) ~"Single Adults",
                                   col %in% c(20:27) ~"Family Units",
                                   col %in% c(28:31) ~"Unacompanied Minors",
           ),
           across(name,~str_remove(., "(\\_\\d+$)") %>% str_remove("(\\d$)")),
           across(fiscal_year,parse_number),
           year = ifelse(month %in% c("October", "November", "December"), fiscal_year -1, fiscal_year),
    ) %>% 
    filter(value !="X",
           demographic != "Total",
           month != "Total",
           !str_detect(name,"total")) %>% 
    mutate(date = paste(year,month) %>% ym(),
           across(value,parse_number)) %>% 
    select(date,demographic,cat = name, count = value) %>%
    summarise(across(count,sum),.by = -count)
  
  return(out_fam)
}
# write function to pull lt cbp encounters

pull_component <-  function(){
  
  out_fam_raw <- 
    map(c("Nationwide OFO", "Nationwide USBP"),
        ~read_excel("cbp_hub.xlsx",
                    sheet = .x,
                    skip = 2,
                    #range = "A4:AE139",
                    )
    )
  
  out_fam <- 
    map2_df(out_fam_raw,
            c("OFO", "USBP"),
            ~clean_names(.x) %>% 
              mutate(across(fiscal_year, as.character)) %>% 
              fill(fiscal_year) %>% 
              pivot_longer(-c(month,fiscal_year)) %>% 
              filter(name != "total",
                     month != "Total",
                     #remove extra footnotes 
                     str_length(fiscal_year)<=9) %>% 
              mutate(
                across(name,~str_remove(., "(\\_\\d+$)") %>% str_remove("(\\d$)")),
                across(c(fiscal_year,
                         value),
                       parse_number),
                year = ifelse(month %in% c("October", "November", "December"),
                              fiscal_year -1, 
                              fiscal_year),
                date = paste(year,month) %>% ym()
              ) %>% 
              select(date,citizenship = name, count = value) %>% 
              mutate(component = .y)
    )
  
  return(out_fam)
}


# LAC Map Maker -----------------------------------------------------------


lac_map_maker <- function(begin = as.Date("2022-10-01"),
                          dar1 = dar,
                          mex1 = mex,
                          us = ne){

  dar23 <- dar |>
    mutate(pais = case_when(pais == "Brasil (1)" ~ "Brasil",
                            pais == "Chile (1)" ~ "Chile",
                            pais == "Rep. Dominicana" ~ "República Dominicana",
                            T ~ pais),
           fiscal_year = ifelse(month %in% c("oct", "nov", "dec"), ano +1, ano)) |>
    group_by(pais,fiscal_year) |>
    summarise(across(encounters,sum)) |>
    rename(name_es = pais,darenc = encounters)

  mex23 <- mex1 |>
    select(name_es = country_es,
           date,
           mex_encounters = encounters) |>
    group_by(name_es, date) |>
    summarise(across(mex_encounters,sum)) |>
    filter(mex_encounters > 500) |>
    arrange(-mex_encounters)

  nems23 <- ne |>
    mutate(citizenship = ifelse(citizenship %in% non_lac, "OTHER", citizenship),
           name = str_to_title(citizenship)) |>
    group_by(name, date) |>
    summarise(across(encounter_count,sum)) |>
    left_join(pop23) |>
    mutate(migpt = encounter_count/pop)

  lacm23 <- lac |>
    left_join(nems23) |>
    left_join(dar23) |>
    left_join(mex23) |>
    pivot_longer(cols = c(encounter_count,darenc,mex_encounters),
                 names_to = "location", values_to = "encounters") |>
    rename(ac = encounters) |>
    select(name,ac,location,everything()) |>
    mutate(point = st_centroid(geometry))|>
    mutate(x = st_coordinates(point)[,1],
           y = st_coordinates(point)[,2],
           xend = case_when(location == "darenc" ~ -77,
                            location == "encounter_count" ~ -98,
                            location == "mex_encounters" ~ -102.5),
           yend = case_when(location == "darenc" ~ 7,
                            location == "encounter_count" ~ 39,
                            location == "mex_encounters" ~ 23.5),
           location = case_when(location == "darenc" ~ "Darien Gap",
                                location == "encounter_count" ~ "US Southwest Border",
                                location == "mex_encounters" ~ "Mexico Border")) |>
    mutate(
      alpha = ifelse(is.na(ac), 0,1))

  return(lacm23)
}



# Axis Date Labels --------------------------------------------------------


tm_db <- function(month = "Jan", n = 3,year = 2024, year0 = 2020){
years <- year0:year
date2 <- my(paste(month,year))
ms <- seq(from = which(month.abb == month),length.out = n, by = 12/3)%%12
ms[ms == 0] <- 12
  tm_db <- outer(ms,
                 years,
                 paste,
                 sep=" ") %>%
  as.vector() %>%
  my()
return(tm_db[tm_db <= as.Date(date2)])

}

fiscal_year <- function(date){
  require(lubridate)
  year <- year(date)
  month <- month(date)
  fiscal_year <- ifelse(month>9,year+1,year)
  return(fiscal_year)
}

tm_dby <- function(
    anchor_date = Sys.Date(),
    year_step   = 5,
             # defaults to today's date
    min_date    = as.Date("2000-01-01"),
    max_date    = as.Date("2030-12-31")
) {
  # Round the anchor_date to the 1st of its month
  anchor <- as.Date(format(anchor_date, "%Y-%m-01"))
  
  out <- c()
  current <- anchor
  
  # Step backward in "year_step" increments until below min_date
  while (current >= min_date) {
    # Keep if it's also <= max_date
    if (current <= max_date) {
      out <- c(out, current)
    }
    
    # Move backward by 'year_step' years
    # Trick: seq(current, length.out=2, by="-5 years") returns two dates,
    # we take the second one to get 'current minus 5 years'
    current <- seq(current, length.out = 2, by = paste0("-", year_step, " years"))[2]
  }
  out <-as.Date(out)
  sort(out)
}
# Color Scales --------------------

cbp_pallete <-
  c(
    "HONDURAS" = "purple4",
    "Honduras" = "purple4",
    "GUATEMALA"= "skyblue",
    "Guatemala"= "skyblue",
    "EL SALVADOR"="darkorchid",
    "El Salvador"="darkorchid",
    "MEXICO"=   "#1E2CAB",
    "Mexico" =  "#1E2CAB",
    "Nicaragua"="deepskyblue4",
    "NICARAGUA"="deepskyblue4",
    "Dominican Republic" = "palegreen3",
    "DOMINICAN REPUBLIC" = "palegreen3",
    
    "HAITI"=    "palegreen2",
    "Haiti"=    "palegreen2",
    "Cuba"=     "forestgreen",
    "CUBA"=     "forestgreen",
    
    "ECUADOR"=   "#FC8125",
    "Ecuador"=   "#FC8625",
    'PERU' =    "orange",
    'Peru' =    "orange",
    "Venezuela"="darkgoldenrod",
    "VENEZUELA"="darkgoldenrod",
    "BRAZIL"=   "coral4",
    "Brazil"=   "coral4",
    "COLOMBIA"= "coral1",
    "Colombia"= "coral1",
    "Chile" = "coral3",
    "CHILE" = "coral3",
    
   
    
    # "All Others" = "darkred",
    # "OTHER"=    "darkred",
    # "Other"=    "darkred",
    "All Others" = "grey50",
    "OTHER"=    "grey50",
    "Other"=    "grey50",
    "China" = "pink2",
    "CHINA" = "pink2",
    "Northern Central America" = "coral",
    "NORTHERN CENTRAL AMERICA" = "coral",
    "NORTHERN\nCENTRAL AMERICA" = "coral",
    "NORTHERN\nCENTRAL\nAMERICA" = "coral",
    "Northern \nCentral \nAmerica" = "coral",
    "Northern Central \nAmerica" = "coral",
    "CHNV" = "darkgreen",
    "CHNV\nCountries" = "darkgreen",
    "CHNV\nCOUNTRIES" = "darkgreen",
    "CHNV COUNTRIES" = "darkgreen",
    "NCA" = "coral",
    "SENEGAL" = "yellow1",
    "GUINEA" =  "yellow3"
    
  )
cbp_pallete2 <-
  c(
    "HONDURAS" = "#7b4173",
    "Honduras" = "#7b4173",
    "GUATEMALA"= "#6b6ecf",
    "Guatemala"= "#6b6ecf",
    "EL SALVADOR"="#393b79",
    "El Salvador"="#393b79",
    "MEXICO"=   "#1E2CAB",
    "Mexico" =  "#1E2CAB",
    "Nicaragua"="deepskyblue4",
    "NICARAGUA"="deepskyblue4",
    
    
    "Dominican Republic" =  "#cedb9c",
    "DOMINICAN REPUBLIC" =  "#cedb9c",
    
    "HAITI"=    "#8ca252",
    "Haiti"=    "#8ca252",
    "Cuba"=     "forestgreen",
    "CUBA"=     "forestgreen",
    
    "ECUADOR"=   "#FC8125",
    "Ecuador"=   "#FC8625",
    'PERU' =    "orange",
    'Peru' =    "orange",
    "Venezuela"="#8c6d31",
    "VENEZUELA"="#8c6d31",
    "BRAZIL"=   "coral4",
    "Brazil"=   "coral4",
    "COLOMBIA"= "coral1",
    "Colombia"= "coral1",
    "Chile" = "coral3",
    "CHILE" = "coral3",
    "All Others" = "grey50",
    "OTHER"=    "grey50",
    "Other"=    "grey50",
    "China" = "#ce6dbd",
    "CHINA" = "#ce6dbd",
    "Northern Central America" = "coral",
    "NORTHERN CENTRAL AMERICA" = "coral",
    "NORTHERN\nCENTRAL AMERICA" = "coral",
    "NORTHERN\nCENTRAL\nAMERICA" = "coral",
    "Northern \nCentral \nAmerica" = "coral",
    "Northern Central \nAmerica" = "coral",
    "CHNV" = "darkgreen",
    "CHNV\nCountries" = "darkgreen",
    "CHNV\nCOUNTRIES" = "darkgreen",
    "CHNV COUNTRIES" = "darkgreen",
    "NCA" = "coral",
    "SENEGAL" = "yellow1",
    "GUINEA" =  "yellow3"
    
  )
scale_fill_cbp <- function(...) {
  scale_fill_manual(...,values = cbp_pallete2)
}

scale_color_cbp <- function(...) scale_color_manual(values = cbp_pallete2)
                                                    

# Other -------------------------------------------------------------------


# Quick Summary Functions for CBP
give_swb_info <- function(mon, year = 2023){
  if(!exists("ne")) ne <- pull_cbp()
  date2 <- my(paste(month2,year2))
  latest <-  ne |> group_by(date) |> summarise(across(encounter_count,sum)) |> filter(date == date2) |> pull(encounter_count)

  diff <- ne |> group_by(date) |>
    summarise(across(encounter_count,sum)) |>
    arrange(-as.numeric(date)) |>
    slice_head(n = 2) |>
    pivot_wider(names_from = date, values_from = encounter_count) |>
    rename_with(~c("m1", "m2")) |>
    mutate(diff = m1 -m2) |>
    pull(diff)
  ret <- c(latest,diff) |> setNames(c("Latest", "Difference"))
  return(ret)
}
det_swb_info <- function(date1a =date1, date2a = date2, country = NULL, df = NULL){
  if(!exists("ne")) ne <- pull_cbp()
  if(!is.null(df)) ne <- df
  date2a <- if(class(date2a) != "Date") as.Date(date2a) else date2a
  date1a <- if(class(date1a) != "Date") as.Date(date1a) else date1a


  diff <- ne |> group_by(date) %>%
    {if(!is.null(country)) filter(.,citizenship == toupper(country)) else filter(.)} |>
    summarise(across(encounter_count,sum)) |>
    filter(date %in% c(date1a,date2a)) %>%
    pivot_wider(names_from = date, values_from = encounter_count) |>
    rename_with(~c("m1", "m2")) |>
    mutate(diff = m2 -m1,
           pct = (m2-m1)/m1*100,
           fct = m2/m1)
  ret <- c(pull(diff,diff),pull(diff,pct), pull(diff,fct)) |> setNames(c("diff", "pct", "fct")) %>% format(scientific = F, digits = 2)
  return(ret)
}

# GGSAVE wrapper
# Enhanced ggs() function to include dynamic folder saving
# Helper function to generate filenames based on the naming convention
generate_filename <- function(category, subset, demographics = NULL, charttype, additionalinfo = NULL, extension = ".jpg") {
  components <- c(category, subset, demographics, charttype, additionalinfo)
  # Remove NULL or empty components
  components <- components[!sapply(components, is.null)]
  components <- components[components != ""]
  # Concatenate with underscores and convert to lowercase
  filename <- paste(tolower(components), collapse = "_")
  paste0(filename, extension)
}

# Enhanced ggs() function with optional folder support
ggs <- function(
    plot, 
    category, 
    subset, 
    demographics = NULL, 
    charttype, 
    additionalinfo = NULL, 
    folder = NULL,          # Set default to NULL
    width = 250, 
    height = 150, 
    extension = ".jpg",
    units = "mm", 
    ...                     # Additional arguments for ggsave
) { 
  # Validate that 'plot' is a ggplot object
  if(!inherits(plot, "ggplot")) {
    stop("Error: The provided plot is not a ggplot object.")
  }
  
  # Generate filename based on the naming convention
  filename <- generate_filename(category, subset, demographics, charttype, additionalinfo,extension)
  
  # Determine the full file path
  if(!is.null(folder)) {
    filepath <- file.path(folder, filename)
    
    # Check if the folder exists; if not, create it (including parent directories)
    if(!dir.exists(folder)) { 
      dir.create(folder, recursive = TRUE) 
      message(paste("Directory created:", folder))
    }
  } else {
    # If folder is NULL, save in the current working directory
    filepath <- filename
  }
  
  # Save the plot using ggsave with error handling
  tryCatch(
    {
      ggsave(
        filename = filepath, 
        plot = plot, 
        width = width, 
        height = height, 
        units = units, 
        ...
      )
      message(paste("Plot saved successfully as", filepath))
    },
    error = function(e) {
      message(paste("Error saving plot:", e$message))
    }
  )
}



#make displaced persons
make_dp <- function(df){
  df %>% mutate(across(c(refugees,
                         asylum_seekers,
                         stateless,
                         oip),~as.numeric(.) %>% replace_na(0)),
                dp = refugees+asylum_seekers+oip)
}



label_maxunit <- function(digits = 2, abbr = T) {
  function(x) {
    # Identify the maximum break value
    max_break <- max(x, na.rm = TRUE)
    
    # Determine the appropriate unit and divisor
    if (max_break >= 1e6) {
      unit <- "M"
      if (abbr == F) unit <- "<br>Million"
      divisor <- 1e6
    } else if (max_break >= 1e3) {
      unit <- "K"
      if (abbr == F) unit <- "<br>Thousand"
      divisor <- 1e3
    } else {
      unit <- ""
      divisor <- 1
    }
    
    # Divide all labels by the divisor and format them
    formatted_labels <- signif(x / divisor, digits)
    formatted_labels <- as.character(formatted_labels)
    
    # Append the unit modifier only to the largest label
    formatted_labels[x == max_break] <- paste0(formatted_labels[x == max_break], unit)
    
    return(formatted_labels)
  }
}
