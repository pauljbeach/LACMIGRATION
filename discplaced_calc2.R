
make_fdp <- function(df,idmc = F) {
  
  if(idmc == F){
  df %>% mutate(across(c(refugees,
                         asylum_seekers,
                         stateless,
                         oip,
                         ooc),~as.numeric(.) %>% replace_na(0)),
                fdp = refugees+asylum_seekers+oip+idps+pal_ref
  
                #+ooc+stateless 
      )
  } else {
    df %>% mutate(across(c(refugees,
                           asylum_seekers,
                           stateless,
                           oip,
                           ooc),~as.numeric(.) %>% replace_na(0)),
                  fdp = refugees + asylum_seekers + oip +idp_idmc+pal_ref
                  #+ooc+stateless
    )
  }
}

make_dp <- function(df) {
  df %>% mutate(across(c(refugees,
                         asylum_seekers,
                         stateless,
                         oip),~as.numeric(.) %>% replace_na(0)),
                dp = refugees+asylum_seekers+oip+pal_ref
  )
}


population_raw <- read_csv(here("Population Data", "UNHCR", "population.csv"),skip = 14) %>% clean_names()

idmc_raw <- read_csv("idmc.csv",skip = 14) %>% clean_names()
unwra_raw <- read_csv("unrwa.csv",skip = 14) %>% clean_names()

idmc <- idmc_raw %>% 
  rename(coo_name = country_of_origin,
         coa_name = country_of_asylum,
         coo_iso  = country_of_origin_iso,
         coa_iso  = country_of_asylum_iso,
         idp_idmc = total) %>% 
  mutate(coa_iso  = coo_iso,
         coa_name = coo_name) %>% 
  arrange(year)
idmc %>% 
  filter(coo_iso == "PSE")

unrwa <- unwra_raw %>% 
  rename(coo_name = country_of_origin,
         coa_name = country_of_asylum,
         coo_iso = country_of_origin_iso,
         coa_iso = country_of_asylum_iso,
         pal_ref = total) %>% 
  mutate(coa_iso = coo_iso,
         coa_name = coo_name) %>% 
  arrange(year) 
ru <- population_raw


wh_iso <- refugees::countries %>% filter(unhcr_region == "The Americas") %>% pull(iso_code)
wh_name <- refugees::countries %>% filter(unhcr_region == "The Americas") %>% pull(name)
lac_iso <- refugees::countries %>% filter(unsd_subregion == "Latin America and the Caribbean") %>% pull(iso_code)





### COA

hcr <- ru %>%
  rename(coo_name = country_of_origin,
         coa_name = country_of_asylum,
         coa_iso = country_of_asylum_iso,
         coo_iso = country_of_origin_iso,
         refugees = refugees_under_unhc_rs_mandate,
         idps = id_ps_of_concern_to_unhcr,
         oip = other_people_in_need_of_international_protection,
         ooc = others_of_concern,
         hst = host_community,
         stateless = stateless_persons) %>% 
  mutate(across(c(oip,hst), ~str_remove(.,"-") %>% 
                  parse_number() %>% 
                  replace_na(0))) %>% 
  full_join(idmc) %>% 
  full_join(unrwa) %>%
  filter(year >= 2000) %>% 
  mutate(across(c(idp_idmc,
                  pal_ref),
                ~replace_na(.,0)),
         across(idp_idmc, ~ifelse(coo_iso == "PSE" & coa_iso == "PSE", .*.3,.))) %>% 
  make_dp() %>% 
  make_fdp(idmc = T)


hcr %>% 
  filter(year == 2023) %>% 
  filter(
    #coo_iso %in% "VEN",
    coa_iso %in% lac_iso,
    #coo_iso == "VEN"
    ) %>% 
  mutate(fdps = fdp +ooc+stateless) %>% 
  summarise(across(c(dp,fdp,fdps,idps,idp_idmc,refugees,asylum_seekers,ooc,oip,stateless,pal_ref),~sum(.,na.rm = T)),
            #.by = coa_iso
            ) %>% 
  arrange(desc(fdp))



hcr %>% 
  filter(year %in% c(2022,2023),
         coo_iso %in% lac_iso,
         coa_iso %in% c(lac_iso,"USA")) %>% 
  mutate(across(coa_iso, ~ifelse(.== "USA",.,"LAC"))) %>% 
  summarise(across(dp,sum),.by = c(year,coa_iso))
  pivot_wider(names_from = year,values_from = dp) %>% 
  rename_with(~c("coa_iso","y1","y2")) %>% 
  mutate(diff = y2-y1) 
  select(coa_iso,diff) %>% 
  pivot_wider(names_from = coa_iso,
              values_from = diff) %>% 
  mutate(diff = LAC/USA) %>% 

6957960/1969961

library(tidymodels)           
app_rate <- pdf_text("Department of Homeland Security Border Security Metrics Report_ 2022.pdf") %>% 
  .[1] %>% 
  str_split("\n") %>% 
  pluck(1) %>% 
  .[-c(1:4)] %>% 
  .[-c(29:49)] %>% 
  .[-c(1:6)] %>% 
  trimws() %>% 
  str_replace_all("[[:blank:]]+", ",") %>% 
  str_split_fixed(",",6) %>% 
  as_tibble() %>% 
  select(year = V1,
         app_rate = V6) %>% 
  mutate(across(everything(),as.numeric))

#make full monthly ts
fmts <- month_swbf %>% 
  bind_rows(
    ne %>% 
      filter(
        component == "U.S. Border Patrol"
      ) %>% 
      summarise(across(encounter_count,sum),.by = date) %>% 
      filter(date > max(month_swbf$date))
  ) %>% 
  arrange(desc(date))

monthly_dates <- seq.Date(from = min(fmts$date), to = max(fmts$date), by = "month")

# Create a new data frame for monthly interpolation
monthly_data <- tibble(
  date = monthly_dates,
  app_rate = spline(app_rate %>% select(app_rate), method="natural",n = length(monthly_dates))$y
)

ggplot(monthly_data, aes(x= date, y = app_rate))+
  geom_line()+
  geom_point()

app_rate %>% 
  ggplot(aes(x = year, y = app_rate))+
  geom_line()

neay <- ne_alt %>% 
  mutate()
  summarise(across())
fmts %>% 
  select(date,apprehensions = encounter_count) %>% 
  left_join(monthly_data) %>% 
  mutate(across(app_rate, ~./1e2),
         `undetected entries (estimated)` = apprehensions/app_rate-apprehensions) %>%
 
  pivot_longer(c(apprehensions, `undetected entries (estimated)`),
               values_to = "entry_count",
               names_to = "cat") %>%
  mutate(fiscal_year = fiscal_year(date)) %>% 
  filter(fiscal_year < 2024) %>% 
  summarise(across(entry_count,sum),.by = c(fiscal_year,cat)) %>% 
  mutate(across(cat,fct_rev)) %>%
  ggplot(aes(x = fiscal_year, y = entry_count))+
  geom_col(aes(fill = cat))+
  theme_usaid_blue(bl = F, base_size = 18, rf = F)+
  geom_rangeframe(position = "stack")+
  scale_y_continuous(labels = ~./1e6)+
  labs(title = "U.S. Southwest Border Estimated Crossings <span style = 'color:red'>[Internal USAID only]</span>",
       #subtitle = "Est. Crossings Defined as USBP Apprehensions + Est. Undetected Entries<sup>1</sup>",
       y = "Millions of Crossings",
       x = "Fiscal Year")+
  scale_x_continuous(breaks = seq(2001,2023,2))+
  geom_text_repel(data = . %>% filter(fiscal_year == max(fiscal_year)),
                  position = position_stacknudge(x = 5,vjust = .5),
                  aes(label = str_to_title(cat) %>% str_wrap(width = 10),
                      color = cat),
                  family = "Cabin",
                  hjust = 0,
                  segment.linetype = "dotted",
                  segment.color = "grey",
                  size = 5.5,
                  fontface= "bold")+
  theme(plot.title.position = "plot")+
  scale_fill_metro(aesthetics = c("fill","color"))+
  coord_cartesian(xlim = c(1999.5,2030),
                  ylim = c(0,4.1e6),
                  expand = F)+
  guides(color = "none",fill = "none",
         x = guide_axis(minor.ticks = T,cap = "both",))+
  # geom_text(aes(label = round(entry_count/1e6,1)),
  #           data = . %>% summarise(across(entry_count,sum), .by = fiscal_year),
  #           vjust = -.2,
  #           family = "Cabin",
  #           fontface = "bold")+
  labs(caption = "Source: DHS/CBP USBP Apprehentions and Reported Apprehention Rate <br>
       Est. Crossings Defined as USBP Apprehensions + Est. Undetected Entries")
ggs()  

unde  
  
