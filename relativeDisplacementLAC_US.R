######
# Relative Displacement Flows in The Americas
# Description: Calculate the relative displacement flows in The Americas between the LAC region and the U.S
# Paul Beach
# pbeach@usaid.gov
######


# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(readxl)
library(ggrepel)
library(scales)
library(janitor)
library(ggthemes)

# The following are very useful packages for reference to pull data directly but may not be approved for GFE
# all data from linked sheet is already in the repo here
  #library(idmc)
  #library(refugees)


# Load Data ---------------------------------------------------------------
# IDMC_displacement is the raw IDMC file with stocks and flows, IDMC_stock is the UNHCR cleaned data for a simple join
# UNRWA is a separate dataset that includes palestinian refugees
sheet_names <- c("IDMC_Displacement", "UNHCR_Stock", "UNHCR_Flows", "UNRWA_Stock", "IDMC_Stock(UNHCR)", "countries")

fd <- map(sheet_names, ~ read_excel("WH_displacement.xlsx", sheet = .x)) %>%
  setNames(sheet_names) %>% 
  map(clean_names)

pop2 <- read_xlsx("IDMC_Internal_Displacement_Conflict-Violence_Disasters (2).xlsx")


# region isos for filtering countries
lac_iso <- fd$countries %>% filter(unsd_subregion == "Latin America and the Caribbean") %>% pull(iso_code)
wh_iso <- fd$countries %>% filter(unhcr_region == "The Americas") %>% pull(iso_code)
# Clean Data --------------------------------------------------------------

#combine the stock datasets and calculate forced displacement

fd_stock <- fd$UNHCR_Stock %>%
  #join the IDMC IDP dataset
  full_join(
    fd$`IDMC_Stock(UNHCR)`  %>% select(year,coa_iso,coo_iso,idmc = total)
  ) %>% 
  # join the UNRWA stock dataset
  full_join(
    fd$UNRWA_Stock %>% select(year,coa_iso,coo_iso,unrwa = total)
  ) %>% 
  #change NA to 0
  mutate(across(where(is.numeric), ~replace_na(.,0)),
         # fix the IDMC data double counting PSE IDPs and Refugees
         across(idmc, ~ifelse(coo_iso == "PSE" & coa_iso == "PSE", .*.3,.))
  ) %>% 
  # calculate forced displacement
  mutate(fdp = idmc + oip + refugees + unrwa + asylum_seekers, # forced displacement population
         fdps = idps + oip + refugees + unrwa + asylum_seekers + ooc + stateless, #UNCHR pop of concern (forcibly displaced or stateless)
         fdpa = oip + refugees + unrwa + asylum_seekers #forced displacement abroad
         ) 


#make sure total is 117m in 2023
fd_stock %>% summarise(across(fdp,sum),.by = year) %>% arrange(desc(year))



# clean the IDMC flow data and scope to LAC 
lac_disp <- fd$IDMC_Displacement %>% 
  filter(iso3 %in% c(wh_iso)) %>%
  mutate(coa_g = ifelse(iso3 %in% "USA", "USA", "LAC")) %>% 
  summarise(across(c(disaster_internal_displacements,
                     conflict_internal_displacements),\(x) sum(x,na.rm = T)),
            .by = c(year,coa_g)) %>% 
  select(year,
         coa_g,
         disaster_internal_displacements,
         conflict_internal_displacements)

# join the IDMC flow data to the UNHCR flow data and calculate forced displacement
fd_flow <- fd$UNHCR_Flows %>% 
  full_join(
    fd$IDMC_Displacement %>% 
      select(year,
             coo_iso = iso3,
             coa_iso = iso3,
             disaster_internal_displacements,
             conflict_internal_displacements)
  ) %>%
  
  mutate(across(where(is.numeric), ~replace_na(.,0)),
         # coa_g = ifelse(coa_iso %in% "USA", "USA", "LAC"),
         fdpa = oip + refugees + asylum_seekers + refugee_like,
         fdp = fdpa + conflict_internal_displacements) %>% 
  select(year, coo_iso, coa_iso, fdp, conflict_internal_displacements, fdpa) %>% 
  
  summarise(across(c(fdp, fdpa, conflict_internal_displacements), 
                   \(x) sum(x,na.rm = T)),
            .by = c(year, coa_iso, coo_iso)
  ) 
#double check the 2023 fdp total is ~27m
fd_flow %>% filter(year == 2023) %>% summarise(across(fdp,sum))


# Analysis ----------------------------------------------------------------

# stock approach to relative displacement 
stock_diff <- function(data,y1,y2, var = fdp){
fd_stock %>%
  # select WH countries of origin and LAC + USA countries of asylum
  filter(coo_iso %in% lac_iso, 
         coa_iso %in% c(lac_iso, "USA")) %>%
  # make a USA/LAC grouping
  mutate(coa_g = ifelse(coa_iso %in% "USA", "USA", "LAC")) %>%
  # calculate the sum of forced displacement abroad
  summarise(across(c({{var}}), \(x) sum(x)), .by = c(year, coa_g)) %>%
  # pick the base and current year
  filter(year %in% c(y1, y2)) %>% 
  arrange(year) %>% 
  pivot_wider(names_from = year, values_from = {{var}}) %>%
  mutate(diff = .[[3]] - .[[2]])
}

stock_diff(fd_stock, 2013, 2023, var = fdpa)
stock_diff(fd_stock, 2017, 2023, var = fdp)

# Flow approach to relative displacement

flow_diff <- function(data,y1,y2, var = fdp){
data %>% 
  filter(coo_iso %in% lac_iso,
         coa_iso %in% c(lac_iso, "USA")
         ) %>%
  # make a USA/LAC grouping
  mutate(coa_g = ifelse(coa_iso %in% "USA", "USA", "LAC")) %>%
  summarise(across(c(fdp, fdpa, conflict_internal_displacements),
                   \(x) sum(x,na.rm = T)),
            .by = c(year, coa_g)) %>% 
  arrange(desc(year)) %>% 
  filter(year >= y1, year <= y2) %>%
  summarise(across(c({{var}}),
                   \(x) sum(x, na.rm = T)),
            .by = c(coa_g))
}

flow_diff(fd_flow, 2013, 2023, var = fdpa)

# relative displacement flows to the USA from the WH region by year
map(c(2013,2018,2021),
    ~ flow_diff(fd_flow, .x, 2023, var = fdp) %>% 
  mutate(across(fdp, ~.[coa_g == "LAC"]/.)) %>% 
  filter(coa_g == "USA") %>% 
  pull(fdp)
) %>% setNames(c(2013,2018,2021))

map(c(2013,2018,2021),
    ~ flow_diff(fd_flow, .x, 2023, var = fdp) %>% 
      mutate(across(fdp, ~./sum(.))) %>% 
      filter(coa_g == "USA") %>% 
      pull(fdp)
) %>% setNames(c(2013,2018,2021))


# Plotting ----------------------------------------------------------------

fd_flow %>% 
  filter(coo_iso %in% wh_iso) %>% 
  mutate(coa_g = ifelse(coa_iso %in% "USA", "USA", "LAC")) %>%
  summarise(across(c(fdp,fdpa), sum), .by = c(coa_g,year)) %>%
  filter(year >= 2000) %>% 
    ggplot(aes(x = year, y = fdp, color = coa_g)) +
    geom_line(size = .75)+
  theme_tufte(base_size = 18,base_family = "Bembo")+
  labs(title = "Yearly Forced Displacement Flows in The Americas",
       subtitle = "Forced Displacement | By Region of Asylum",
       y = NULL,
       x = NULL,
       color = "Region of Asylum",
       caption = "Data: UNHCR, IDMC\n Total includes Includes Refugees, IDPs (conflict based), Asylum Seekers, and Others in Need of International Protection")+
    guides(color = "none")+
  scale_y_continuous(
                     breaks = seq(1e6,3e6,1e6),
                     labels = c("1","2","3m"))+
  geom_point(data = . %>% filter(year == max(year)),
             size = 2)+
  geom_text_repel(aes(label = coa_g),
                  nudge_y = 1,
                  size = 5,
                  fontface = "bold",
                  nudge_x = 1,
                  hjust = 0.5,
                   data = . %>% filter(year == max(year)))+
    geom_rangeframe(color = "black")+
    scale_x_continuous(breaks = seq(2000,2020,4) %>% c(2023),
                       minor_breaks = T)+
    theme(plot.caption = element_text(size = 8),
          plot.caption.position = "plot")+
    guides(x = guide_axis(minor.ticks = T))+
    #add minor ticks
    theme(panel.grid.minor = element_blank())
ggs(280)



