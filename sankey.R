library(librarian)
shelf(tidyverse,ggsankeyfier,ggtext,ggthemes,paletteer,population,refugees)

source("usaidtheme.R")



pos <- position_sankey(v_space = "auto", order = "as_is",direction = "backward")
lac_iso <- refugees::countries %>% filter(unsd_subregion == "Latin America and the Caribbean") %>% 
  pull(iso_code)

wh_iso <- refugees::countries %>% filter(unhcr_region == "The Americas") %>% 
  pull(iso_code)

hcr2 <- refugees::population %>% 
  filter(year == 2024,
         coo_iso %in% "VEN",
         coa_iso %in% wh_iso) %>% 
  mutate(across(c(coo_name,coa_name),
                ~str_remove_all(.,"\\s*\\([^\\)]+\\)") %>%
                  toupper() %>% 
                  str_replace_all(" OF AMERICA", ""))
  ) %>% 
  mutate(across(where(is.numeric), ~replace_na(.,0)),
         dp = oip+refugees+asylum_seekers) %>% 
  mutate(across(c(coo_name,coa_name),
                ~ifelse(. %in% nca,"NCA",.))) %>% 
  summarise(across(dp,sum),
            .by = c(coo_name,
                    coa_name,
                    year))

make_msankey <- function(x = c("COLOMBIA", "PERU",'ECUADOR',"UNITED STATES"),
                         reg0 = c(2e6,6e5,3e5, 1e5)){
  
  reg <- c(reg0,5e4)
  init1 <- map_df(x,
                  
                  ~hcr2 %>%
                    filter(coa_name == .x,
                           
                           #coo_name == "VENEZUELA"
                    ) %>%
                    summarise(across(dp,sum),.by = coa_name)
  ) %>%
    bind_rows(
      hcr2 %>%
        filter(!coa_name %in% x,
               #coo_name == "VENEZUELA"
        ) %>%
        mutate(coa_name = "OTHER") %>%
        summarise(across(dp,sum),.by = coa_name)
    )
  
  init3 <- init1 %>% 
    mutate(reg = reg,
           dp = dp-reg) %>% 
    .[c(1:(nrow(init1)-2),
        nrow(init1),
        nrow(init1)-1),] %>% 
    pivot_longer(-coa_name,values_to = "dp") %>% 
    arrange(name)
  
  init2 <- init1 %>%
    mutate(loc = 2:(length(x)+2)) %>%
    pivot_wider(names_from = loc,values_from = coa_name,names_prefix = "coa") %>%
    select(contains("coa")) %>%
    replace(., col(.) > row(.), "transparent") %>%
    replace(.,col(.) < row(.), "OTHER")%>% 
    .[c(1:(nrow(init1)-2),
        nrow(init1),
        nrow(init1)-1),] %>% 
    #double it
    bind_rows(
      init1%>%
        mutate(loc = 2:(length(x) + 2)) %>%
        pivot_wider(names_from = loc,values_from = coa_name,names_prefix = "coa") %>%
        select(contains("coa")) %>%
        replace(., col(.) > row(.), "transparent") %>%
        replace(.,col(.) < row(.), "OTHER") %>% 
        .[c(1:(nrow(init1)-2),
            nrow(init1),
            nrow(init1)-1),],.
    ) %>% 
    select(-contains(as.character(length(x) + 2))) %>%
    mutate(coa1 = "LAC") %>%
    bind_cols(init3)
  
  stage_names <- paste0("coa", 1:(length(x) + 1))
  
  init2 %>%    
    #keep if no alpha
    summarise(across(dp,sum),.by = -c(name,dp)) %>% 
    filter(dp > 0) %>% 
    ggsankeyfier::pivot_stages_longer(stages_from = stage_names,
                                      values_from = "dp",
                                      additional_aes_from = c("coa1"
                                                              # "name"
                                      )) %>% 
    mutate(dp2 = sum(dp[connector == "to"]),.by = node)
}



#####v2#######
hcr2 %>% 
  mutate(coa_name = ifelse(coa_name %in% c("COLOMBIA", "PERU","BRAZIL",'ECUADOR',"CHILE",
                                           "UNITED STATES"), coa_name,"OTHER")) %>% 
  summarise(across(dp,sum),.by = coa_name)


sankey_plot <- make_msankey(x = c("COLOMBIA", "PERU","BRAZIL",'ECUADOR',"CHILE",
                   "UNITED STATES"),
             reg0 = c(2e6,6.52e5,4.5e5,2.1e5,1e5,0)) %>% 
  mutate(across(node, ~fct_relevel(.,"transparent",after = 0) %>% 
                  fct_relevel("OTHER", after = Inf) %>%  
                  fct_relevel("UNITED STATES", after = Inf)
  )) %>% 
  ggplot(aes(x = stage, y = dp, group = node,
             connector = connector, edge_id = edge_id,
             #color = node
  ))+
  geom_sankeyedge(ncp = 1000,aes(fill = node
                                 #alpha = name
  ),
  slope = .2,
  #color = "black",
  position = pos)+
  geom_sankeynode(aes(fill = node),
                  #fill = "black",
                  #color = "black",
                  position = pos,
                  #data = . %>% filter(stage == "coa_name")
  )+
  scale_alpha_manual(breaks = c("dp","reg"),
                     values = c(.5,1),
                     labels = c("REGULAR","IRREGULAR") %>% rev())+
  guides(color = 'none', fill = "none")+
  theme_void()+
  labs(title = "Countries Hosting Forcibly Displaced Venezuelans" %>% 
         toupper(),
       subtitle = "From Latin America | 2024" %>% toupper(),
  )+
  theme(axis.text.x = element_markdown(size = 18,
                                       family = "Gill Sans"),
        plot.title = element_textbox_simple(margin = margin(5,5,2,10),
                                            family = "Gill Sans",
                                            face = "bold",
                                            size = 18,
                                            hjust = 0,
                                            color = "#002F6C"),
        plot.subtitle = element_markdown(margin = margin(5,5,15,10),
                                         family = "Gill Sans",
                                         face = "bold",
                                         size = 18,
                                         hjust = 0),
        plot.margin = margin(10,0,0,0))+
  geom_text(aes(label = ifelse(dp2>1e6,
                               paste0(node,"\n","(",round(dp2/1e6,1), "M",")"),
                               paste0(node,"\n","(",round(dp2/1e3,0), "K",")")
  ),
  color = node),
  stat = "sankeynode",
  position = position_sankey(v_space = "auto",
                             order = "as_is", nudge_x = 0.1),
  hjust = 0,
  
  size = 3.5,
  data = . %>% filter(stage != "coa1"),
  family = "Gill Sans",
  fontface = "bold",
  )+
  geom_text(aes(label = paste0('WESTERN\nHEMISPHERE\n(',round(sum(dp)/1e6,1),"M)"),
                color = node),
            stat = "sankeynode",
            position = position_sankey(v_space = "auto", order = "as_is", nudge_x = -.7),
            hjust = .5,
            size = 4.5,
            data = . %>% filter(stage == "coa1"),
            family = "Gill Sans",
            fontface = "bold",
  )+
  geom_text(aes(label = paste0('OTHER (',
                               #round(dp[stage == "coa7"]/1e3,0),
                               "647",
                               "K)"),
                color = node),
            stat = "sankeynode",
            position = position_sankey(v_space = "auto", order = "as_is", nudge_x = .1),
            hjust = 0,
            size = 3.5,
            data = . %>% filter(stage == "coa7"),
            color = c("transparent","grey30", "transparent"),
            family = "Gill Sans",
            fontface = "bold"
  )+
  scale_x_discrete(position = "bottom",
                   breaks = c(3,2),
                   labels = c("COUNTRY OF <br> ORIGIN",
                              "COUNTRY OF <br> ASYLUM"))+
  
  scale_fill_manual(aesthetics = c("fill"),
                    values = c("VENEZUELA" = "#AF142B",
                               "COLOMBIA" = "#0072CE",
                               "transparent" = "transparent",
                               "UNITED STATES" = "#002F6C",
                               "PERU" = "#F1002E",
                               "CHILE" = "purple4",
                               "BRAZIL" = "#009739",
                               "ECUADOR" = "#FFD100",
                               "NCA" = "lightblue",
                               "MEXICO" = "#006341",
                               "OTHER" = "grey40"))+
  scale_color_manual(aesthetics = c("color"),
                     values = c("VENEZUELA" = "#AF142B",
                                "COLOMBIA" = "#0072CE",
                                "transparent" = "transparent",
                                "UNITED STATES" = "#002F6C",
                                "CHILE" = "purple4",
                                "PERU" = "#F1002E",
                                "BRAZIL" = "#009739",
                                "NCA" = "lightblue",
                                "ECUADOR" = "#FFD001",
                                "MEXICO" = "#006341",
                                "OTHER" ="transparent"))+
  coord_cartesian(xlim = c(.2,9.5))+
  guides(alpha = guide_legend(reverse = T,byrow = T))+
  labs(alpha = NULL)+
  theme(legend.position = c(.7,.2),
        plot.caption = element_markdown(hjust = .1),
        legend.key.size = unit(15, "mm"),
        legend.spacing.y = unit(-1.2, "lines"),
        legend.text = element_markdown(family = "Gill Sans",
                                       face = "bold",
                                       size = 14))+
  labs(caption = "Data from UNCHCR (Update 2024.6) | Chart Created by USAID/LAC")
ggs(
  plot = sankey_plot,
  category = "displacement",
  subset = NULL,
  demographics = "coaven",
  charttype = "sankey",
  additionalinfo = NULL,
  folder = "mar_charts",
  width = 280,
  height = 170
)


######### v1 #######
hcr %>% 
  filter(year == 2023,
         coo_iso %in% lac_iso,
         coa_iso %in% wh_iso) %>% 
  mutate(across(coo_name, ~ifelse(coo_iso == "VEN",.,"OTHER")),
         across(coa_name,~ifelse(coa_iso %in% c("COL",
                                                "PER","COL","ECU", "USA", "MEX", "BRA"
         ),
         .,
         "OTHER")),
         across(c(coo_name,coa_name),
                ~str_remove_all(.,"\\s*\\([^\\)]+\\)") %>%
                  toupper() %>% 
                  str_replace_all(" OF AMERICA", ""))) %>% 
  summarise(across(dp,sum),.by = c(coa_name,coo_name)) %>% 
  mutate(dpa = sum(dp),.by = coa_name) %>% 
  mutate(dpo = sum(dp),.by = coo_name) %>% 
  ggsankeyfier::pivot_stages_longer(stages_from = c("coo_name","coa_name"),
                                    values_from = "dp",
                                    additional_aes_from = c("coo_name",
                                                            "dpa",
                                                            "dpo")
  ) %>% 
  filter(dp>=1e5) %>%
  mutate(across(node,
                ~fct_reorder(.,dpa) %>% 
                  fct_relevel("UNITED STATES",after = 0))
  ) %>%
  
  
  
  ggplot(aes(x = stage, y = dp, group = node,
             connector = connector, edge_id = edge_id,
             #color = node
  ))+
  geom_sankeyedge(aes(fill = coo_name),
                  color = "black",
                  position = pos)+
  geom_sankeynode(aes(fill = node),
                  #fill = "black",
                  #color = "black",
                  position = pos,
                  data = . %>% filter(stage == "coa_name"))+
  geom_sankeynode(aes(fill = node),
                  #color = "black",
                  position = pos,
                  data = . %>% filter(stage == "coo_name"))+
  geom_text(aes(label = ifelse(dpa>1e6 & connector == "to",
                               paste0(node,"\n","(",round(dpa/1e6,1), "M",")"),
                               paste0("(",node,"\n","(",round(dpa/1e3,0), "K",")")
  ),
  color = node),
  stat = "sankeynode",
  position = position_sankey(v_space = "auto", order = "ascending", nudge_x = 0.1),
  hjust = 0,
  size = 4,
  family = "Gill Sans",
  fontface = "bold",
  data = . %>% filter(stage == "coa_name")
  ) +
  geom_text(aes(label = paste0(node, "\n(", round(dpo / 1e6, 1), "M)"),
                color = node), 
            stat = "sankeynode",
            position = position_sankey(v_space = "auto", 
                                       order = "ascending", 
                                       nudge_x = -0.1),
            hjust = 1,
            size = 4,
            family = "Gill Sans",
            fontface = "bold",
            data = . %>% filter(stage == "coo_name")
  )+
  guides(color = 'none', fill = "none")+
  theme_void()+
  labs(title = "FORCED DISPLACEMENT IN THE AMERICAS",
       subtitle = "Across International Borders | 2023")+
  theme(axis.text.x = element_markdown(size = 18,
                                       family = "Gill Sans"),
        plot.title = element_markdown(margin = margin(5,5,2,5),
                                      family = "Gill Sans",
                                      face = "bold",
                                      size = 22,
                                      hjust = .5,
                                      color = "#002F6C"),
        plot.subtitle = element_markdown(margin = margin(5,5,15,5),
                                         family = "Gill Sans",
                                         face = "bold",
                                         size = 18,
                                         hjust = .1),
        plot.margin = margin(10,0,0,0))+
  scale_x_discrete(position = "bottom",
                   
                   labels = c("COUNTRY OF <br> ORIGIN",
                              "COUNTRY OF <br> ASYLUM"))+
  scale_fill_manual(aesthetics = c("color","fill"),
                    values = c("VENEZUELA" = "#AF142B",
                               "COLOMBIA" = "#0072CE",
                               "UNITED STATES" = "#002F6C",
                               "PERU" = "#F1002E",
                               "BRAZIL" = "#009739",
                               "ECUADOR" = "#FFD100",
                               "MEXICO" = "#006341",
                               "OTHER" = "grey40"))+
  
  coord_cartesian(expand = T)



