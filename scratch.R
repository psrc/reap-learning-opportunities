# http://aws-linux/mediawiki/index.php/ACS_Variables_2023#B03002

library(psrccensus)
library(tidycensus)
library(psrcelmer)
library(sf)
library(tidyverse)
library(psrcplot)

# Displacement Risk table from PSRC Portal
disp_risk <- "https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Displacement_Risk_Data/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

dr_tbl <- st_read(disp_risk) |> 
  select(geoid10, risk_level, risk_level_name) |> 
  st_drop_geometry()

# Elmer B03002 Hispanic or Latino Origin by Race 5 year ACS
sql_query <- "select * from census.census_table(2023,'Tract','ACS5','B03002')"
# sql_query <- "select * from census.census_table(2020,'Tract','ACS5','B03002')"

crosswalk_10_20 <- get_table(schema = "census",
                             tbl_name = "v_geo_relationships_tracts")

d <- get_query(sql_query, "Elmer")

est <- d |> 
  filter(variable_name %in% c("B03002_001", "B03002_012", "B03002_003", "B03002_004", "B03002_006")) |> 
  select("variable_description", "geoid", "estimate")

est_join <- left_join(est, crosswalk_10_20, by = c("geoid"="geoid20"))

sum_df <- est_join |> 
  group_by(variable_description, geoid10) |> 
  summarize(estimate = sum(estimate))

df <- left_join(sum_df, dr_tbl, by = "geoid10") |> 
  group_by(variable_description, risk_level, risk_level_name) |> 
  summarize(estimate = sum(estimate)) |> 
  na.omit() 

denom <- df |> 
  group_by(variable_description) |> 
  summarize(denom = sum(estimate))

df2 <- left_join(df, denom, by = "variable_description")

df3 <- df2 |> 
  mutate(share = estimate/denom) |> 
  filter(variable_description != "Total:") |> 
  mutate(label = case_when(variable_description == "Asian alone" ~ "Asian",
                           variable_description == "Black or African American alone" ~ "Black",
                           variable_description == "Hispanic or Latino:" ~ "Hispanic/Latinx",
                           variable_description == "White alone" ~ "White",))

df4 <- df3 |> 
  filter(risk_level_name != "lower") |> 
  mutate(risk_level_name_label = str_to_title(risk_level_name)) |> 
  mutate(risk_level_name_label = factor(risk_level_name_label, levels = c("Moderate", "Higher"))) |>
  arrange(risk_level_name_label)
  
p <- ggplot(df4, aes(x = label, y = share, fill = risk_level_name_label)) +
  geom_col() +
  geom_text(aes(label = paste0(round(share*100, 0), "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_discrete(palette = psrc_colors$oranges_inc) +
  labs(x = NULL,
       y = NULL,
       title = "Population in Areas of Moderate and Higher Displacement Risk",
       caption = "Source: U.S. Census Bureau, American Community Survey 5-Year Estimates, 2019-2023") +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(hjust = .5),
        axis.text.x = element_text(margin = margin(b = 10)),
        plot.margin = margin(t = 10, r = 10, b = 35, l = 10) 
        )

p
