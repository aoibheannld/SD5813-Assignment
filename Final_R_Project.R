install.packages("tidyverse")
library(tidyverse)
study_dta_raw <- read_csv("C:/Users/Aoibheann/Desktop/Adv_Vis_data.csv")
head(study_dta_raw)
str(study_dta_raw)
study_dta_removed_cols <- study_dta_raw %>%
  select(-(5:34))
str(study_dta_removed_cols)
blank_cols <- study_dta_removed_cols %>%
  select(where(~all(is.na (.) == ".."))) %>%
  names()

study_dta_cleaned <- study_dta_removed_cols %>%
  select(!all_of(blank_cols))
str(study_dta_cleaned)
study_dta_no_missing_rows <- study_dta_cleaned %>%
  drop_na()
nrow(study_dta_no_missing_rows)


study_dta_imputed_mean <- study_dta_cleaned %>%
  
  
  mutate("1990 [YR1990]" = ifelse(is.na("1990 [YR1990]"),
                                  mean( "1990 [YR1990]", na.rm = TRUE),
                                  "1990 [YR1990]"))

sum(is.na(study_dta_imputed_mean $"1990 [YR1990]"))

calculate_mode <- function(x) {uniqx <- unique(na.omit(x))
uniqx[which.max(tabulate(match(x, uniqx)))]
}

study_dta_imputed_mode <- study_dta_cleaned %>%
  mutate("Country Name" = ifelse(is.na("Country Name"),
                                 calculate_mode("Country Name"),
                                 "Country Name"))


sum(is.na(study_dta_imputed_mode$ "Country Name"))

study_dta_cleaned_missing_handled <- study_dta_cleaned

central_american_countries <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama")

study_dta_central_america <- study_dta_cleaned_missing_handled %>%
  filter(`Country Name` %in% central_american_countries)


head(study_dta_central_america)

saveRDS(study_dta_central_america, "study_dta.rds")

#Script for Graph 1

study_dta_central_america%>%
  select(-(38))

nicaragua_honduras_data <- study_dta_central_america %>%
  select(-38) %>%
  filter(`Country Name` %in% c("Nicaragua", "Honduras")) %>%
  mutate(
    `1990 [YR1990]` = as.numeric(`1990 [YR1990]`),
    `2020 [YR2020]` = as.numeric(`2020 [YR2020]`)
  ) %>%
  select(`Country Name`, `1990 [YR1990]`, `2020 [YR2020]`) %>%
  pivot_longer(
    cols = c(`1990 [YR1990]`, `2020 [YR2020]`),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Year = case_when(
      Year == "1990 [YR1990]" ~ 1990,
      Year == "2020 [YR2020]" ~ 2020
    )
  )

# Plot the data
ggplot(nicaragua_honduras_data, aes(x = Year, y = Value, color = `Country Name`)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Percentage Change from 1990 to 2020",
    x = "Year",
    y = "Percentage",
    color = "Country"
  ) +
  scale_x_continuous(breaks = c(1990, 2020)) +
  theme_minimal() +
  theme(legend.position = "bottom")

#Script for Graph 2
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)

central_american_countries <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama")

central_america_map <- ne_countries(country = "central_american_countries", 
                                    scale = "medium", 
                                    returnclass = "sf")
  
Forest_area_2021 <- study_dta_cleaned_missing_handled %>%
    filter(`Country Name` %in% central_american_countries) %>%
    select(`Country Name`, `2021 [YR2021]`)
  
central_america_map <- left_join(central_america_map, forest_area_2021, by = c("name" = "Country Name"))

ggplot(central_america_map) +
    +     geom_sf(aes(fill = `2021 [YR2021]`), color = "black") +
    +     scale_fill_gradient(
      +         name = "Forest Area (%)",
      +         low = "#ffe6f0",
      +         high = "#cc0066",
      +         na.value = "lightgray"
) +
    +     labs(
      +         title = "Forest Area Percentage in Central American Countries (2021)",
      +         caption = "Source: Your Data Source") +
    +     theme_minimal() +
    +     theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  
  
 # Graph_3_Script
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  
forest_area_data <- study_dta_central_america %>%
    filter(`Country Name` %in% c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama")) %>%
    select(`Country Name`, `1990 [YR1990]`, `2000 [YR2000]`, `2010 [YR2010]`, `2020 [YR2020]`)
  
forest_area_data_clean <- forest_area_data %>%
    +     mutate(across(c(`1990 [YR1990]`, `2000 [YR2000]`, `2010 [YR2010]`, `2020 [YR2020]`), as.numeric))
forest_area_long <- forest_area_data_clean %>%
    +     pivot_longer(
      +         cols = c(`1990 [YR1990]`, `2000 [YR2000]`, `2010 [YR2010]`, `2020 [YR2020]`),
      +         names_to = "Year",
      +         values_to = "ForestArea") %>%
    +     mutate(
      +         Year = as.numeric(gsub(" \\[YR|\\]", "", Year)))
 View(forest_area_long)
  
heatmap_gg <- ggplot(forest_area_long, aes(x = Year, y = `Country Name`, fill = ForestArea)) +
    +     geom_tile() +
    +     scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "Forest Area (%)") +
    +     labs(
      +         title = "Forest Area Coverage in Central America (1990, 2000, 2010, 2020)",
      +         x = "Year",
      +         y = "Country") +
    +     theme_minimal() +
    +     theme(
      +         axis.text.x = element_text(angle = 45, hjust = 1),
      +         legend.position = "bottom")
 
heatmap_plotly <- ggplotly(heatmap_gg)
htmlwidgets::saveWidget(heatmap_plotly, "forest_area_heatmap.html")
  
browseURL("forest_area_heatmap.html")
