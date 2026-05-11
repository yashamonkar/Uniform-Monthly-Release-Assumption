
setwd("C:/Users/amonkar/Documents/GitHub/Uniform-Monthly-Release-Assumption")

library(readxl)
library(dplyr)
library(ggplot2)
library(maps)

plants <- read_excel("data/eia8602024/2___Plant_Y2024.xlsx", sheet = "Plant", skip = 1)
gens   <- read_excel("data/eia8602024/3_1_Generator_Y2024.xlsx", sheet = "Operable", skip = 1)

hydro <- gens %>%
  filter(grepl("Hydro", Technology, ignore.case = TRUE)) %>%
  group_by(`Utility ID`, `Plant Code`) %>%
  summarise(
    mw = sum(`Nameplate Capacity (MW)`, na.rm = TRUE),
    Technology = paste(unique(Technology), collapse = "; "),
    .groups = "drop"
  )

d <- plants %>%
  filter(State == "CA") %>%
  inner_join(hydro, by = c("Utility ID", "Plant Code"))

#Remove the Pumped Hydro Storage Facilities


ca <- map_data("state", region = "california")

ggplot(d, aes(Longitude, Latitude, size = mw)) +
  geom_polygon(data = ca, aes(long, lat, group = group), fill = "lightgray", inherit.aes = FALSE) +
  geom_point(alpha = 0.5) +
  coord_fixed(1.3) +
  labs(size = "MW") +
  theme_void()