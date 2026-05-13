# =============================================================================
# California Hydropower Plants: Run-of-River vs Storage-based
# =============================================================================
# This script does four things:
#
# 1. EIA-860 (2024): Loads plant locations + generator nameplate capacity (MW)
#    for all California hydro plants. Plots them on a CA map sized by MW.
#
# 2. ORNL EHA (2024): Loads the Existing Hydropower Assets database, which
#    classifies each US hydro plant by operating Mode (Run-of-river, Peaking,
#    Canal/Conduit, etc). Groups these modes into two buckets:
#       - Run-of-River : run-of-river, canal/conduit, reregulating
#       - Storage-based: peaking, intermediate peaking, hybrid peaking
#    Plots CA plants colored by type.
#
# 3. Joins EIA (capacity, location) with EHA (plant type) using EIA Plant Code,
#    then plots CA plants sized by MW and colored by type.
#
# 4. Pie chart of TOTAL MW capacity in each bucket (Run-of-River vs Storage).
# =============================================================================

setwd("C:/Users/amonkar/Documents/GitHub/Uniform-Monthly-Release-Assumption")

library(readxl)
library(dplyr)
library(ggplot2)
library(maps)

# Base California map outline used by all plots
ca <- map_data("state", region = "california")


# -----------------------------------------------------------------------------
# PART 1: EIA-860 data — California hydro plants, sized by capacity
# -----------------------------------------------------------------------------

# Plant file = locations; Generator file = capacity per generator unit
plants <- read_excel("data/eia8602024/2___Plant_Y2024.xlsx", sheet = "Plant", skip = 1)
gens   <- read_excel("data/eia8602024/3_1_Generator_Y2024.xlsx", sheet = "Operable", skip = 1)

# Keep only hydro generators, then sum MW across generators within each plant
hydro <- gens %>%
  filter(grepl("Hydro", Technology, ignore.case = TRUE)) %>%
  group_by(`Utility ID`, `Plant Code`) %>%
  summarise(
    mw = sum(`Nameplate Capacity (MW)`, na.rm = TRUE),
    Technology = paste(unique(Technology), collapse = "; "),
    .groups = "drop"
  )

# Join hydro capacity onto plant locations, keep CA only
d <- plants %>%
  filter(State == "CA") %>%
  inner_join(hydro, by = c("Utility ID", "Plant Code"))

# Plot: bubble size = nameplate capacity
ggplot(d, aes(Longitude, Latitude, size = mw)) +
  geom_polygon(data = ca, aes(long, lat, group = group),
               fill = "lightgray", inherit.aes = FALSE) +
  geom_point(alpha = 0.5) +
  coord_fixed(1.3) +
  labs(size = "MW", title = "EIA-860: CA Hydro Plants") +
  theme_void()


# -----------------------------------------------------------------------------
# PART 2: ORNL EHA data — California hydro plants, colored by type
# -----------------------------------------------------------------------------

eha <- read_excel("data/ornl_dams.xlsx", sheet = "Operational")

# Filter to CA, drop missing/unknown modes, and bucket Mode into 2 types
eha_ca <- eha %>%
  filter(State == "CA", !is.na(Mode), Mode != "Unknown") %>%
  mutate(plant_type = case_when(
    Mode %in% c("Run-of-river", "Canal/Conduit", "Reregulating",
                "Run-of-river/Upstream Peaking") ~ "Run-of-River",
    Mode %in% c("Peaking", "Intermediate Peaking",
                "Run-of-river/Peaking") ~ "Storage-based",
    TRUE ~ NA_character_
  ))

# Plot: color = plant type (no MW info yet — that comes from EIA)
ggplot(eha_ca, aes(Lon, Lat, color = plant_type)) +
  geom_polygon(data = ca, aes(long, lat, group = group),
               fill = "lightgray", inherit.aes = FALSE) +
  geom_point(alpha = 0.6, size = 2) +
  coord_fixed(1.3) +
  labs(color = "Plant Type", title = "ORNL EHA: CA Hydro Plants by Type") +
  theme_void()


# -----------------------------------------------------------------------------
# PART 3: Join EIA + EHA — plot sized by MW AND colored by type
# -----------------------------------------------------------------------------

# Match plants between datasets using EIA Plant Code (EIA_PtID in EHA)
joined <- d %>%
  left_join(eha_ca %>% select(EIA_PtID, plant_type),
            by = c("Plant Code" = "EIA_PtID"))

#Assign Unknown
joined <- joined %>%
  mutate(plant_type = ifelse(is.na(plant_type), "Unknown", plant_type))

#Clean up the data to removed pumped hydro
joined <- joined %>%
  filter(!Technology %in% c("Hydroelectric Pumped Storage",
                            "Hydroelectric Pumped Storage; Conventional Hydroelectric"))

#Subset to values over 15 MW of installed capacity
joined <- joined %>% filter(mw > 50)

#Clean up the data to removed PGE
joined <- joined %>%
  filter(!`Utility Name` %in% c("Pacific Gas & Electric Co."))



ggplot(joined, aes(Longitude, Latitude, size = mw, color = plant_type)) +
  geom_polygon(data = ca, aes(long, lat, group = group),
               fill = "lightgray", inherit.aes = FALSE) +
  geom_point(alpha = 0.6) +
  coord_fixed(1.3) +
  labs(size = "MW", color = "Plant Type",
       title = "CA Hydro: Capacity (MW) and Type") +
  theme_void()

#Save the data
joined <- joined %>% arrange(desc(mw))
write.csv(joined, "data/combined_plants.csv", row.names = FALSE)

# -----------------------------------------------------------------------------
# PART 4: Read the NID data
# -----------------------------------------------------------------------------
nid <- read.csv("data/nid_dams.csv", skip = 1)
nid <- nid %>% filter(State == "California")
nid_sub <- nid %>% select(-c(Other.Names, Former.Names, NID.ID, Other.Structure.ID, Federal.ID, Designer.Names, Number.of.Associated.Structures, State.or.Federal.Agency.ID, Distance.to.Nearest.City..Miles., American.Indian.Alaska.Native.Native.Hawaiian))

