library(tidyverse)
library(sf)
library(rnaturalearth)

# Load in eBird data
full <- read_csv("full_simpcw_final_all.csv")

# Load in protected areas 
protected_areas <- st_read("Outputs/cropped.gpkg")
protected_areas <- st_make_valid(protected_areas)

# Load Simpcw territory .kml in
data_sf <- st_read("Spatial Data/doc.kml")

# Transform into sf data
full_sf <- full %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(data_sf))  

# Get species within Simpcw
full_sf <- full_sf %>%
  mutate(in_territory = st_within(geometry, data_sf, sparse = FALSE)[, 1]) %>%
  filter(in_territory) 

# Match CRS of full_sf and protected_areas to data_sf
full_sf_test <- st_transform(full_sf, st_crs(data_sf))
protected_areas <- st_transform(protected_areas, st_crs(data_sf))

# Add column for checklists within protected areas (TRUE/FALSE)
full_sf <- full_sf %>%
  mutate(in_protected_area = lengths(st_within(geometry, protected_areas)) > 0)

# Remove species that had very little data
remove_species <- c(
  "Black-chinned Hummingbird",
  "Red-breasted Sapsucker",
  "Northern House Wren",
  "Western Meadowlark",
  "Rock Wren",
  "Golden Eagle",
  "Northern Pygmy-Owl",
  "Great Horned Owl"
)

observed_sf <- full_sf %>%
  filter(species_observed == TRUE) %>% 
  filter(!common_name %in% remove_species)

## Mapping Observations
# Wrap long species names for facet labels
observed_sf <- observed_sf %>%
  mutate(common_name_wrapped = case_when(
    common_name == "Black-capped Chickadee" ~ "Black-capped \nChickadee",
    common_name == "Common Raven" ~ "Common \nRaven",
    common_name == "Mountain Chickadee" ~ "Mountain \nChickadee",
    common_name == "Pileated Woodpecker" ~ "Pileated \nWoodpecker",
    common_name == "Red-naped Sapsucker" ~ "Red-naped \nSapsucker",
    common_name == "Rufous Hummingbird" ~ "Rufous \nHummingbird",
    TRUE ~ common_name
  ))

# Plot
ggplot(data = observed_sf) +
  geom_sf(data = data_sf, fill = "grey90", color = "black", inherit.aes = FALSE) +
  geom_sf(data = protected_areas, aes(fill = "Protected Area"), color = NA, alpha = 0.3, inherit.aes = FALSE) +
  geom_sf(aes(color = factor(in_protected_area)), size = 1) +
  facet_wrap(~ common_name_wrapped, ncol = 5) +
  scale_color_manual(
    values = c("TRUE" = "#00bfc4", "FALSE" = "#f8766d"),
    labels = c("TRUE" = "Inside Protected Area", "FALSE" = "Outside Protected Area"),
    name = NULL
  ) +
  scale_fill_manual(
    values = c("Protected Area" = "darkgreen"),
    name = NULL
  ) +
  scale_x_continuous(breaks = seq(-120, -115, by = 2)) +
  scale_y_continuous(breaks = seq(51, 54, by = 1)) +   # Adjust as needed
  labs(
    title = "Checklists with Species Presence in Simpcw Territory (2010–2025)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()
