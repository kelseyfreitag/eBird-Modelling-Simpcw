library(auk)
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)
library(assertr)
library(here)

# Resolve namespace conflicts
select <- dplyr::select

# Set file locations and names for output
f_ebd <- "Outputs/Final/ebd_simpcw_final_all.txt"
f_sed <- "Outputs/Final/sed_simpcw_final_all.txt"

# Load Simpcw territory .kml in
data_sf <- st_read("Spatial Data/doc.kml")

## Sanity Check: Species Names
# Set taxonomy version (2025)
tax <- get_ebird_taxonomy(version = 2025)

# List species
species <- c(
  "American Crow", "Bald Eagle", "Black-capped Chickadee", "Common Raven",
  "Golden Eagle", "Great Horned Owl", "Mountain Chickadee", "Rufous Hummingbird",
  "Steller's Jay", "Western Meadowlark", "Common Loon", "Black-chinned Hummingbird",
  "Northern Pygmy-Owl", "Western Screech-Owl", "Northern Flicker", "Red-naped Sapsucker",
  "Pileated Woodpecker","Red-breasted Sapsucker", "Pacific Wren",
  "Marsh Wren", "Northern House Wren", "Bewick's Wren", "Rock Wren"
)

# Check for any mismatches
species[!species %in% tax$common_name]


## eBird data filtering
# Set file locations
ebd <- auk_ebd(
  file = here("Data/Raw/ebd_CA_smp_relAug-2025/ebd_CA_relAug-2025_FIXED.txt"),
  file_sampling = here("Data/Raw/ebd_CA_smp_relAug-2025/ebd_CA_relAug-2025_sampling_FIXED.txt")
)

# Run filters
ebd_filters <- ebd %>% 
  
  # Target species
  auk_species(c("American Crow", "Bald Eagle", "Black-capped Chickadee", "Common Raven", 
                "Golden Eagle", "Great Horned Owl", "Mountain Chickadee", "Rufous Hummingbird",
                "Steller's Jay", "Western Meadowlark", "Common Loon",
                "Black-chinned Hummingbird",
                "Northern Pygmy-Owl",
                "Western Screech-Owl", "Northern Flicker", "Red-naped Sapsucker","Pileated Woodpecker",
                "Red-breasted Sapsucker", "Pacific Wren", "Marsh Wren", "Northern House Wren",
                "Bewick's Wren", "Rock Wren"
  ), taxonomy_version=2025) %>% 
  
  # Keep only standard protocols (Best Practices)
  auk_protocol(protocol = c("Stationary", "Traveling")) %>%
  
  # Restrict to suggested limits (Best Practices)
  auk_duration(c(0, 60*5)) %>%
  auk_distance(distance = c(0, 5)) %>%
  
  # Complete data (for getting a proxy of effort)
  auk_complete() %>%
  
  # Date filters
  auk_date(date = c("*-05-01", "*-07-31")) %>%
  
  # Year filters (only applicable to sampling checklists) %>%
  auk_year(year = 2010:2025) %>%
  
  # Spatial filters - Limit to BCR selection
  auk_bbox(st_bbox(st_transform(data_sf, crs = 4326))) %>% # bbox must be GPS lat/lon (4326)
  
  # Write
  if(!file.exists("Outputs/Final/ebd_simpcw_final_all.txt")) {
    auk_filter(f_ebd, f_sed, overwrite = TRUE)
  }

## Zero-Filling Data
# Load in and convert NA distances to 0 for obs and sampling data
sampling <- read_sampling("Outputs/sed_simpcw_final_all.txt") %>%
  mutate(effort_distance_km = replace_na(effort_distance_km, 0))

obs <- read_ebd("Outputs/ebd_simpcw_final_all.txt") %>%
  mutate(effort_distance_km = replace_na(effort_distance_km, 0))

# Zero fill data and keep relevant columns
tax <- get_ebird_taxonomy(version = 2025)

full <- auk_zerofill(obs, sampling_events = sampling, collapse = TRUE) %>%
  mutate(
    year = year(observation_date),
    month = month(observation_date)
  ) %>%
  assert(within_bounds(0, 5), effort_distance_km) %>%
  assert(within_bounds(2010, 2025), year) %>%
  assert(within_bounds(5, 7), month) %>%
  left_join(tax %>% select(scientific_name, common_name), by = "scientific_name") %>%
  select(
    year, observation_date, checklist_id,
    observation_count, species_observed,
    latitude, longitude,
    scientific_name, common_name
  )

# Save filtered data
write_csv(full, "full_simpcw_final_all.csv")

# Transform into sf data
full_sf <- full %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(data_sf))  

# Get species within Simpcw
full_sf <- full_sf %>%
  mutate(in_territory = st_within(geometry, data_sf, sparse = FALSE)[, 1]) %>%
  filter(in_territory) 

# Get summary per year per species of checklists with and without
summary_df <- full_sf %>%
  st_drop_geometry() %>%
  group_by(common_name, year) %>%
  summarize(
    total_checklists = n_distinct(checklist_id),
    checklists_with_species = sum(species_observed, na.rm = TRUE),
    prop_checklists = checklists_with_species / total_checklists,
    .groups = "drop"
  )

# Save checklists in Simpcw
write_csv(summary_df, "Outputs/simpcw_checklists_final_all.csv")

# Load in protected areas 
protected_areas <- st_read("Outputs/cropped.gpkg")

# Make geometries valid
protected_areas <- st_make_valid(protected_areas)

# Match CRS of full_sf and protected_areas to data_sf
full_sf_test <- st_transform(full_sf, st_crs(data_sf))
protected_areas <- st_transform(protected_areas, st_crs(data_sf))

# Add column for checklists within protected areas (TRUE/FALSE)
full_sf <- full_sf %>%
  mutate(in_protected_area = lengths(st_within(geometry, protected_areas)) > 0)

# Summarize checklists within and outside protected areas
protected_summary <- full_sf %>%
  st_drop_geometry() %>%
  group_by(common_name, year, in_protected_area) %>%
  summarize(
    total_checklists = n_distinct(checklist_id),
    checklists_with_species = sum(species_observed, na.rm = TRUE),
    prop_checklists = checklists_with_species / total_checklists,
    .groups = "drop"
  )

# Save checklists in and outside of Protected Areas within Simpcw
write_csv(protected_summary, "Outputs/protected_areas_checklists_final_all.csv")
