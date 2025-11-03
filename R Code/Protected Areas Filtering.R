library(tidyverse)
library(sf)
library(rnaturalearth)

# Set working directory
setwd("/Volumes/Seagate/Matt and Mia/")

# Unzip Simpcw territory boundary
zip_file <- normalizePath("Spatial Data/SFN_TT.kmz")
unzip(zip_file, exdir = "Outputs/")

# Load Simpcw territory boundary
data_sf <- st_read("Spatial Data/doc.kml") 

# Read in protected and conserved area data
data <- st_read("Spatial Data/ProtectedConservedArea_2024/ProtectedConservedArea_2024.gdb", layer="ProtectedConservedArea_2024") %>%
  st_transform(st_crs(data_sf))

# Filter for AB and BC
# Filter Federal, Provincial, Municipal, and Indigenous protected areas
data_filtered <- data %>% 
  filter(LOC == "1" | LOC == "2") %>% 
  filter(OWNER_TYPE == "1" | OWNER_TYPE == "2" | OWNER_TYPE == "3" | OWNER_TYPE == "4")

# Crop protected areas to Simpcw territory
cropped_data <- st_intersection(data_filtered, data_sf) %>%
  select(NAME_E, OWNER_E, GOV_TYPE, O_AREA_HA, Shape_Area)

# Save
st_write(cropped_data, "Outputs/cropped_all.gpkg")

## Visualization of different levels of filtering

##-----------------------------------------------------------------------------

# Load data and make valid
cropped_data <- st_read("Outputs/cropped_all.gpkg") %>% 
  st_transform(st_crs(data_sf))

# Plot with no filters applied 
ggplot() +
  geom_sf(data = data_sf, fill = "grey85") +
  geom_sf(data = cropped_data, aes(fill = NAME_E), show.legend = FALSE) +   # hides legend
  theme_minimal() +
  ggtitle("Protected Areas: All Data") +
  scale_fill_viridis_d(
    option = "B"
  ) +
  scale_x_continuous(breaks = seq(-122, -110, by = 1)) +  # every 5 on x-axis
  scale_y_continuous(breaks = seq(50, 54, by = 1)) 

##-----------------------------------------------------------------------------

# Set names to remove and remove
excluded_names <- c("Old Growth Management Areas (Mapped Legal)", 
                    "Wildlife Habitat Areas")

cropped_filtered <- cropped_data %>%
  filter(!(NAME_E %in% excluded_names))

# Make valid
cropped_filtered <- st_make_valid(cropped_filtered)

# Save
output_path <- "Outputs/cropped.gpkg"

if (!file.exists(output_path)) {
  st_write(cropped_filtered, output_path)
  message("File written to: ", output_path)
} else {
  message("File already exists. Not overwriting: ", output_path)
}

# Plot with some filters applied 
ggplot() +
  geom_sf(data = data_sf, fill = "grey85") +
  geom_sf(data = cropped_filtered, aes(fill = NAME_E), show.legend = FALSE) +   # hides legend
  theme_minimal() +
  ggtitle("Protected Areas: Some Filtering") +
  scale_fill_viridis_d(
    option = "B"
  ) +
  scale_x_continuous(breaks = seq(-122, -110, by = 1)) +  # every 5 on x-axis
  scale_y_continuous(breaks = seq(50, 54, by = 1)) 

##-----------------------------------------------------------------------------

# Filter area to be greater than 10000 HA
selected <- cropped_filtered %>% 
  filter(O_AREA_HA > 10000)

# Plot with some filters applied 
ggplot() +
  geom_sf(data = data_sf, fill = "grey85") +
  geom_sf(data = selected, aes(fill = NAME_E), show.legend = FALSE) +   # hides legend
  theme_minimal() +
  ggtitle("Protected Areas: > 10,000 HA") +
  scale_fill_viridis_d(
    option = "B"
  ) +
  scale_x_continuous(breaks = seq(-122, -110, by = 1)) +  # every 5 on x-axis
  scale_y_continuous(breaks = seq(50, 54, by = 1)) 


