library(tidyverse)
library(sf)
library(rnaturalearth)
library(RColorBrewer)

# Load unzipped territory .kml in
kml_file <- "Spatial Data/doc.kml"
data_sf <- st_read(kml_file)

# Load master trend data in
master <- read_csv("Species Data/Master_list.csv")

# Get Canada outline
canada <- ne_states(country = "Canada", returnclass = "sf")

# Set CRS 
crs_target <- st_crs(data_sf)
canada_proj <- st_transform(canada, crs_target)

# Load species files and assign names
species_files <- tibble(
  species = c("American Crow", "Bald Eagle", "Black-capped Chickadee", "Common Raven", 
              "Golden Eagle", "Great-horned Owl", "Marsh Wren", "Mountain Chickadee", "Rufous Hummingbird","Steller's Jay", "Western Meadowlark", "Northern Flicker", "Northern Pygmy-owl", "Pacific Wren", "Pileated Woodpecker", "Red-naped Sapsucker"),
  path = c(
    "Species Data/amecro_ebird-trends_2022/amecro_breeding_ebird-trends_2022.gpkg",
    "Species Data/baleag_ebird-trends_2022/baleag_breeding_ebird-trends_2022.gpkg",
    "Species Data/bkcchi_ebird-trends_2022/bkcchi_resident_ebird-trends_2022.gpkg", 
    "Species Data/comrav_ebird-trends_2022/comrav_resident_ebird-trends_2022.gpkg",
    "Species Data/goleag_ebird-trends_2022/goleag_breeding_ebird-trends_2022.gpkg",
    "Species Data/grhowl_ebird-trends_2022/grhowl_resident_ebird-trends_2022.gpkg",
    "Species Data/marwre_ebird-trends_2022/marwre_breeding_ebird-trends_2022.gpkg",
    "Species Data/mouchi_ebird-trends_2022/mouchi_breeding_ebird-trends_2022.gpkg",
    "Species Data/rufhum_ebird-trends_2022/rufhum_breeding_ebird-trends_2022.gpkg",
    "Species Data/stejay_ebird-trends_2022/stejay_resident_ebird-trends_2022.gpkg",
    "Species Data/wesmea_ebird-trends_2022/wesmea_breeding_ebird-trends_2022.gpkg",
    "Species Data/norfli_ebird-trends_2022/norfli_breeding_ebird-trends_2022.gpkg",
    "Species Data/nopowl_ebird-trends_2022/nopowl_resident_ebird-trends_2022.gpkg",
    "Species Data/pacwre1_ebird-trends_2022/pacwre1_breeding_ebird-trends_2022.gpkg",
    "Species Data/pilwoo_ebird-trends_2022/pilwoo_resident_ebird-trends_2022.gpkg",
    "Species Data/rensap_ebird-trends_2022/rensap_breeding_ebird-trends_2022.gpkg"
  )
)

species_files <- species_files %>%
  left_join(master, by = c("species" = "Species"))

# Load, clip, and transform all species trend points; 
# then join each point with its species' Trend Period and create a combined label for plotting
all_species <- species_files %>%
  mutate(data = map2(path, species, ~ {
    dat <- st_read(.x, quiet = TRUE)
    clipped <- st_intersection(dat, data_sf)
    clipped$species <- .y
    st_transform(clipped, crs_target)
  })) %>%
  pull(data) %>%
  bind_rows() %>%
  left_join(species_files %>% select(species, `Trend Period`), by = "species") %>%
  mutate(
    species_label = paste0(str_wrap(species, width = 16), "\n", `Trend Period`)
  )

# Save trend data as .gpkg
out_file <- "Outputs/all_species_trend_points.gpkg"

if (!file.exists(out_file)) {
  st_write(all_species, out_file)
} else {
  message("File already exists. Skipping write.")
}

## Plot trend data
# Set bounding box and expand for visibility
bbox <- st_bbox(data_sf)
expand_factor <- 0.3
bbox_expanded <- bbox
bbox_expanded["xmin"] <- bbox["xmin"] - expand_factor
bbox_expanded["xmax"] <- bbox["xmax"] + expand_factor
bbox_expanded["ymin"] <- bbox["ymin"] - expand_factor
bbox_expanded["ymax"] <- bbox["ymax"] + expand_factor

# Plot
ggplot() +
  geom_sf(data = canada_proj, fill = "white", color = "gray40") +
  geom_sf(data = data_sf, fill = "gray95", color = "black", size = 0.5, alpha = 0.5) +
  geom_sf(data = all_species, aes(fill = abd_ppy), color = NA) +
  facet_wrap(~ species_label, ncol = 4)  +
  scale_fill_gradientn(
    colors = (brewer.pal(11, "RdBu")),  # 11 steps
    limits = c(-10, 10),
    oob = scales::squish
  ) +
  coord_sf(
    xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]),
    ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]),
    expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = "Species Trends in Simpcw Territory (2012–2022)",
    subtitle = "Red = Decrease, Blue = Increase, Point Size = Relative Abundance",
    fill = "Trend (% per year)"
  ) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
  ) +
  scale_x_continuous(breaks = seq(-120, -115, by = 2)) +
  scale_y_continuous(breaks = seq(51, 54, by = 1))

## Calculate average trends for all species
# Load, clip, and summarise average trend
species_means <- species_files %>%
  mutate(mean_trend = map2_dbl(path, species, ~ {
    dat <- st_read(.x, quiet = TRUE)
    clipped <- st_intersection(dat, data_sf)
    mean(clipped$abd_ppy, na.rm = TRUE)
  }))

# Join average to territory polygon 
territory_filled <- species_means %>%
  rowwise() %>%
  mutate(geometry = st_geometry(data_sf)) %>%
  st_as_sf(crs = crs_target) 

# Wrap labels longer than 16 letters
territory_filled <- territory_filled %>%
  mutate(
    species_label = paste0(str_wrap(species, width = 16), "\n", `Trend Period`))

## Plot average trend data
ggplot() +
  geom_sf(data = canada_proj, fill = "white", color = "gray40") +
  geom_sf(data = territory_filled, aes(fill = mean_trend), color = "black", size = 0.5, alpha = 0.75) +
  facet_wrap(~ species_label, ncol = 4) +
  scale_fill_gradientn(
    colors = (brewer.pal(11, "RdBu")),  # 11 steps
    limits = c(-10, 10),
    oob = scales::squish
  ) +
  theme_minimal() +
  coord_sf(
    xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]),
    ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]),
    expand = FALSE
  ) +
  labs(
    title = "Average Species Trends in Simpcw Territory (2012–2022)",
    subtitle = "Red = Decrease, Blue = Increase",
    fill = "Average Trend"
  ) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
  ) +
  scale_x_continuous(breaks = seq(-120, -115, by = 2)) +
  scale_y_continuous(breaks = seq(51, 54, by = 1))
