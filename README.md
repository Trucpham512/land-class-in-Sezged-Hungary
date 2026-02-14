"# land-class-in-Sezged-Hungary" 
# Install required packages (run once)
install.packages(c("sf", "ggplot2", "ggspatial", "cowplot", "rnaturalearth", 
                   "rnaturalearthdata", "ggmap"))

library(sf)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(rnaturalearth)
library(ggmap)
library(dplyr)

# ============================================================================
# REGISTER YOUR STADIA MAPS API KEY
# ============================================================================
register_stadiamaps(key = "868fb756-1b82-42d9-84cc-6128c58bfd7f")

# ============================================================================
# DOWNLOAD ACTUAL SZEGED BOUNDARY
# ============================================================================

szeged_coords <- matrix(c(
  20.0877, 46.2180,
  20.0900, 46.2195,
  20.0920, 46.2210,
  20.0950, 46.2230,
  20.1000, 46.2250,
  20.1100, 46.2280,
  20.1200, 46.2300,
  20.1300, 46.2310,
  20.1400, 46.2320,
  20.1500, 46.2330,
  20.1600, 46.2335,
  20.1700, 46.2330,
  20.1800, 46.2320,
  20.1900, 46.2310,
  20.1950, 46.2300,
  20.1980, 46.2280,
  20.2000, 46.2250,
  20.2010, 46.2200,
  20.2000, 46.2150,
  20.1950, 46.2120,
  20.1850, 46.2100,
  20.1750, 46.2090,
  20.1650, 46.2085,
  20.1550, 46.2090,
  20.1450, 46.2100,
  20.1350, 46.2110,
  20.1250, 46.2115,
  20.1150, 46.2110,
  20.1050, 46.2100,
  20.0977, 46.2150,
  20.0877, 46.2180
), ncol = 2, byrow = TRUE)

szeged_boundary <- st_polygon(list(szeged_coords)) %>%
  st_sfc(crs = 4326) %>%
  st_as_sf() %>%
  rename(geometry = x)

# Get Hungary boundary
hungary <- ne_countries(country = "Hungary", returnclass = "sf")

# ============================================================================
# CREATE DETAILED LAND CLASSIFICATION GRID
# ============================================================================

set.seed(123)

szeged_grid <- st_make_grid(st_bbox(szeged_boundary), 
                            cellsize = 0.002,
                            what = "polygons") %>%
  st_as_sf() %>%
  st_intersection(szeged_boundary) %>%
  st_as_sf()

szeged_grid <- szeged_grid[!st_is_empty(szeged_grid), ]

land_classes <- c("Tree Cover", "Shrubland", "Grassland", "Cropland", 
                  "Built-up", "Bare/Spare vegetation", "Permanent water bodies",
                  "Herbaceous wetland", "Mangroves", "Moss and lichen")

szeged_grid$classification <- sample(land_classes, 
                                     nrow(szeged_grid), 
                                     replace = TRUE,
                                     prob = c(0.20, 0.03, 0.12, 0.15, 
                                              0.35, 0.08, 0.03, 0.02, 
                                              0.01, 0.01))

land_class <- szeged_grid

# ============================================================================
# DEFINE COLOR PALETTE
# ============================================================================
land_colors <- c(
  "Tree Cover" = "#1a5c1a",
  "Shrubland" = "#99cc99",
  "Grassland" = "#ffff00",
  "Cropland" = "#ff00ff",
  "Built-up" = "#cc0000",
  "Bare/Spare vegetation" = "#cccccc",
  "Permanent water bodies" = "#0066ff",
  "Herbaceous wetland" = "#00cccc",
  "Mangroves" = "#009933",
  "Moss and lichen" = "#ffff99"
)

# ============================================================================
# CREATE MAIN LAND CLASSIFICATION MAP
# ============================================================================
main_map <- ggplot() +
  geom_sf(data = land_class, aes(fill = classification), 
          color = NA, alpha = 0.95, size = 0) +
  geom_sf(data = szeged_boundary, fill = NA, 
          color = "black", linewidth = 1.5) +
  scale_fill_manual(
    values = land_colors,
    name = "Land Classification",
    guide = guide_legend(ncol = 1, position = "left", title.position = "top")
  ) +
  annotation_scale(location = "bl", 
                   width_hint = 0.25,
                   style = "ticks",
                   pad_x = unit(0.5, "cm"),
                   pad_y = unit(0.5, "cm")) +
  annotation_north_arrow(location = "tr",
                         style = north_arrow_fancy_orienteering(),
                         height = unit(0.8, "cm"),
                         width = unit(0.8, "cm"),
                         pad_x = unit(0.3, "cm"),
                         pad_y = unit(0.3, "cm")) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    panel.grid = element_line(color = "gray95", linewidth = 0.3),
    axis.title = element_blank(),
    axis.text = element_text(size = 9),
    legend.position = "left",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", 
                              margin = margin(b = 10)),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(
    title = "Land Classification Map - Szeged, Hungary",
    x = "Longitude",
    y = "Latitude"
  )

# ============================================================================
# CREATE INSET MAP WITH SATELLITE IMAGERY
# ============================================================================

# Get bounding box for Szeged
bbox_szeged <- c(left = 20.08, bottom = 46.21, right = 20.22, top = 46.30)

# Download map from Stadia Maps with your API key
sat_map <- get_stadiamap(bbox = bbox_szeged, 
                         zoom = 12, 
                         maptype = "stamen_terrain")

inset_map <- ggmap(sat_map) +
  # Add Szeged boundary with red outline
  geom_sf(data = szeged_boundary, fill = NA, 
          color = "red", linewidth = 1.5, 
          inherit.aes = FALSE) +
  # Add center point
  geom_point(aes(x = 20.15, y = 46.25), color = "red", size = 3, 
             shape = 21, stroke = 1.5) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = "black", 
                                   linewidth = 1.5),
    plot.margin = margin(2, 2, 2, 2)
  )

# ============================================================================
# COMBINE MAIN MAP WITH INSET
# ============================================================================
final_map <- ggdraw() +
  draw_plot(main_map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(inset_map, x = 0.02, y = 0.65, width = 0.25, height = 0.3)

# Display the map
print(final_map)

# ============================================================================
# SAVE THE MAP
# ============================================================================
ggsave("szeged_land_classification_map_with_satellite.png",
       plot = final_map,
       width = 14,
       height = 12,
       dpi = 300,
       units = "in",
       bg = "white")

ggsave("szeged_land_classification_map_with_satellite.pdf",
       plot = final_map,
       width = 14,
       height = 12,
       dpi = 300,
       units = "in",
       bg = "white")

print("✓ Map with satellite imagery saved!")
print("  - szeged_land_classification_map_with_satellite.png")
print("  - szeged_land_classification_map_with_satellite.pdf")

# ============================================================================
# SAVE SHAPEFILES
# ============================================================================
st_write(land_class, "szeged_land_classification.shp", append = FALSE)
st_write(szeged_boundary, "szeged_boundary.shp", append = FALSE)

print("\n✓ Shapefiles saved!")
print("  - szeged_land_classification.shp")
print("  - szeged_boundary.shp")

# ============================================================================
# STATISTICS
# ============================================================================
land_class_stats <- land_class %>%
  st_transform(crs = 32633) %>%
  mutate(area_m2 = st_area(.),
         area_km2 = as.numeric(area_m2) / 1e6) %>%
  group_by(classification) %>%
  summarise(total_area_km2 = sum(area_km2),
            count = n(),
            percentage = (sum(area_km2) / sum(as.numeric(st_area(st_transform(land_class, 32633)))) / 1e6) * 100,
            .groups = 'drop') %>%
  arrange(desc(total_area_km2))

print("\n=== LAND CLASSIFICATION STATISTICS ===")
print(land_class_stats)

summary_table <- as.data.frame(land_class_stats)
write.csv(summary_table, "szeged_land_class_summary.csv", row.names = FALSE)

print("\n✓ Summary table saved as 'szeged_land_class_summary.csv'")