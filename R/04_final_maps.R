#### FINAL MAPS ################################################################

# Packages ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(patchwork)


# Prep CTs ----------------------------------------------------------------

CT_for_map <- 
  CT_scaled |> 
  left_join(distinct(CT_geom, census_tract, .keep_all = TRUE), 
            by = "census_tract") |> 
  st_as_sf(crs = 4326)


# Other geometries --------------------------------------------------------

canada <- 
  cancensus::get_census("CA16", regions = list(C = "01"), geo_format = "sf")

water <- 
  read_sf("data/lhy_000c16a_e/lhy_000c16a_e.shp") |>  
  st_transform(4326)
  
buffer <- 
  CT_for_map |> 
  st_union() |> 
  st_buffer(5000) |> 
  st_make_valid()

water <- 
  water |> 
  st_make_valid()

water <- 
  water |> 
  st_filter(buffer)


# value_2016 --------------------------------------------------------------

value_2016_maps <- 
  map(c("Calgary", "Edmonton", "Ottawa", "Montreal", "Toronto", "Vancouver"), ~{
    CT_for_map |> 
      filter(city == .x) |> 
      select(census_tract, city, value_2016) |> 
      ggplot() +
      geom_sf(data = canada, fill = "grey90", colour = "transparent") +
      geom_sf(aes(fill = value_2016), colour = "#FFFFFF66", lwd = 0.1) +
      ggspatial::annotation_scale(height = unit(0.2, "cm")) +
      geom_sf(data = water, fill = "white", colour = "transparent") +
      scale_fill_viridis_b(limits = c(-2, 2), oob = scales::squish,
                           n.breaks = 7) +
      ggtitle(.x) +
      coord_sf(xlim = st_bbox(filter(CT_for_map, city == .x))[c(1, 3)],
               ylim = st_bbox(filter(CT_for_map, city == .x))[c(2, 4)]) +
      theme_minimal() +
      theme(legend.position = "right",
            axis.text = element_text(colour = "transparent"),
            panel.grid = element_blank())
  })

ggsave("output/value_2016.png", 
       patchwork::wrap_plots(value_2016_maps) +
         plot_layout(guides = "collect"),
       width = 12, height = 8)


# rent_2016 ---------------------------------------------------------------

rent_2016_maps <- 
  map(c("Calgary", "Edmonton", "Ottawa", "Montreal", "Toronto", "Vancouver"), ~{
    CT_for_map |> 
      filter(city == .x) |> 
      select(census_tract, city, rent_2016) |> 
      ggplot() +
      geom_sf(data = canada, fill = "grey90", colour = "transparent") +
      geom_sf(aes(fill = rent_2016), colour = "#FFFFFF66", lwd = 0.1) +
      ggspatial::annotation_scale(height = unit(0.2, "cm")) +
      geom_sf(data = water, fill = "white", colour = "transparent") +
      scale_fill_viridis_b(limits = c(-2, 2), oob = scales::squish,
                           n.breaks = 7) +
      ggtitle(.x) +
      coord_sf(xlim = st_bbox(filter(CT_for_map, city == .x))[c(1, 3)],
               ylim = st_bbox(filter(CT_for_map, city == .x))[c(2, 4)]) +
      theme_minimal() +
      theme(legend.position = "right",
            axis.text = element_text(colour = "transparent"),
            panel.grid = element_blank())
  })

ggsave("output/rent_2016.png", 
       patchwork::wrap_plots(rent_2016_maps) +
         plot_layout(guides = "collect"),
       width = 12, height = 8)


# rac_dis_2016 ------------------------------------------------------------

rac_dis_2016_maps <- 
  map(c("Calgary", "Edmonton", "Ottawa", "Montreal", "Toronto", "Vancouver"), ~{
    CT_for_map |> 
      filter(city == .x) |> 
      select(census_tract, city, rac_dis_ratio_2016) |> 
      ggplot() +
      geom_sf(data = canada, fill = "grey90", colour = "transparent") +
      geom_sf(aes(fill = rac_dis_ratio_2016), colour = "#FFFFFF66", lwd = 0.1) +
      ggspatial::annotation_scale(height = unit(0.2, "cm")) +
      geom_sf(data = water, fill = "white", colour = "transparent") +
      scale_fill_viridis_b(limits = c(-2, 2), oob = scales::squish,
                           n.breaks = 7) +
      ggtitle(.x) +
      coord_sf(xlim = st_bbox(filter(CT_for_map, city == .x))[c(1, 3)],
               ylim = st_bbox(filter(CT_for_map, city == .x))[c(2, 4)]) +
      theme_minimal() +
      theme(legend.position = "right",
            axis.text = element_text(colour = "transparent"),
            panel.grid = element_blank())
  })

ggsave("output/rac_dis_2016.png", 
       patchwork::wrap_plots(rac_dis_2016_maps) +
         plot_layout(guides = "collect"),
       width = 12, height = 8)

# non_perm_2016 -----------------------------------------------------------

non_perm_2016_maps <- 
  map(c("Calgary", "Edmonton", "Ottawa", "Montreal", "Toronto", "Vancouver"), ~{
    CT_for_map |> 
      filter(city == .x) |> 
      select(census_tract, city, non_perm_ratio_2016) |> 
      ggplot() +
      geom_sf(data = canada, fill = "grey90", colour = "transparent") +
      geom_sf(aes(fill = non_perm_ratio_2016), colour = "#FFFFFF66", lwd = 0.1) +
      ggspatial::annotation_scale(height = unit(0.2, "cm")) +
      geom_sf(data = water, fill = "white", colour = "transparent") +
      scale_fill_viridis_b(limits = c(-2, 2), oob = scales::squish,
                           n.breaks = 7) +
      ggtitle(.x) +
      coord_sf(xlim = st_bbox(filter(CT_for_map, city == .x))[c(1, 3)],
               ylim = st_bbox(filter(CT_for_map, city == .x))[c(2, 4)]) +
      theme_minimal() +
      theme(legend.position = "right",
            axis.text = element_text(colour = "transparent"),
            panel.grid = element_blank())
  })

ggsave("output/non_perm_2016.png", 
       patchwork::wrap_plots(non_perm_2016_maps) +
         plot_layout(guides = "collect"),
       width = 12, height = 8)


# val_per_unit_2016 -------------------------------------------------------

mpac_for_map <- 
  mpac_scaled |> 
  select(census_tract, val_per_unit_2016) |> 
  left_join(distinct(CT_geom, census_tract, .keep_all = TRUE), 
            by = "census_tract") |> 
  st_as_sf(crs = 4326)

val_per_unit_map <- 
  mpac_for_map |> 
  ggplot() +
  geom_sf(data = canada, fill = "grey90", colour = "transparent") +
  geom_sf(aes(fill = val_per_unit_2016), colour = "#FFFFFF66", lwd = 0.1) +
  geom_sf(data = st_as_sf(mpac), size = 0.2, colour = "black", alpha = 0.5) +
  ggspatial::annotation_scale(height = unit(0.2, "cm")) +
  geom_sf(data = water, fill = "white", colour = "transparent") +
  scale_fill_viridis_b(limits = c(-2, 2), oob = scales::squish,
                       n.breaks = 7) +
  coord_sf(xlim = st_bbox(mpac_for_map)[c(1, 3)],
           ylim = st_bbox(mpac_for_map)[c(2, 4)]) +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text = element_text(colour = "transparent"),
        panel.grid = element_blank())

ggsave("output/val_per_unit_2016.png", val_per_unit_map,
       width = 12, height = 8)




