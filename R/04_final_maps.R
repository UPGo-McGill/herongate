#### FINAL MAPS ################################################################

# Packages ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(patchwork)


# Prep CTs ----------------------------------------------------------------

CT_for_map <- 
  CT_final |> 
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


# black_2016 --------------------------------------------------------------

black_2016_maps <- 
  map(c("Calgary", "Edmonton", "Ottawa", "Montreal", "Toronto", "Vancouver"), ~{
    CT_for_map |> 
      filter(city == .x) |> 
      select(census_tract, city, black_ratio_2016) |> 
      ggplot() +
      geom_sf(data = canada, fill = "grey90", colour = "transparent") +
      geom_sf(aes(fill = black_ratio_2016), colour = "#FFFFFF66", lwd = 0.1) +
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

ggsave("output/black_2016.png", 
       patchwork::wrap_plots(black_2016_maps) +
         plot_layout(guides = "collect"),
       width = 12, height = 8)


# value_2016 --------------------------------------------------------------

value_2016_maps <- 
  map(c("Calgary", "Edmonton", "Ottawa", "Montreal", "Toronto", "Vancouver"), ~{
    CT_for_map |> 
      filter(city == .x) |> 
      ggplot() +
      geom_sf(data = canada, fill = "grey90", colour = "transparent") +
      geom_sf(aes(fill = value_2016), colour = "#FFFFFF66", lwd = 0.1) +
      ggspatial::annotation_scale(height = unit(0.2, "cm")) +
      # geom_sf(data = water, fill = "white", colour = "transparent") +
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


# rented_2016 -------------------------------------------------------------

rented_2016_maps <- 
  map(c("Calgary", "Edmonton", "Ottawa", "Montreal", "Toronto", "Vancouver"), ~{
    CT_for_map |> 
      filter(city == .x) |> 
      ggplot() +
      geom_sf(data = canada, fill = "grey90", colour = "transparent") +
      geom_sf(aes(fill = rented_2016), colour = "#FFFFFF66", lwd = 0.1) +
      ggspatial::annotation_scale(height = unit(0.2, "cm")) +
      # geom_sf(data = water, fill = "white", colour = "transparent") +
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

ggsave("output/rented_2016.png", 
       patchwork::wrap_plots(rented_2016_maps) +
         plot_layout(guides = "collect"),
       width = 12, height = 8)


# value_change ------------------------------------------------------------

value_change_maps <- 
  map(c("Calgary", "Edmonton", "Ottawa", "Montreal", "Toronto", "Vancouver"), ~{
    CT_for_map |> 
      filter(city == .x) |> 
      ggplot() +
      geom_sf(data = canada, fill = "grey90", colour = "transparent") +
      geom_sf(aes(fill = value_change), colour = "#FFFFFF66", lwd = 0.1) +
      ggspatial::annotation_scale(height = unit(0.2, "cm")) +
      # geom_sf(data = water, fill = "white", colour = "transparent") +
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

ggsave("output/value_change.png", 
       patchwork::wrap_plots(value_change_maps) +
         plot_layout(guides = "collect"),
       width = 12, height = 8)


# black_ratio_change ------------------------------------------------------

black_ratio_change_maps <- 
  map(c("Calgary", "Edmonton", "Ottawa", "Montreal", "Toronto", "Vancouver"), ~{
    CT_for_map |> 
      filter(city == .x) |> 
      ggplot() +
      geom_sf(data = canada, fill = "grey90", colour = "transparent") +
      geom_sf(aes(fill = black_ratio_change), colour = "#FFFFFF66", lwd = 0.1) +
      ggspatial::annotation_scale(height = unit(0.2, "cm")) +
      # geom_sf(data = water, fill = "white", colour = "transparent") +
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

ggsave("output/black_ratio_change.png", 
       patchwork::wrap_plots(black_ratio_change_maps) +
         plot_layout(guides = "collect"),
       width = 12, height = 8)

