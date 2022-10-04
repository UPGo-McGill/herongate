#### HERONGATE MAPS ############################################################

library(patchwork)


# 2016 value maps ---------------------------------------------------------

mtl_value <-
  CT_scaled |> 
  filter(city == "Montreal") |> 
  select(census_tract, value_2016, black_ratio_2016) |> 
  pivot_longer(c(value_2016, black_ratio_2016)) |> 
  left_join(distinct(CT_geom, census_tract, .keep_all = TRUE), 
            by = "census_tract") |>
  st_as_sf(crs = 4326) |> 
  ggplot(aes(fill = value)) +
  geom_sf(colour = "transparent") +
  facet_wrap(~name) +
  scale_fill_viridis_b(limits = c(-2, 2), oob = scales::squish,
                       n.breaks = 7) +
  theme_void() +
  theme(legend.position = "right")

ggsave("output/mtl_value.png", mtl_value, width = 8, height = 5, units = "in")

to_value <-
  CT_scaled |> 
  filter(city == "Toronto") |> 
  select(census_tract, value_2016, black_ratio_2016) |> 
  pivot_longer(c(value_2016, black_ratio_2016)) |> 
  left_join(distinct(CT_geom, census_tract, .keep_all = TRUE), 
            by = "census_tract") |>
  st_as_sf(crs = 4326) |> 
  ggplot(aes(fill = value)) +
  geom_sf(colour = "transparent") +
  facet_wrap(~name) +
  scale_fill_viridis_b(limits = c(-2, 2), oob = scales::squish,
                       n.breaks = 7) +
  theme_void() +
  theme(legend.position = "right")

ggsave("output/to_value.png", to_value, width = 8, height = 5, units = "in")

ott_value <-
  CT_scaled |> 
  filter(city == "Ottawa") |> 
  select(census_tract, value_2016, black_ratio_2016) |> 
  pivot_longer(c(value_2016, black_ratio_2016)) |> 
  left_join(distinct(CT_geom, census_tract, .keep_all = TRUE), 
            by = "census_tract") |>
  st_as_sf(crs = 4326) |> 
  ggplot(aes(fill = value)) +
  geom_sf(colour = "transparent") +
  facet_wrap(~name) +
  scale_fill_viridis_b(limits = c(-2, 2), oob = scales::squish,
                       n.breaks = 7) +
  theme_void() +
  theme(legend.position = "right")

ggsave("output/ott_value.png", ott_value, width = 8, height = 5, units = "in")


# 2016 value change maps --------------------------------------------------

mtl_value_change <-
  CT_scaled |> 
  filter(city == "Montreal") |> 
  select(census_tract, value_change_pct, black_ratio_2016) |> 
  pivot_longer(c(value_change_pct, black_ratio_2016)) |> 
  left_join(distinct(CT_geom, census_tract, .keep_all = TRUE), 
            by = "census_tract") |>
  st_as_sf(crs = 4326) |> 
  ggplot(aes(fill = value)) +
  geom_sf(colour = "transparent") +
  facet_wrap(~name) +
  scale_fill_viridis_b(limits = c(-2, 2), oob = scales::squish,
                       n.breaks = 7) +
  theme_void() +
  theme(legend.position = "right")

ggsave("output/mtl_value_change.png", mtl_value_change, width = 8, height = 5, 
       units = "in")

to_value_change <-
  CT_scaled |> 
  filter(city == "Toronto") |> 
  select(census_tract, value_change_pct, black_ratio_2016) |> 
  pivot_longer(c(value_change_pct, black_ratio_2016)) |> 
  left_join(distinct(CT_geom, census_tract, .keep_all = TRUE), 
            by = "census_tract") |>
  st_as_sf(crs = 4326) |> 
  ggplot(aes(fill = value)) +
  geom_sf(colour = "transparent") +
  facet_wrap(~name) +
  scale_fill_viridis_b(limits = c(-2, 2), oob = scales::squish,
                       n.breaks = 7) +
  theme_void() +
  theme(legend.position = "right")

ggsave("output/to_value_change.png", to_value_change, width = 8, height = 5, 
       units = "in")

ott_value_change <-
  CT_scaled |> 
  filter(city == "Ottawa") |> 
  select(census_tract, value_change_pct, black_ratio_2016) |> 
  pivot_longer(c(value_change_pct, black_ratio_2016)) |> 
  left_join(distinct(CT_geom, census_tract, .keep_all = TRUE), 
            by = "census_tract") |>
  st_as_sf(crs = 4326) |> 
  ggplot(aes(fill = value)) +
  geom_sf(colour = "transparent") +
  facet_wrap(~name) +
  scale_fill_viridis_b(limits = c(-2, 2), oob = scales::squish,
                       n.breaks = 7) +
  theme_void() +
  theme(legend.position = "right")

ggsave("output/ott_value_change.png", ott_value_change, width = 8, height = 5, 
       units = "in")


# 2016 rent maps ----------------------------------------------------------

mtl_rent <-
  CT_scaled |> 
  filter(city == "Montreal") |> 
  select(census_tract, rent_2016, black_ratio_2016) |> 
  pivot_longer(c(rent_2016, black_ratio_2016)) |> 
  left_join(distinct(CT_geom, census_tract, .keep_all = TRUE), 
            by = "census_tract") |>
  st_as_sf(crs = 4326) |> 
  ggplot(aes(fill = value)) +
  geom_sf(colour = "transparent") +
  facet_wrap(~name) +
  scale_fill_viridis_b(limits = c(-2, 2), oob = scales::squish,
                       n.breaks = 7) +
  theme_void() +
  theme(legend.position = "right")

ggsave("output/mtl_rent.png", mtl_rent, width = 8, height = 5, 
       units = "in")

to_rent <-
  CT_scaled |> 
  filter(city == "Toronto") |> 
  select(census_tract, rent_2016, black_ratio_2016) |> 
  pivot_longer(c(rent_2016, black_ratio_2016)) |> 
  left_join(distinct(CT_geom, census_tract, .keep_all = TRUE), 
            by = "census_tract") |>
  st_as_sf(crs = 4326) |> 
  ggplot(aes(fill = value)) +
  geom_sf(colour = "transparent") +
  facet_wrap(~name) +
  scale_fill_viridis_b(limits = c(-2, 2), oob = scales::squish,
                       n.breaks = 7) +
  theme_void() +
  theme(legend.position = "right")

ggsave("output/to_rent.png", to_rent, width = 8, height = 5, 
       units = "in")

ott_rent <-
  CT_scaled |> 
  filter(city == "Ottawa") |> 
  select(census_tract, rent_2016, black_ratio_2016) |> 
  pivot_longer(c(rent_2016, black_ratio_2016)) |> 
  left_join(distinct(CT_geom, census_tract, .keep_all = TRUE), 
            by = "census_tract") |>
  st_as_sf(crs = 4326) |> 
  ggplot(aes(fill = value)) +
  geom_sf(colour = "transparent") +
  facet_wrap(~name) +
  scale_fill_viridis_b(limits = c(-2, 2), oob = scales::squish,
                       n.breaks = 7) +
  theme_void() +
  theme(legend.position = "right")

ggsave("output/ott_rent.png", ott_rent, width = 8, height = 5, 
       units = "in")


# Rent change maps --------------------------------------------------------

mtl_rent_change <-
  CT_scaled |> 
  filter(city == "Montreal") |> 
  select(census_tract, rent_change_2016, black_ratio_2016) |> 
  pivot_longer(c(rent_change_2016, black_ratio_2016)) |> 
  left_join(distinct(CT_geom, census_tract, .keep_all = TRUE), 
            by = "census_tract") |>
  st_as_sf(crs = 4326) |> 
  ggplot(aes(fill = value)) +
  geom_sf(colour = "transparent") +
  facet_wrap(~name) +
  scale_fill_viridis_b(limits = c(-2, 2), oob = scales::squish,
                       n.breaks = 7) +
  theme_void() +
  theme(legend.position = "right")

ggsave("output/mtl_rent_change.png", mtl_rent_change, width = 8, height = 5, 
       units = "in")

to_rent_change <-
  CT_scaled |> 
  filter(city == "Toronto") |> 
  select(census_tract, rent_change_2016, black_ratio_2016) |> 
  pivot_longer(c(rent_change_2016, black_ratio_2016)) |> 
  left_join(distinct(CT_geom, census_tract, .keep_all = TRUE), 
            by = "census_tract") |>
  st_as_sf(crs = 4326) |> 
  ggplot(aes(fill = value)) +
  geom_sf(colour = "transparent") +
  facet_wrap(~name) +
  scale_fill_viridis_b(limits = c(-2, 2), oob = scales::squish,
                       n.breaks = 7) +
  theme_void() +
  theme(legend.position = "right")

ggsave("output/to_rent_change.png", to_rent_change, width = 8, height = 5, 
       units = "in")

ott_rent_change <-
  CT_scaled |> 
  filter(city == "Ottawa") |> 
  select(census_tract, rent_change_2016, black_ratio_2016) |> 
  pivot_longer(c(rent_change_2016, black_ratio_2016)) |> 
  left_join(distinct(CT_geom, census_tract, .keep_all = TRUE), 
            by = "census_tract") |>
  st_as_sf(crs = 4326) |> 
  ggplot(aes(fill = value)) +
  geom_sf(colour = "transparent") +
  facet_wrap(~name) +
  scale_fill_viridis_b(limits = c(-2, 2), oob = scales::squish,
                       n.breaks = 7) +
  theme_void() +
  theme(legend.position = "right")

ggsave("output/ott_rent_change.png", ott_rent_change, width = 8, height = 5, 
       units = "in")
