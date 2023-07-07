#### MPAC DATA #################################################################

library(tidyverse)
library(sf)
library(tidygeocoder)
library(qs)


# Import data -------------------------------------------------------------

mpac <- read_csv("data/ottawa_mpac.csv")


# Geolocate addresses -----------------------------------------------------

# mpac_addresses <- 
#   mpac |> 
#   select(street = LOCATIONAL_ADDRESS) |> 
#   mutate(city = "OTTAWA", state = "ONTARIO", country = "CANADA") |> 
#   mutate(street = str_remove(street, "-\\d*")) |> 
#   mutate(street = str_remove(street, " APT.*$")) |> 
#   mutate(street = str_remove(street, " UNIT.*$")) |> 
#   mutate(street = str_remove(street, " L3.*$")) |> 
#   mutate(street = str_remove(street, " SUITE.*$")) |> 
#   distinct()
# 
# mpac_addresses <- 
#   mpac_addresses |> 
#   tidygeocoder::geocode(street = street, city = city, state = state,
#                         country = country, method = "osm")
# 
# qsave(mpac_addresses, file = "data/mpac_geocode.qs")
# # mpac_addresses <- qread("data/mpac_geocode.qs")
# 
# # Manual tweaks
# extra <- 
#   mpac_addresses |> 
#   filter(is.na(lat)) |> 
#   mutate(street = case_when(
#     street == "300 GOULBURN PVT" ~ "300 GOULBURN AVE",
#     street == "387 CARMEN ST" ~ "387 CARMEN AVE",
#     street == "378 RICHELIEU CRT" ~ "378 RICHELIEU CT",
#     TRUE ~ street)) |> 
#   select(-lat, -long) |> 
#   tidygeocoder::geocode(street = street, city = city, state = state,
#                         country = country, method = "osm") |> 
#   mutate(lat = if_else(street == "378 RICHELIEU CT", 45.43508977966809, lat),
#          long = if_else(street == "378 RICHELIEU CT", -75.6530693013985, long))
# 
# mpac_addresses <- 
#   mpac_addresses |> 
#   filter(!is.na(lat)) |> 
#   bind_rows(extra) |> 
#   distinct()
# 
# qsave(mpac_addresses, file = "data/mpac_geocode.qs")
mpac_addresses <- qread("data/mpac_geocode.qs")


# Connect geolocated coordinates to original data -------------------------

ottawa_geom <- 
  CT_geom |> 
  filter(census_tract %in% (filter(CT, city == "Ottawa"))$census_tract)

valid <- 
  mpac_addresses |> 
  st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
  st_filter(ottawa_geom) |> 
  select(street, geometry)

mpac <- 
  mpac |> 
  mutate(street = LOCATIONAL_ADDRESS) |> 
  mutate(street = str_remove(street, "-\\d*")) |> 
  mutate(street = str_remove(street, " APT.*$")) |> 
  mutate(street = str_remove(street, " UNIT.*$")) |> 
  mutate(street = str_remove(street, " L3.*$")) |> 
  mutate(street = str_remove(street, " SUITE.*$")) |> 
  mutate(street = case_when(
    street == "300 GOULBURN PVT" ~ "300 GOULBURN AVE",
    street == "387 CARMEN ST" ~ "387 CARMEN AVE",
    street == "378 RICHELIEU CRT" ~ "378 RICHELIEU CT",
    TRUE ~ street)) |> 
  left_join(valid, by = "street") |> 
  filter(!st_is_empty(geometry))


# Process data then join to CTs -------------------------------------------

mpac_scaled <-
  mpac |> 
  mutate(bed_3 = (BEDROOM_3 + BEDROOM_4 + BEDROOM_5) / 
           (BACHELOR + BEDROOM_1 + BEDROOM_2 + BEDROOM_3 + BEDROOM_4 + 
              BEDROOM_5)) |> 
  select(ID = ASSESSMENT_ROLL_NUMBER, 
         year = TAX_YEAR,
         property_code = PROPERTY_CODE_WITH_DESCRIPTION,
         units = NUMBER_OF_UNITS,
         value = CURRENT_ASSESSED_VALUE,
         bed_3,
         geometry) |> 
  # filter(units >= 30) |>
  mutate(val_per_unit = value / units, .after = value) |> 
  st_as_sf(crs = 4326) |> 
  st_join(ottawa_geom) |> 
  st_drop_geometry() |> 
  distinct() |> 
  pivot_wider(names_from = year, values_from = c(units, value, val_per_unit)) |> 
  group_by(census_tract) |> 
  summarize(n_2008 = sum(!is.na(units_2008)),
            n_2016 = sum(!is.na(units_2016)),
            avg_units_2008 = mean(units_2008, na.rm = TRUE),
            avg_units_2016 = mean(units_2016, na.rm = TRUE),
            n_units_2008 = sum(units_2008, na.rm = TRUE),
            n_units_2016 = sum(units_2016, na.rm = TRUE),
            val_per_unit_2008 = sum(value_2008, na.rm = TRUE) / n_units_2008,
            val_per_unit_2016 = sum(value_2016, na.rm = TRUE) / n_units_2016,
            bed_3 = mean(bed_3)) |> 
  mutate(val_per_unit_change = val_per_unit_2016 - val_per_unit_2008, 
         .after = val_per_unit_2016) |> 
  mutate(across(c(n_units_2008:val_per_unit_change), scale_fun)) |> 
  left_join(CT_scaled)
  
mpac_scaled_units <-
  mpac |> 
  select(ID = ASSESSMENT_ROLL_NUMBER, 
         year = TAX_YEAR,
         property_code = PROPERTY_CODE_WITH_DESCRIPTION,
         units = NUMBER_OF_UNITS,
         val = CURRENT_ASSESSED_VALUE,
         geometry) |> 
  filter(units >= 20) |>
  mutate(val_per_unit = val / units, .after = val) |> 
  st_as_sf(crs = 4326) |> 
  st_join(ottawa_geom) |> 
  st_drop_geometry() |> 
  distinct() |> 
  pivot_wider(names_from = year, values_from = c(units, val, val_per_unit)) |> 
  mutate(val_per_unit_2008 = val_2008 / units_2008,
         val_per_unit_2016 = val_2016 / units_2016,
         val_per_unit_change = val_per_unit_2016 - val_per_unit_2008) |> 
  mutate(across(val_per_unit_2008:val_per_unit_change, scale_fun)) |> 
  inner_join(CT_scaled)

# qsave(mpac_scaled, file = "output/mpac_scaled.qs")
