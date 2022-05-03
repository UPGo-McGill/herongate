CT_raw_VAN <- get_tongfen_ca_census(
  regions = list(CMA = "59933"), 
  meta = meta_for_ca_census_vectors(c(vec_2006, vec_2016)),
  level = "CT",
  base_geo = "CA16")

CT_geom_VAN <- 
  CT_raw_VAN |> 
  select(TongfenUID, geometry)

CT_VAN <- 
  CT_raw_VAN |> 
  pivot_longer(-c(TongfenID:geometry),
               names_to = c("variable", "year"),
               names_sep = "___") |> 
  st_drop_geometry() |> 
  pivot_wider(names_from = variable, values_from = value) |> 
  relocate(under17, .after = total_children) |> 
  mutate(under17 = coalesce(under17, u17_1 + u17_2 + u17_3)) |>
  select(-u17_1, -u17_2, -u17_3) |> 
  mutate(prior1960 = coalesce(prior1960, built1 + built2),
         prior1980 = coalesce(prior1980, built3 + built4),
         prior1990 = coalesce(prior1990, built5 + built6),
         prior2000 = coalesce(prior2000, built7 + built8),
         prior2006 = coalesce(prior2006, built9)) |> 
  relocate(prior1960:prior2016, .after = total_built) |> 
  select(-c(built1:built9)) |> 
  mutate(minor = coalesce(minor, minor1 + minor2)) |> 
  select(-minor1, -minor2) |> 
  relocate(minor, .after = major) |> 
  left_join(CT_geom_VAN, by = "TongfenUID") |> 
  rename(census_tract = TongfenID) |> 
  select(-TongfenUID) |> 
  st_as_sf()

CT_geom_VAN <- 
  CT_VAN |> 
  select(census_tract, geometry)

CT_VAN <- 
  CT_VAN |> 
  st_drop_geometry()


# Clean data --------------------------------------------------------------

CT_VAN <- 
  CT_VAN |> 
  # Drop CTs with low population
  filter(!is.na(population), population >= 100) |> 
  # Drop CTs only present in one year
  group_by(census_tract) |> 
  filter(n() == 2) |> 
  ungroup() |> 
  # Create percentage variables
  mutate(p_college = degree / total_students,
         p_children = under17 / total_children, # Previously was under17 / population
         p_detached = detached / total_houses,
         p_racialized = (vismin + indigenous) / total_race,
         p_black = black / total_race,
         p_white = (notvismin - indigenous) / total_race)

CT_VAN <- 
  CT_VAN |> 
  # Create change in value and p_racialized for 2016
  group_by(census_tract) |> 
  mutate(value_change = value - value[year == "2006"], 
         value_change2 = value_change / value[year == "2006"],
         income_change = income - income[year == "2006"],
         income_change2 = income_change / income[year == "2006"],
         racialized_change = p_racialized - p_racialized[year == "2006"]) |> 
  ungroup() |> 
  # Create nbhd composition/transition variables
  mutate(composition = p_white >= 0.7,
         transition = racialized_change >= 0.1)

# Create neighbourhood_type variable
CT_VAN <- 
  CT_VAN |> 
  mutate(neighbourhood_type = case_when(
    composition & !transition ~ "white_stable",
    composition & transition ~ "white_transition",
    !composition & !transition ~ "mixed_stable",
    !composition & transition ~ "mixed_transition"))


# Pivot data for regressions ----------------------------------------------

CT_final_VAN <-
  CT_VAN |> 
  pivot_wider(names_from = year, values_from = 3:71) |> 
  select(-value_change_2006, -value_change2_2006, -income_change_2006,
         -income_change2_2006, -racialized_change_2006, -transition_2006,
         -neighbourhood_type_2006) |> 
  rename(value_change = value_change_2016,
         value_change2 = value_change2_2016,
         income_change = income_change_2016,
         income_change2 = income_change2_2016,
         racialized_change = racialized_change_2016,
         transition = transition_2016,
         neighbourhood_type = neighbourhood_type_2016)

CT_final_VAN <- 
  CT_final_VAN |> 
  mutate(white_stable = neighbourhood_type == "white_stable",
         white_transition = neighbourhood_type == "white_transition",
         mixed_stable = neighbourhood_type == "mixed_stable",
         mixed_transition = neighbourhood_type == "mixed_transition")


# Add new race variable ---------------------------------------------------

CT_final_VAN <- 
  CT_final_VAN |> 
  mutate(black_ratio_2006 = p_black_2006 / 
           (sum(p_black_2006 * total_race_2006, na.rm = TRUE) / 
              sum(total_race_2006, na.rm = TRUE)),
         black_ratio_2016 = p_black_2016 / 
           (sum(p_black_2016 * total_race_2016, na.rm = TRUE) / 
              sum(total_race_2016, na.rm = TRUE)))

# Add distance to downtown -----------------------------------------

ph_VAN <- st_point(c(-123.1182923162272, 49.28260752585029)) |> 
  st_sfc(crs = 4326) |> 
  st_transform(32610)

ph_dist_VAN <- 
  CT_geom_VAN |> 
  distinct(census_tract, .keep_all = TRUE) |> 
  mutate(ph_dist = as.numeric(st_distance(st_transform(geometry, 32610), ph_VAN))) |> 
  st_drop_geometry()

CT_final_VAN <- 
  CT_final_VAN |> 
  left_join(ph_dist_VAN, by = "census_tract")

