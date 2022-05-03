#### HERONGATE DATA IMPORT #####################################################

library(tidyverse)
library(sf)
library(cancensus)
library(tongfen)


# List 2006 vectors -------------------------------------------------------

vec_2006 <- c(
  population = "v_CA06_1",
  total_students = "v_CA06_1248",
  degree = "v_CA06_1254",
  income = "v_CA06_2000",
  total_children = "v_CA06_78",
  u17_1 = "v_CA06_79",
  u17_2 = "v_CA06_80",
  u17_3 = "v_CA06_81",
  total_marital = "v_CA06_41",
  married = "v_CA06_43",
  total_language = "v_CA06_243",
  english = "v_CA06_244",
  french = "v_CA06_245",
  bilingual = "v_CA06_246",
  total_lf = "v_CA06_575",
  lf = "v_CA06_576",
  employed = "v_CA06_577",
  unemployed = "v_CA06_578",
  poverty = "v_CA06_1981",
  rooms = "v_CA06_99",
  total_built = "v_CA06_109",
  built1 = "v_CA06_110",
  built2 = "v_CA06_111",
  built3 = "v_CA06_112",
  built4 = "v_CA06_113",
  built5 = "v_CA06_114",
  built6 = "v_CA06_115",
  built7 = "v_CA06_116",
  built8 = "v_CA06_117",
  built9 = "v_CA06_118",
  total_houses = "v_CA06_119",
  detached = "v_CA06_120",
  semidetached = "v_CA06_121",
  row = "v_CA06_122",
  duplex = "v_CA06_123", # This was previously called "apartment"
  big_apartment = "v_CA06_124",
  small_apartment = "v_CA06_125",
  value = "v_CA06_2054",
  occupied_dwellings = "v_CA06_101",
  owned = "v_CA06_102",
  rented = "v_CA06_103",
  band_housing = "v_CA06_104",
  total_repairs = "v_CA06_105",
  minor1 = "v_CA06_106",
  minor2 = "v_CA06_107",
  major = "v_CA06_108",
  total_race = "v_CA06_1302",
  indigenous = "v_CA06_565",
  vismin = "v_CA06_1303",
  chinese = "v_CA06_1304",
  southasian = "v_CA06_1305",
  black = "v_CA06_1306",
  filipino = "v_CA06_1307",
  latin = "v_CA06_1308",
  southeast = "v_CA06_1309",
  arab = "v_CA06_1310",
  westasian = "v_CA06_1311",
  korean = "v_CA06_1312",
  japanese = "v_CA06_1313",
  notvismin = "v_CA06_1316"
  )


# List 2016 vectors -------------------------------------------------------

vec_2016 <- c(
  population = "v_CA16_401",
  total_students = "v_CA16_5096",
  degree = "v_CA16_5105", # Previously had v_CA16_5117 (non-university certificate)
  income = "v_CA16_2397", # Previously had v_CA16_2398 (after-tax hh income)
  total_children = "v_CA16_2510", # Not comparable to 2006 children variable
  under17 = "v_CA16_2513", # Not comparable to 2006 children variable
  total_marital = "v_CA16_451",
  married = "v_CA16_457", # Previously had v_CA16_454 (married & common law)
  total_language = "v_CA16_512",
  english = "v_CA16_515",
  french = "v_CA16_518",
  bilingual = "v_CA16_521",
  total_lf = "v_CA16_5597",
  lf = "v_CA16_5600",
  employed = "v_CA16_5603",
  unemployed = "v_CA16_5606",
  poverty = "v_CA16_2570",
  rooms = "v_CA16_4855",
  total_built = "v_CA16_4862",
  prior1960 = "v_CA16_4863",
  prior1980 = "v_CA16_4864",
  prior1990 = "v_CA16_4865",
  prior2000 = "v_CA16_4866",
  prior2006 = "v_CA16_4867",
  prior2011 = "v_CA16_4868",
  prior2016 = "v_CA16_4869",
  total_houses = "v_CA16_408",
  detached = "v_CA16_409",
  semidetached = "v_CA16_412",
  row = "v_CA16_413",
  duplex = "v_CA16_414", # Previously had "apartment" (v_CA06_123) which was actually duplex
  big_apartment = "v_CA16_410", 
  small_apartment = "v_CA16_415",
  value = "v_CA16_4896",
  occupied_dwellings = "v_CA16_4836",
  owned = "v_CA16_4837",
  rented = "v_CA16_4838",
  band_housing = "v_CA16_4839",
  total_repairs = "v_CA16_4870",
  minor = "v_CA16_4871",
  major = "v_CA16_4872",
  total_race = "v_CA16_3954",
  indigenous = "v_CA16_3855",
  vismin = "v_CA16_3957",
  chinese = "v_CA16_3963",
  southasian = "v_CA16_3960",
  black = "v_CA16_3966",
  filipino = "v_CA16_3969",
  latin = "v_CA16_3972",
  southeast = "v_CA16_3978",
  arab = "v_CA16_3975",
  westasian = "v_CA16_3981",
  korean = "v_CA16_3984",
  japanese = "v_CA16_3987",
  notvismin = "v_CA16_3996"
  )


# Build dataset -----------------------------------------------------------

names(vec_2006) <- paste(names(vec_2006), "2006", sep = "___")
names(vec_2016) <- paste(names(vec_2016), "2016", sep = "___")

CT_raw <- get_tongfen_ca_census(
  regions = list(CMA = "505"), 
  meta = meta_for_ca_census_vectors(c(vec_2006, vec_2016)),
  level = "CT",
  base_geo = "CA16")

CT_geom <- 
  CT_raw |> 
  select(TongfenUID, geometry)

CT <- 
  CT_raw |> 
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
  left_join(CT_geom, by = "TongfenUID") |> 
  rename(census_tract = TongfenID) |> 
  select(-TongfenUID) |> 
  st_as_sf()

CT_geom <- 
  CT |> 
  select(census_tract, geometry)

CT <- 
  CT |> 
  st_drop_geometry()


# Clean data --------------------------------------------------------------

CT <- 
  CT |> 
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

CT <- 
  CT |> 
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
CT <- 
  CT |> 
  mutate(neighbourhood_type = case_when(
    composition & !transition ~ "white_stable",
    composition & transition ~ "white_transition",
    !composition & !transition ~ "mixed_stable",
    !composition & transition ~ "mixed_transition"))


# Pivot data for regressions ----------------------------------------------

CT_final <-
  CT |> 
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

CT_final <- 
  CT_final |> 
  mutate(white_stable = neighbourhood_type == "white_stable",
         white_transition = neighbourhood_type == "white_transition",
         mixed_stable = neighbourhood_type == "mixed_stable",
         mixed_transition = neighbourhood_type == "mixed_transition")


# Add new race variable ---------------------------------------------------

CT_final <- 
  CT_final |> 
  mutate(black_ratio_2006 = p_black_2006 / 
           (sum(p_black_2006 * total_race_2006, na.rm = TRUE) / 
              sum(total_race_2006, na.rm = TRUE)),
         black_ratio_2016 = p_black_2016 / 
           (sum(p_black_2016 * total_race_2016, na.rm = TRUE) / 
              sum(total_race_2016, na.rm = TRUE)))

CT_final <- 
  CT_final |> 
  mutate(black_ratio_change = black_ratio_2016 - black_ratio_2006)


# Add distance to Parliament Hill -----------------------------------------

ph <- st_point(c(-75.70055444964218, 45.42343521835774)) |> 
  st_sfc(crs = 4326) |> 
  st_transform(32618)

ph_dist <- 
  CT_geom |> 
  distinct(census_tract, .keep_all = TRUE) |> 
  mutate(ph_dist = as.numeric(st_distance(st_transform(geometry, 32618), ph))) |> 
  st_drop_geometry()

CT_final <- 
  CT_final |> 
  left_join(ph_dist, by = "census_tract")

CT_final$ph_dist