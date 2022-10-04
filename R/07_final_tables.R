#### Final tables

# Canada/CMA demographics
CMA <- get_census("CA16", regions = list(
  CMA = c("35535", "24462", "59933", "48825", "505", "48835")),
  vectors = set_names(vec_2016, str_remove(names(vec_2016), "___2016")))

Canada <- get_census("CA16", regions = list(C = "01"), vectors = set_names(
  vec_2016, str_remove(names(vec_2016), "___2016")))

CMA <- bind_rows(Canada, CMA)

CMA |> 
  mutate(p_college = degree / total_students,
         p_children = under17 / total_children, # Previously was under17 / population
         p_detached = detached / total_houses,
         p_racialized = (vismin + indigenous) / total_race,
         p_black = black / total_race,
         p_white = (notvismin - indigenous) / total_race,
         p_non_perm = non_perm / total_imm,
         p_rac_dis = (indigenous + black + arab + latin + filipino + 
                        southasian + westasian) / total_race) |> 
  select(region = `Region Name`, population = Population, p_rac_dis, 
         p_non_perm, value, income, rent, p_detached, p_college) |> 
  mutate(region = str_remove(region, " \\(.*\\)")) |> 
  mutate(
    population = scales::comma(population, 1000),
    p_rac_dis = scales::percent(p_rac_dis, 0.1),
    p_non_perm = scales::percent(p_non_perm, 0.1),
    value = scales::dollar(value, 100),
    income = scales::dollar(income, 10),
    rent = scales::dollar(rent),
    p_detached = scales::percent(p_detached, 0.1),
    p_college = scales::percent(p_college, 0.1),
  ) |> 
  t() |> 
  as_tibble(rownames = "var") |> 
  gt::gt()
  



# Summary statistics
CT_final |> 
  select(value_2016, value_2006, value_change_pct, rent_2016, rent_2006,
         rent_change_pct, rac_dis_ratio_2016, rac_dis_ratio_change,
         non_perm_ratio_2016, non_perm_ratio_change, income_2016, 
         detached_2016, rooms_2016, p_children_2016, p_college_2016,
         ph_dist, poverty_2016, rented_2016) |> 
  mutate(across(where(is.numeric), ~ifelse(is.infinite(.x), NA_real_, .x))) |> 
  tidyr::drop_na() |> 
  summarize(across(where(is.numeric), list("_min" = min, "_max" = max, 
                                           "_mean" = mean, "_sd" = sd))) |> 
  pivot_longer(cols = everything(), names_to = c("var", "stat"), 
               names_sep = "__") |> 
  pivot_wider(names_from = stat, values_from = value) |> 
  bind_rows(
    mpac_scaled |> 
      select(val_per_unit_2016, val_per_unit_change, bed_3, n_2016,
             avg_units_2016, val_per_unit_2008) |> 
      mutate(across(where(is.numeric), ~ifelse(is.infinite(.x), NA_real_, .x))) |> 
      tidyr::drop_na() |> 
      summarize(across(where(is.numeric), list("_min" = min, "_max" = max, 
                                               "_mean" = mean, "_sd" = sd))) |> 
      pivot_longer(cols = everything(), names_to = c("var", "stat"), 
                   names_sep = "__") |> 
      pivot_wider(names_from = stat, values_from = value)
  ) |> 
  mutate(across(where(is.numeric), round, 2)) |> 
  gt::gt()

