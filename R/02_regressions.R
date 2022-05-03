#### HERONGATE REGRESSIONS #####################################################

# Descriptives ------------------------------------------------------------

CT_final$neighbourhood_type |> table()
CT_final$white_stable |> table()

CT_final |> 
  ggplot(aes(black_ratio_2016, value_2016)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  theme_minimal()

CT_final |> 
  ggplot(aes(black_ratio_2016, value_change2)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  theme_minimal()


# 2016 value --------------------------------------------------------------

# Black ratio + effect on home values
model_2016_value <- 
  lm(value_2016 ~ black_ratio_2016 + income_2016 + detached_2016 +
     rooms_2016 + p_children_2016 + p_college_2016 + ph_dist, data = CT_final)

summary(model_2016_value)

# Missing value
missing_val_2016 <- which(!1:243 %in% names(model_2016_value$residuals))

CT_final |> 
  slice(-missing_val_2016) |> 
  mutate(residuals = model_2016_value$residuals) |> 
  left_join(CT_geom, by = "census_tract") |> 
  st_as_sf() |> 
  ggplot() +
  geom_sf(aes(fill = residuals), colour = "white", lwd = 0.2) +
  scale_fill_fermenter(palette = "Spectral") +
  theme_minimal()

# Toronto: black ratio strong - effect
lm(value_2016 ~ black_ratio_2016 + income_2016 + detached_2016 +
     rooms_2016 + p_children_2016 + p_college_2016 + ph_dist, data = CT_final_TO) |> 
  summary()

# Vancouver: black ratio strong - effect
lm(value_2016 ~ black_ratio_2016 + income_2016 + detached_2016 +
     rooms_2016 + p_children_2016 + p_college_2016 + ph_dist, data = CT_final_VAN) |> 
  summary()

# All CTs: black ratio + effect
lm(value_2016 ~ black_ratio_2016 + income_2016 + detached_2016 +
     rooms_2016 + p_children_2016 + p_college_2016, data = CT_final_all) |> 
  summary()


# Value change ------------------------------------------------------------

# Black ratio weak - effect on value change
model_change <- 
  lm(value_change2 ~ value_2006 + black_ratio_2016 + black_ratio_change + income_2016 + rooms_2016 + 
       detached_2016 + p_college_2016 + p_children_2016 + ph_dist, data = CT_final)
  
summary(model_change)

# Missing value
missing_val_change <- which(!1:243 %in% names(model_change$residuals))

CT_final |> 
  slice(-missing_val_change) |> 
  mutate(residuals = model_change$residuals) |> 
  left_join(CT_geom, by = "census_tract") |> 
  st_as_sf() |> 
  ggplot() +
  geom_sf(aes(fill = residuals), colour = "white", lwd = 0.2) +
  scale_fill_fermenter(palette = "Spectral") +
  theme_minimal()

# TO: black ratio strong - effect
lm(value_change2 ~ value_2006 + black_ratio_2016 + black_ratio_2006 + 
     income_2016 + rooms_2016 + detached_2016 + p_college_2016 + 
     p_children_2016 + ph_dist, 
   data = filter(CT_final_TO, !is.nan(value_change2), !is.infinite(value_change2))) |> 
  summary()

# VAN: black ratio strong - effect
lm(value_change2 ~ value_2006 + black_ratio_2016 + income_2016 + rooms_2016 + 
     detached_2016 + p_college_2016 + p_children_2016 + ph_dist, 
   data = CT_final_VAN) |> 
  summary()

# All CTs: strong - effect
lm(value_change2 ~ value_2006 + black_ratio_2016 + income_2016 + rooms_2016 + 
     detached_2016 + p_college_2016 + p_children_2016, 
   data = filter(CT_final_all, !is.nan(value_change2), !is.infinite(value_change2))) |> 
  summary()










lm(value_change2 ~ value_2006 + rooms_2016 + detached_2016 + income_change +
     p_college_2016 + p_children_2016 + white_stable + mixed_stable + 
     mixed_transition, data = CT_final) |> 
  summary()

lm(value_change2 ~ value_2006 + rooms_2016 + detached_2016 + income_change2 +
     p_college_2016 + p_children_2016 + white_stable + mixed_stable + 
     mixed_transition, data = CT_final) |> 
  summary()


lm(value_2016 ~ p_white_2016 + rooms_2016 + detached_2016 + income_2016,
   data = CT_final) |> 
  summary()


lm(value_change ~ value_2006 + black_ratio_2016 + income_2016 + detached_2016 +
     rooms_2016 + p_children_2016 + p_college_2016, data = CT_final) |> 
  summary()




lm(value_2016 ~ black_ratio_2016, data = CT_final_TO) |> 
  summary()


