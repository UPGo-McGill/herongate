#### FINAL MODELS ##############################################################

# Value 2016
model_2016_value_fe <- 
  lm(value_2016 ~ rac_dis_ratio_2016 + non_perm_ratio_2016 + income_2016 + 
       detached_2016 + rooms_2016 +  p_children_2016 + p_college_2016 + 
       ph_dist + poverty_2016 + rent_2016 + rented_2016 + employed_2016 + city - 1, 
     data = CT_scaled)

summary(model_2016_value_fe)

# Value change
model_value_change_fe <- 
  lm(value_change_pct ~ value_2006 + rac_dis_ratio_2016 + non_perm_ratio_2016 + 
       income_2016 + detached_2016 + rooms_2016 +  p_children_2016 + 
       p_college_2016 + ph_dist + poverty_2016 + rent_2016 + rented_2016 +
       city - 1, data = CT_scaled)

summary(model_value_change_fe)

# Rent 2016
model_2016_rent_fe <- 
  lm(rent_2016 ~ rac_dis_ratio_2016 + non_perm_ratio_2016 + income_2016 + 
       detached_2016 + rooms_2016 +  p_children_2016 + p_college_2016 + 
       ph_dist + poverty_2016 + rented_2016 + city - 1, 
     data = CT_scaled)

summary(model_2016_rent_fe)

# Rent change
model_rent_change_fe <- 
  lm(rent_change_pct ~ rent_2006 + rac_dis_ratio_2016 + 
       rac_dis_ratio_change + non_perm_ratio_2016 + non_perm_ratio_change + 
       income_2016 + detached_2016 + rooms_2016 +  p_children_2016 + 
       p_college_2016 + ph_dist + poverty_2016 + rented_2016 +
       city - 1, data = CT_scaled)

summary(model_rent_change_fe)

# Multi-family value 2016
model_2016_mpac <-
  lm(val_per_unit_2016 ~ bed_3 + n_2016 + avg_units_2016 + rac_dis_ratio_2016 + 
       non_perm_ratio_2016 + income_2016 + detached_2016 + rooms_2016 + 
       p_children_2016 + p_college_2016 + ph_dist + poverty_2016 + rent_2016 + 
       rented_2016
     + employed_2016 + density_2016
     , data = mpac_scaled)

summary(model_2016_mpac)

# Multi-family value change
model_change_mpac <-
  lm(val_per_unit_change ~ bed_3 + n_2016 + avg_units_2016 + val_per_unit_2008 + 
       rac_dis_ratio_2016 + non_perm_ratio_2016 + income_2016 + detached_2016 + 
       rooms_2016 + p_children_2016 + p_college_2016 + ph_dist + poverty_2016 + 
       rent_2016 + rented_2016, data = mpac_scaled)

summary(model_change_mpac)
