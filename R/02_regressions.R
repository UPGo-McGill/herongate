#### HERONGATE REGRESSIONS #####################################################

library(lme4)
library(lmerTest)


# Descriptives ------------------------------------------------------------

CT_scaled |> 
  ggplot(aes(black_ratio_2016, value_2016)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  theme_minimal()

CT_scaled |> 
  select(black_ratio_2016, value_change_pct)

CT_scaled |> 
  ggplot(aes(black_ratio_2016, value_change_pct)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  theme_minimal()


# 2016 value --------------------------------------------------------------

# Black ratio + effect on home values
model_2016_value <- 
  lm(value_2016 ~ black_ratio_2016 + income_2016 + detached_2016 +
     rooms_2016 + p_children_2016 + p_college_2016 + ph_dist, data = CT_scaled)

summary(model_2016_value)

# MLM
model_2016_value_mlm <- 
  lmer(value_2016 ~ 1 + black_ratio_2016 + income_2016 + detached_2016 +
       rooms_2016 + p_children_2016 + p_college_2016 + ph_dist + (1 | city), 
     data = CT_scaled)

summary(model_2016_value_mlm)

model_2016_value_fe <- 
  lm(value_2016 ~ black_ratio_2016 + income_2016 + detached_2016 +
         rooms_2016 + p_children_2016 + p_college_2016 + ph_dist + city - 1, 
       data = CT_scaled)

summary(model_2016_value_fe)

value_out <- stargazer::stargazer(model_2016_value_fe, type = "html")
write_lines(value_out, "output/value_2016.html")


  
  





# Value change ------------------------------------------------------------

# Black ratio weak - effect on value change
model_value_change <- 
  lm(value_change_pct ~ value_2006 + black_ratio_2016 + black_ratio_change + 
       income_2016 + rooms_2016 + detached_2016 + p_college_2016 + 
       p_children_2016 + ph_dist, data = CT_scaled)
  
summary(model_value_change)

model_2016_value_change_mlm <- 
  lmer(value_change_pct ~ 1 + value_2006 + black_ratio_2016 + black_ratio_change + 
         income_2016 + detached_2016 + rooms_2016 + p_children_2016 + 
         p_college_2016 + ph_dist + (1 | city), 
       data = CT_scaled)

summary(model_2016_value_change_mlm)

model_value_change_fe <- 
  lm(value_change_pct ~ value_2006 + black_ratio_2016 + black_ratio_change + 
       income_2016 + detached_2016 + rooms_2016 + p_children_2016 + 
       p_college_2016 + ph_dist + city - 1, data = CT_scaled)

summary(model_value_change_fe)




# 2016 rent ---------------------------------------------------------------

# Black ratio + effect on home values
model_2016_rent <- 
  lm(rent_2016 ~ black_ratio_2016 + income_2016 + detached_2016 +
       rooms_2016 + p_children_2016 + p_college_2016 + ph_dist, data = CT_scaled)

summary(model_2016_rent)

# MLM
model_2016_value_mlm <- 
  lmer(value_2016 ~ 1 + black_ratio_2016 + income_2016 + detached_2016 +
         rooms_2016 + p_children_2016 + p_college_2016 + ph_dist + (1 | city), 
       data = CT_scaled)

summary(model_2016_value_mlm)

# FE
model_2016_rent_fe <- 
  lm(rent_2016 ~ black_ratio_2016 + income_2016 + detached_2016 +
         rooms_2016 + p_children_2016 + p_college_2016 + ph_dist + city - 1, 
       data = CT_scaled)

summary(model_2016_rent_fe)



# Rent change -------------------------------------------------------------

# Black ratio strong - effect on rent change, but no effect of black_ratio_change
model_rent_change <- 
  lm(rent_change_2016 ~ value_2006 + black_ratio_2016 + black_ratio_change + 
       income_2016 + rooms_2016 + detached_2016 + p_college_2016 + 
       p_children_2016 + ph_dist, data = CT_scaled)

summary(model_rent_change)

model_rent_change_mlm <- 
  lmer(rent_change_2016 ~ 1 + value_2006 + black_ratio_2016 + black_ratio_change + 
       income_2016 + rooms_2016 + detached_2016 + p_college_2016 + 
       p_children_2016 + ph_dist + (1 | city), data = CT_scaled)

summary(model_rent_change_mlm)

model_rent_change_fe <- 
  lm(rent_change_2016 ~ value_2006 + black_ratio_2016 + black_ratio_change + 
       income_2016 + rooms_2016 + detached_2016 + p_college_2016 + 
       p_children_2016 + ph_dist + city - 1, data = CT_scaled)

summary(model_rent_change_fe)




summary(model_2016_value)
summary(model_2016_value_mlm)
summary(model_value_change)
summary(model_value_change_mlm)
summary(model_rent_change)
summary(model_rent_change_mlm)







