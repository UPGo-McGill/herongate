#### HERONGATE REGRESSIONS #####################################################

lm(value_change ~ value_2006 + rooms_2016 + detached_2016 + income_change +
     p_college_2016 + p_children_2016 + white_stable + mixed_stable + 
     mixed_transition, data = CT_final) |> 
  summary()

lm(value_change2 ~ value_2006 + rooms_2016 + detached_2016 + income_change +
     p_college_2016 + p_children_2016 + white_stable + mixed_stable + 
     mixed_transition, data = CT_final) |> 
  summary()

lm(value_change2 ~ value_2006 + rooms_2016 + detached_2016 + income_change2 +
     p_college_2016 + p_children_2016 + white_stable + mixed_stable + 
     mixed_transition, data = CT_final) |> 
  summary()