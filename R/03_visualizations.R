#### VISUALIZATIONS ############################################################

CT_2006 <- get_census("CA06", regions = list(CMA = "505"), level = "CT",
                      geo_format = "sf")

CT_2016 <- get_census("CA16", regions = list(CMA = "505"), level = "CT",
                      geo_format = "sf")

ggplot() +
  geom_sf(data = CT_2016, colour = "blue", fill = "transparent") +
  geom_sf(data = CT_2006, colour = "red", fill = "transparent") +
  geom_sf(data = CT_raw, colour = "purple", fill = "transparent") +
  theme_void()