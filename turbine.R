tuesdata <- tidytuesdayR::tt_load('2020-10-27')
turbines <- tuesdata[["wind-turbine"]]

library(tidyverse)

turbines_df<- turbines %>% 
        transmute(turbine_capacity = turbines, rotor_diameter_m,hub_height_m,
                  commissioning_date = parse_number(commissioning_date),
                  province_territory = fct_lump_n(province_territory, 9),
                  model = fct_lump_n(model,10)) %>% 
        filter(!is.na(turbine_capacity)) %>% 
        mutate_if(is.character, factor)


turbines_df %>% 
        select(turbine_capacity:commissioning_date) %>% 
        pivot_longer(rotor_diameter_m:commi)
