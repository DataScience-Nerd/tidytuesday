library(tidyverse)

crimes_states<- crimes %>% 
        group_by(`STATE/UT`) %>% 
        summarise(Rape_tot = sum(Rape))

crimes %>% 
        count(`STATE/UT`) %>% View()


crimes$`STATE/UT`<- toupper(crimes$`STATE/UT`)

crimes_states %>% View()
crimes$`STATE/UT` <- replace(crimes$`STATE/UT`, crimes$`STATE/UT` == "DELHI UT", "DELHI")
crimes$`STATE/UT` <- replace(crimes$`STATE/UT`, crimes$`STATE/UT` == "D & N HAVELI", "D&N HAVELI")
crimes$`STATE/UT` <- replace(crimes$`STATE/UT`, crimes$`STATE/UT` == "A & N ISLANDS", "A&N ISLANDS")

crimes_states %>% 
        ggplot(aes(reorder(`STATE/UT`, Rape_tot), Rape_tot))+
        geom_bar(stat = "identity")+
        coord_flip()
