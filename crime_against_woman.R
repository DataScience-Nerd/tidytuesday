library(tidyverse)

crimes_states<- crimes %>% 
        group_by(`STATE/UT`) %>% 
        summarise(Rape_tot = max(Rape))

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

crimes_states %>% group_by(`STATE/UT`) %>% 
        summarise(max(Rape_tot))

crimes$crimes_total<- crimes$Rape + crimes$`Dowry Deaths` + crimes$`Kidnapping and Abduction` +crimes$`Assault on women with intent to outrage her modesty` +crimes$`Insult to modesty of Women` + crimes$`Cruelty by Husband or his Relatives` + crimes$`Importation of Girls`
        

crimes %>% 
        ggplot(aes(Year, crimes_total ))+
        geom_bar(stat = "identity", alpha =.5, fill = "red")


total<- crimes %>%
        filter(DISTRICT == "TOTAL")
