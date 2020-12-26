library(tidyverse)
library(scales)
crimes %>% 
        count(`STATE/UT`) %>% View()

crimes$`STATE/UT`<- toupper(crimes$`STATE/UT`)

crimes$`STATE/UT` <- replace(crimes$`STATE/UT`, crimes$`STATE/UT` == "DELHI UT", "DELHI")
crimes$`STATE/UT` <- replace(crimes$`STATE/UT`, crimes$`STATE/UT` == "D & N HAVELI", "D&N HAVELI")
crimes$`STATE/UT` <- replace(crimes$`STATE/UT`, crimes$`STATE/UT` == "A & N ISLANDS", "A&N ISLANDS")
crimes_states %>% View()



crimes_states %>% group_by(`STATE/UT`) %>% 
        summarise(max(Rape_tot))

crimes$crimes_total<- crimes$Rape + crimes$`Dowry Deaths` + crimes$`Kidnapping and Abduction` +crimes$`Assault on women with intent to outrage her modesty` +crimes$`Insult to modesty of Women` + crimes$`Cruelty by Husband or his Relatives` + crimes$`Importation of Girls`
        

crimes %>% 
        ggplot(aes(Year, crimes_total ))+
        geom_bar(stat = "identity", alpha =.5, fill = "red")


total<- crimes %>%
        filter(DISTRICT == "TOTAL" | DISTRICT == "Total District(s)" | DISTRICT =="ZZ TOTAL")

total$Year <- as.factor(total$Year)

total %>% 
        filter(`STATE/UT` == "KERALA") %>%
        ggplot(aes(Year, crimes_total))+
        geom_col(alpha =.5 , fill = "red")+
        ggtitle("Kerala Rape")



total<- total %>% 
        select(state = `STATE/UT`, district=DISTRICT, Year, Rape, kidnap =`Kidnapping and Abduction`,
               dowry_death = `Dowry Deaths`, assault = `Assault on women with intent to outrage her modesty`,
               insult_to_modesty =`Insult to modesty of Women`, cruelty = `Cruelty by Husband or his Relatives`,
               importation =`Importation of Girls`, crimes_total)

long_tot<- total %>% 
        pivot_longer(4:10,names_to = "case", values_to = "number")

long_tot$case <- as.factor(long_tot$case)

NotFancy <- function(l) {
        l <- format(l, scientific = FALSE)
        parse(text=l)
}
long_tot %>% 
        ggplot(aes(Year,crimes_total, fill = case))+
        geom_col()+
        scale_y_continuous(labels=comma)+
        coord_flip()+
        facet_wrap(~case)
