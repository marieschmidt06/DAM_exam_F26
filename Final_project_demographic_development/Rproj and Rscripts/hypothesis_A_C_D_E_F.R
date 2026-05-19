#load packages
library(tidyverse)
library(readr)
library(here)
library(dplyr)
library(gganimate)
library(gifski)
library(av)
library(ggplot2)

#read CSV files
total_1801 <- read_csv2("data/1801_total.csv", na="NULL")
total_1834 <- read_csv2("data/1834_total.csv", na="NULL")
total_1860 <- read_csv2("data/1860_total.csv", na="NULL")

#create objects with selected rows
  #add column for shire in 1801 and 1834
clean_1801 <- total_1801 %>%
  filter(amt=="Århus") %>% 
  select(amt, sogn, koen, alder, civilstand) %>% 
  mutate(
    herred = case_when(
      sogn %in% c("Lyngå", "Hadsten", "Vitten", "Haldum","Foldby", "Lading", "Sabro", "Fårup") ~ "Sabro",
      sogn %in% c("Spørring","Grundfør","Trige","Søften","Ølsted","Elev","Elsted","Lisbjerg") ~ "Vester Lisbjerg",
      sogn %in% c("Skivholme","Borum","Sjelle","Skørring","Galten","Skovby","Framlev","Storring","Harlev","Stjær") ~ "Framlev",
      sogn %in% c("Kasted","Skejby","Vejlby","Tilst","Lyngby","Årslev","Brabrand","Hasle","Åby","Århus Købstad") ~ "Hasle",
      sogn %in% c("Ormslev","Kolt","Viby","Holme","Tranbjerg","Tiset","Mårslet","Beder","Malling","Astrup","Tulstrup","Tunø") ~ "Ning",
      sogn %in% c("Odder","Hvilsted","Torrild","Nølev","Saksild","Bjerager","Randlev","Hundslund","Falling","Ørting","Halling","Gosmer","Gylling","Alrø") ~ "Hads",
      TRUE ~ "Ukendt"
    )
  )

clean_1834 <- total_1834 %>%
  filter(amt=="Århus") %>% 
  select(amt, sogn, koen, alder, civilstand) %>% 
  mutate(
    herred = case_when(
      sogn %in% c("Lyngå", "Hadsten", "Vitten", "Haldum","Foldby", "Lading", "Sabro", "Fårup") ~ "Sabro",
      sogn %in% c("Spørring","Grundfør","Trige","Søften","Ølsted","Elev","Elsted","Lisbjerg") ~ "Vester Lisbjerg",
      sogn %in% c("Skivholme","Borum","Sjelle","Skørring","Galten","Skovby","Framlev","Storring","Harlev","Stjær") ~ "Framlev",
      sogn %in% c("Kasted","Skejby","Vejlby","Tilst","Lyngby","Årslev","Brabrand","Hasle","Åby","Århus Købstad") ~ "Hasle",
      sogn %in% c("Ormslev","Kolt","Viby","Holme","Tranbjerg","Tiset","Mårslet","Beder","Malling","Astrup","Tulstrup","Tunø") ~ "Ning",
      sogn %in% c("Odder","Hvilsted","Torrild","Nølev","Saksild","Bjerager","Randlev","Hundslund","Falling","Ørting","Halling","Gosmer","Gylling","Alrø") ~ "Hads",
      TRUE ~ "Ukendt"
    )
  )

clean_1860 <- total_1860 %>%
  filter(amt=="Århus") %>% 
  select(amt, herred, sogn, koen, alder, civilstand)

#combine datasets
  #add column for year
all_census <- bind_rows(
  mutate(clean_1801, år = 1801),
  mutate(clean_1834, år = 1834),
  mutate(clean_1860, år = 1860)
)

#download the new combined dataset as CSV
write.csv(all_census,"~/Downloads/aarhus_census_all.csv", row.names = FALSE)


  ## Hypothesis A ##

#count inhabitants in Aarhus County
inhab_shires <- all_census %>% 
  count(herred,år)

#plot inhabitants of Aarhus County  
ggplot(inhab_shires,
       aes(x=år,y=n,color=herred))+
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 13))+
  labs(title = "Population growth: Aarhus County",
       x = "Year",
       y = "Population",
       color = "Shire:")

ggsave("pop_grow_aarhus.png")

#count inhabitants in Hasle Shire  
inhab_has_parish <- all_census %>% 
  filter(herred=="Hasle") %>% 
  count(sogn,år)

#plot inhabitants of Hasle Shire  
ggplot(inhab_has_parish,
       aes(x=år,y=n,color=sogn))+
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 13))+
  labs(title = "Population growth: Hasle Shire",
       x = "Year",
       y = "Population",
       color = "Parish:")

ggsave("pop_grow_hasle.png")


  ## Hypothesis C ##

#calculate average age in Aarhus County
inhab_age_aar <- all_census %>%
  filter(år %in% c(1801, 1834, 1860)) %>%
  group_by(år,herred) %>%
  summarise(
    average_age = mean(alder, na.rm = TRUE)
  )

#plot average age in Aarhus County
ggplot(inhab_age_aar,
       aes(x = år,
           y = average_age,
           group = herred,
           color = herred)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 13))+
  labs(
    title = "Average age in Aarhus County",
    x = "Year",
    y = "Average age",
    color = "Shire:"
  )

ggsave("age_aarhus.png")

#calculate average age in Hasle Shire 
inhab_age_has <- all_census %>%
  filter(herred=="Hasle") %>% 
  filter(år %in% c(1801, 1834, 1860)) %>%
  group_by(år,sogn) %>%
  summarise(
    average_age = mean(alder, na.rm = TRUE)
  )

#plot average age in Hasle Shire
ggplot(inhab_age_has,
       aes(x = år,
           y = average_age,
           group = sogn,
           color = sogn)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 13))+
  labs(
    title = "Average age in Hasle Shire",
    x = "Year",
    y = "Average age",
    color = "Parish:"
  )

ggsave("age_hasle.png")


  ## Hypothesis D ##

#check which labels are included in the dataset
unique(all_census$civilstand)
unique(all_census$koen)

#calculate average age of unmarried inhabitants in Aarhus County 
avrg_unm_aar <- all_census %>%
  filter(
    alder >= 15,
    alder <= 50,
    civilstand=="ugift", 
    civilstand != "",
    koen != "") %>%
  group_by(år, herred) %>%
  summarise(antal=n(),
    gennemsnitsalder = mean(alder, na.rm = TRUE))

#plot average age of unmarried inhabitants in Aarhus County
ggplot(avrg_unm_aar,
       aes(x = år,
           y = gennemsnitsalder,
           color = herred,
           group=herred)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(
    title = "Average age as unmarried: Aarhus County",
    x = "Year",
    y = "Average age",
    color = "Shire:")+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 15))

ggsave("avrg_age_unm_aar.png",width = 8, height = 6)

#Calculate average age of unmarried inhabitants in Hasle Shire
avrg_unm_has <- all_census %>%
  filter(
    alder >= 15,
    alder <= 50,
    civilstand=="ugift",
    herred=="Hasle",
    civilstand != "",
    koen != "") %>%
  group_by(år, sogn) %>%
  summarise(antal=n(),
            gennemsnitsalder = mean(alder, na.rm = TRUE))

#plot average age of unmarried inhabitants in Hasle Shire
ggplot(avrg_unm_has,
       aes(x = år,
           y = gennemsnitsalder,
           color = sogn,
           group=sogn)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(
    title = "Average age as unmarried: Hasle Shire",
    x = "Year",
    y = "Average age",
    color = "Parish:")+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 15))

ggsave("avrg_age_unm_has.png",width = 8, height = 6)


  ## Hypothesis E ##

#Calculate share of elders in Aarhus County shires 
plot_o_aar <- all_census %>%
  filter(alder != "") %>%
  group_by(år, herred) %>%
  summarise(
    total_befolkning = n(),
    over_60 = sum(alder > 60),
    procent_over_60 = over_60 / total_befolkning * 100
  )

#calculate the total share of elders in Aarhus County
o_total_aar <- all_census%>%
  filter(alder != "",) %>%
  group_by(år) %>%
  summarise(
    total_befolkning = n(),
    over_60 = sum(alder > 60),
    procent_over_60 = over_60 / total_befolkning * 100
  )

#plot share of elders in shires with total line 
ggplot(plot_o_aar,
       aes(x = år,
           y = procent_over_60,
           group=herred,
           color = herred)) +
  geom_line(size = 0.5) +
  geom_point(size = 1)+
  geom_line(
    data = o_total_aar,
    aes(x = år,
        y = procent_over_60),
    inherit.aes=FALSE,
    color = "black",
    linetype = "dashed",
    size = 1) +
  geom_point(
    data = o_total_aar,
    aes(x = år,
        y = procent_over_60),
    inherit.aes=FALSE,
    color = "black",
    size = 2
  ) +
  labs(
    title = "Population (>60 years): Aarhus County",
    x = "Year",
    y = "Percentage share",
    color = "Shire:")+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 17)) 

ggsave("old_aarhus.png", width = 8, height = 8)

#calculate share of elders in Hasle Shire parishes  
plot_o_has <- all_census %>%
  filter(herred=="Hasle",
         alder != "") %>%
  group_by(år, sogn) %>%
  summarise(
    total_befolkning = n(),
    over_60 = sum(alder > 60),
    procent_over_60 = over_60 / total_befolkning * 100
  )

#calculate total share of elders in Hasle Shire parishes
o_total_has <- all_census%>%
  filter(alder != "",
         herred=="Hasle") %>%
  group_by(år) %>%
  summarise(
    total_befolkning = n(),
    over_60 = sum(alder > 60),
    procent_over_60 = over_60 / total_befolkning * 100
  )

#plot share of elders in parishes with total line 
ggplot(plot_o_has,
       aes(x = år,
           y = procent_over_60,
           group=sogn,
           color = sogn)) +
  geom_line(size = 0.5) +
  geom_point(size = 1)+
  geom_line(
    data = o_total_has,
    aes(x = år,
        y = procent_over_60),
    inherit.aes=FALSE,
    color = "black",
    linetype = "dashed",
    size = 1) +
  geom_point(
    data = o_total_has,
    aes(x = år,
        y = procent_over_60),
    inherit.aes=FALSE,
    color = "black",
    size = 2
  ) +
  labs(
    title = "Population (>60 years): Hasle Shire",
    x = "Year",
    y = "Percentage share",
    color = "Parish:")+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 17))

ggsave("old_hasle.png", width = 8, height = 8)


  ## Hypothesis F ##

#calculate share of children below 15 years in Aarhus County shires
plot_y_aar <- all_census %>%
  filter(alder != "",
         ) %>%
  group_by(år,herred) %>%
  summarise(
    total_befolkning = n(),
    under_15 = sum(alder < 15),
    procent_under_15 = under_15 / total_befolkning * 100
  )

#calculate the total share of children in Aarhus County
y_total_aar <- all_census%>%
  filter(alder != "",) %>%
  group_by(år) %>%
  summarise(
    total_befolkning = n(),
    under_15 = sum(alder < 15),
    procent_under_15 = under_15 / total_befolkning * 100
  )

#plot share of children in Aarhus County shires with total line 
ggplot(plot_y_aar,
       aes(x = år,
           y = procent_under_15,
           group = herred,
           color = herred)) +
  geom_line(size = 0.5) +
  geom_point(size = 1)+
  geom_line(
    data = y_total_aar,
    aes(x = år,
        y = procent_under_15),
    inherit.aes=FALSE,
    color = "black",
    linetype = "dashed",
    size = 1) +
  geom_point(
    data = y_total_aar,
    aes(x = år,
        y = procent_under_15),
    inherit.aes=FALSE,
    color = "black",
    size = 2
  ) +
  labs(
    title = "Population (<15 years): Aarhus County",
    x = "Year",
    y = "Percentage share",
    color = "Shire:")+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 16))

ggsave("young_aarhus.png",width = 8, height = 7)

#calculate share of children in Hasle Shire parishes
plot_y_has <- all_census %>%
  filter(,
    herred=="Hasle",
    alder != "") %>%
  group_by(år, sogn) %>%
  summarise(
    total_befolkning = n(),
    under_15 = sum(alder < 15),
    procent_under_15 = under_15 / total_befolkning * 100
  )

#calculate the total share of children in Hasle Shire
y_total_has <- all_census%>%
  filter(alder != "",
         herred=="Hasle") %>%
  group_by(år) %>%
  summarise(
    total_befolkning = n(),
    under_15 = sum(alder < 15),
    procent_under_15 = under_15 / total_befolkning * 100
  )

#plot share of children in parishes of Hasle Shire with total line 
ggplot(plot_y_has,
       aes(x = år,
           y = procent_under_15,
           group=sogn,
           color = sogn)) +
  geom_line(size = 0.5) +
  geom_point(size = 1)+
  geom_line(
    data = y_total_has,
    aes(x = år,
        y = procent_under_15),
    inherit.aes=FALSE,
    color = "black",
    linetype = "dashed",
    size = 1) +
  geom_point(
    data = y_total_has,
    aes(x = år,
        y = procent_under_15),
    inherit.aes=FALSE,
    color = "black",
    size = 2
  ) +
  labs(
    title = "Population (<15 years): Hasle Shire",
    x = "Year",
    y = "Percentage share",
    color = "Parish:")+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 16))

ggsave("young_hasle.png", width = 8, height = 7)










  
