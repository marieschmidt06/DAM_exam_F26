##Hypothesis B##
#load packages
install.packages("tidyverse")
install.packages("gganimate")
library(gganimate)
library(gifski)
library(av)
library(ggplot2)
library(tidyverse)
library(here)
library(dplyr)

#download the OpenRefine CSV files
census_1801 <- read_csv2("data/openrefine_1801.csv", na="NULL")
census_1834 <- read_csv2("data/openrefine_1834.csv", na="NULL")
census_1860 <- read_csv2("data/openrefine_1860.csv", na="NULL")

glimpse(census_1801)

#create new object with filtered rows and columns
  #change 'stilling' in 1860 to 'erhverv'
m_occ_1801 <- census_1801 %>%
  filter(alder > 15, amt == "Århus") %>%
  select(amt, sogn, koen, alder, civilstand, erhverv)

m_occ_1834 <- census_1834 %>%
  filter(alder > 15, amt == "Århus") %>%
  select(amt, sogn, koen, alder, civilstand, erhverv)

m_occ_1860 <- census_1860 %>%
  filter(alder > 15, amt == "Århus") %>%
  select(amt, sogn, herred, koen, alder, civilstand, stilling) %>% 
  rename(erhverv=stilling)

#check which parishes appear 
unique(m_occ_1801$sogn)

#create column for shire ('herred') in 1801 and 1834 
m_occ_1801 <- m_occ_1801 %>%
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

m_occ_1834 <- m_occ_1834 %>%
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

#join the censuses, add column for year
all_m_occ <- bind_rows(
  mutate(m_occ_1801, år = 1801),
  mutate(m_occ_1834, år = 1834),
  mutate(m_occ_1860, år = 1860)
)

#count each occupation
occ_count <- all_m_occ %>%
  count(år, herred, erhverv, sort = TRUE)

#create new columns for primary, secondary and tertiary occupations
all_occ_123 <- all_m_occ %>%
  mutate(
    sektor = case_when(
      erhverv %in% c("Bonde","Jordejer","Daglejer","Jordløs husmand","Tjenestekarl",
                     "Gaardbeboer","Gårdmand","selveier gaardmand","Husmand","Boelsmand",
                     "lever af sin Jordlod","Fæstegaardmand","Gaardejer") ~ "primære",
      erhverv %in% c("Væver","Skrædder","Smed","Snedker","Hjulmand","Skomager","Tømrer",
                     "Murer","Træskomager","Slagter") ~ "sekundære",
      erhverv %in% c("Købmand","Matros","Sømand") ~ "tertiære",
      TRUE ~ "Ukendt"
    )
  ) 

#create object containing only cases where a sector has been ascribed
occ_sektor <- all_occ_123 %>%
  filter(sektor != "Ukendt")

write.csv(occ_sektor,"~/Downloads/aarhus_census_sectors.csv", row.names = FALSE)

#count the occupations in Aarhus County
occ_sec_count <- occ_sektor %>% 
  count(sektor,erhverv)

#calculate percentage share of each sector
plot_sektor <- occ_sektor %>%
  count(år, sektor) %>% 
  group_by(år) %>% 
  mutate(procent = round(n / sum(n) * 100,1))

#create a pie chart 
ggplot(plot_sektor, aes(x = 2, y = n, fill = sektor)) +
  geom_col(width=1,color="white") +
  coord_polar(theta = "y") +
  xlim(0.5,2.5)+
  facet_wrap(~år, scales="free") +
  theme_void() +
  labs(title = "Occupational distribution: Aarhus County",
       fill="Sector:")+
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5,
                                  margin = margin(b = 20)))+
  geom_text(
    aes(label = ifelse(procent > 5,
                       paste0(round(procent,1), "%"),
                       "")),
    position = position_stack(vjust = 0.5)
  )+
  scale_fill_discrete(labels = c(
    "Primary",
    "Secondary",
    "Tertiary"))

ggsave("sector_pie.png", width = 8, height = 6)

#calculate percentage share of each occupation sector in Aarhus County shires 
plot_occ_aar <- occ_sektor %>% 
  count(år,herred,sektor) %>% 
  group_by(år,herred) %>% 
  mutate(procent = round(n / sum(n) * 100,1))

#plot sectors in Aarhus County shires 
ggplot(plot_occ_aar,
       aes(x = år, y = procent, color = sektor)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 15))+
  facet_wrap(~herred,ncol=3) +
  labs(title = "Occupational development: Aarhus County",
    x = "Year",
    y = "Percentage share",
    color = "Occupation sector:")+
  scale_color_discrete(labels = c(
    "Primary",
    "Secondary",
    "Tertiary"
  ))+
  scale_x_continuous(breaks = c(1801,1834,1860))

ggsave("sector_aarhus.png",width = 8, height = 6)

#calculate percentage share of occupation sectors in Hasle Shire parishes  
plot_occ_has <- occ_sektor %>% 
  filter(herred=="Hasle") %>% 
  count(år,sogn,sektor) %>% 
  group_by(år,sogn) %>% 
  mutate(procent = round(n / sum(n) * 100,1))

#plot percentage share of occupations in Hasle Shire
ggplot(plot_occ_has,
       aes(x = år, y = procent, color = sektor)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~sogn,ncol=4) +
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 8),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    text = element_text(size = 18))+
  labs(
    title = "Occupational development: Hasle Shire",
    x = "Year",
    y = "Percentage share",
    color = "Occupation sector:")+
  scale_color_discrete(labels = c(
    "Primary",
    "Secondary",
    "Tertiary"
  ))+
  scale_x_continuous(breaks = c(1801,1834,1860))

ggsave("sector_hasle.png",width = 10, height = 8)


  








