url <- "https://github.com/viralemergence/virion/raw/main/Virion/Virion.csv.gz"

library(tidyverse); library(vroom)

virion <- vroom(url)

virion_nh <- virion %>% 
  filter(Host != 'homo sapiens')

virion_mammalia <- virion_nh %>% 
  filter(HostClass == "mammalia")

virion_order_virus <- virion_mammalia %>%
  select(HostOrder, Host, VirusFamily, VirusGenus) %>% 
  na.exclude()

virion_order_virus_count <- virion_order_virus %>% 
  count(HostOrder, sort = TRUE)


virion_rodent_hantavirus <- virion_order_virus %>% 
  filter(HostOrder == "rodentia") %>% 
  filter(VirusFamily == "hantaviridae")


virion_rodent_hantavirus_distinct <- virion_rodent_hantavirus %>% 
  select(Host, VirusGenus) %>% 
  distinct()


hantavirus_count <- virion_rodent_hantavirus %>% 
  count(Host, name = "incidence", sort = TRUE) %>% 
  rename(species = Host)

library(ecointeraction)

hantavirus_incidence <- accumulate_incidence(data = hantavirus_count,
                                                    group = species,
                                                    incidence = incidence)

hantavirus_host <- cutoff_incidence(hantavirus_incidence)

mamDist <- ecointeraction::mammalsdistance %>% 
  mutate(species = str_to_lower(species))

hantavirus_data <- hantavirus_host %>% 
  prep_incidence_data(distance  = mamDist) 

library(modelgrid)
models <- automodel_replication(hantavirus_data, mamDist)


