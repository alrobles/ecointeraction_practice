library(sf)
library(tidyverse)
geoPAM <- read_csv("geoPAM.csv")
#geoPAM %>% select(-c(1, 2)) %>% as.matrix() %>%  colSums()

geoPAM <- geoPAM[ ,-c(1,2)] %>% as.matrix
geoPAM <- t(geoPAM)
geoJaccard <- fastJaccard::jaccard_fast_matrix(geoPAM)

rownames(geoJaccard) <- colnames(geoJaccard) <- rownames(geoPAM)
geoJaccard %>% as_tibble() %>%
  write_csv("geoJaccard.csv")

envPAM <- read_csv("envPAM.csv")
envPAM <- envPAM[ ,-c(1,2)] %>% as.matrix
envPAM <- t(envPAM)
envJaccard <- fastJaccard::jaccard_fast_matrix(envPAM)
rownames(envJaccard) <- colnames(envJaccard) <- rownames(envPAM)
envJaccard %>% as_tibble() %>%
  write_csv("data/15_envJaccard.csv")


