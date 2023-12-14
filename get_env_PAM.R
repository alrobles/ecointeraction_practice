library(terra)
library(tidyverse)
#shp_dir <- "data/input/Shapes_files_america/"
worldPCA <- terra::rast("https://worldpca.nyc3.digitaloceanspaces.com/WorldPCARast.tiff")
rodentiaMaps <- mdd::get_mdd_rodentia()

geoPAM <- read_csv("geoPAM.csv")

Worldmap <- rnaturalearth::ne_countries(returnclass = "sf")
Worldmap <- terra::vect(Worldmap)
worldPCA <- terra::crop(worldPCA, Worldmap, mask = TRUE)
PCAminmax <- terra::minmax(worldPCA)
x1 <- round(PCAminmax[1])
x2 <- ceiling(PCAminmax[2])
y1 <- round(PCAminmax[3])
y2 <- ceiling(PCAminmax[4])
x <- seq(x1, x2, 0.1)
y <- seq(y1, y2, 0.1)

env_grid <- tidyr::expand_grid(x, y)
get_env_PAM(1)

get_env_PAM  <- function(x){
  host <- rodentiaMaps[x]
  spEnvDf <- terra::extract(worldPCA, host )
  spEnvDf <- as.matrix(na.exclude(spEnvDf[ ,c(2:3)]))
  r <- terra::rast(env_grid)
  z <- terra::rasterize(spEnvDf, r, value=1, fun=mean)
  M <- as.data.frame(z, xy = TRUE) %>% as_tibble()
  M$sciname <- host$sciname
  M <- M %>% select(x, y, sciname, mean) %>% 
    rename(value = mean)
  return(M)
}

get_env_PAM_safe <- possibly(get_env_PAM, tibble(x = NA_real_, y = NA_real_, sciname = NA_character_, value = NA_real_))


hostDf <-   1:length(rodentiaMaps) %>% 
  purrr::map_df(function(x) get_env_PAM_safe(x), .progress = TRUE)

envPAM <- hostDf %>% distinct() %>% spread(sciname, value, fill = 0)


write_csv(envPAM, "envPAM.csv")
