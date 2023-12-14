# Primero vemos información de los datos
library(mdd)
library(terra)
rodentiaMaps <- mdd::get_mdd_rodentia()
  
rodentiaMaps

Worldmap <- rnaturalearth::ne_countries(returnclass = "sf")
Worldmap <- terra::vect(Worldmap)
length(rodentiaMaps)
1:5
rodentiaMaps[1]

get_PAM_df <- function(x){
  r <- terra::rast(Worldmap, res = 1)
  z <- terra::rasterize(x = rodentiaMaps[x], y =  r, "sciname")
  M <- as.data.frame(z, xy = TRUE) %>% as_tibble()
  M %>% mutate(value = 1)
}
get_PAM_safe <- possibly(get_PAM_df, tibble(x = NA_real_, y = NA_real_, sciname = NA_character_, value = NA_real_))


hostDf <- purrr::map_df(1:length(rodentiaMaps), get_PAM_safe, .progress = TRUE)

geoPAM <- hostDf %>% spread(sciname, value, fill = 0)

write_csv(geoPAM, "geoPAM.csv")



