# Aggregation des tuiles de délimitation



library(sf)
library(dplyr)
library(ggplot2)


# Liste des fichiers 
listfile <- list.files("D:/Arthur_data/BdD_Geo/Delim_NDVI_simpl/", pattern = ".gpkg$")

layers <- lapply(paste0("D:/Arthur_data/BdD_Geo/Delim_NDVI_simpl/", listfile), st_read)
names(layers) <- basename(listfile) 
n <- combn(seq_along(listfile), 2, simplify = FALSE)

list_parc_tile <-c()
for (z in 1:length(n)){
  nn <- n[[z]]
  tmp1 <- st_read(paste0("D:/Arthur_data/BdD_Geo/Delim_NDVI_simpl/",listfile[nn[1]]))
  tmp1 <- tmp1%>%group_by(field)%>%mutate(sum_area=sum(area))%>%ungroup()%>%
    mutate(tile = listfile[nn[1]])
  
  tmp2 <- st_read(paste0("D:/Arthur_data/BdD_Geo/Delim_NDVI_simpl/", listfile[nn[2]]))
  tmp2 <- tmp2%>%group_by(field)%>%mutate(sum_area=sum(area))%>%ungroup()%>%
    mutate(tile = listfile[nn[2]])
  
  inter <- st_intersection(tmp1, tmp2)
  inter <- inter%>%mutate(Area_sup = sum_area - sum_area.1,
                          Attrib = ifelse(Area_sup < 1, tile, tile.1))%>%
    st_drop_geometry()%>%
    select(field, Attrib)%>%unique()
  
  list_parc_tile[[z]] <- inter
  
}

# Liste des parcelles les mieux représentées dans chaque tuile
liste_des_parcelles_tuile <- data.table::rbindlist(list_parc_tile)

# Aggrégation des tuiles avec les parcelles séléctionnées

aggreg <- c()
for (i in 1:length(unique(liste_des_parcelles_tuile$Attrib))){

  Tile <- liste_des_parcelles_tuile%>%filter(Attrib == unique(liste_des_parcelles_tuile$Attrib)[i])
  
  tmp <- st_read(paste0("D:/Arthur_data/BdD_Geo/Delim_NDVI_simpl/", unique(Tile$Attrib)))
  tmp <- tmp%>%filter(!field%in%Tile$field)
  
  aggreg[[i]] <- tmp
}

cartwalldelim <- st_as_sf(data.table::rbindlist(aggreg, ignore.attr=TRUE))
length(unique(cartwalldelim$field))

cartwalldelim <- cartwalldelim%>%st_cast("MULTIPOLYGON")

ggplot(cartwalldelim)+geom_sf(aes(fill=MZ, color=MZ))

cartwalldelim <- cartwalldelim%>%group_by(field)%>%mutate(Surfacetotale = sum(area))%>%filter(Surfacetotale>10000)



st_write(cartwalldelim, "d://Arthur_data/BdD_Geo/cartewaldelim2.gpkg", delete_dsn = TRUE, delete_layer = TRUE)
