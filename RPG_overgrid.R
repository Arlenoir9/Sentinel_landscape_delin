library(sf)
library(stars)
library(dplyr)
library(ggplot2)

# Revu le 22/06/2025.
# Attention, les identifiants DD sont inversés

ImS2<-raster::raster("D://Img_Sen2/S2_coreg/SENTINEL2A_20170419-110601-310_L2A_T31UDR_D_V1-4/SENTINEL2A_20170419-110601-310_L2A_T31UDR_D_V1-4_SRE_B4.tif")
newcrs<-raster::crs(ImS2)
rm(ImS2)
years<-c(2016, 2017, 2018, 2019, 2020)
cult2<-c(311, 312, 321, 322, 323, 36, 351, 352)

R2l<-c()


listgrid<-list.files("E://Arthur_data/BdD_Geo/RPG/Wall/Grids/", pattern=".gpkg")

for (ii in 43){
  lg<-listgrid[ii]
  grid<-st_read(paste0("E://Arthur_data/BdD_Geo/RPG/Wall/Grids/", lg))
  grid<-grid%>%mutate(nR=1:n())
  extgrid<-st_bbox(grid)
  
  for (jj in 1:length(years)){
    
    yr<-years[jj]
    
    tmp<-st_read(paste0("E://Arthur_data/BdD_Geo/RPG/Wall/Filtered/RPG_filt_", yr, ".shp"))
    tmp<-tmp%>%dplyr::select(OBJECTID, CAMPAGNE)
    tmp<-st_crop(tmp, extgrid)
    if(length(tmp$OBJECTID)<=1){next}
    tmp_int<-st_intersection(tmp, grid)
    tmp_int<-st_cast(tmp_int, "MULTIPOLYGON")
    
    R2l[[jj]]<-tmp_int
    
  }
  rm(tmp_int)
  if(length(R2l)==0){next}
  proj_grid<-sf::st_as_sf(data.table::rbindlist(R2l))
  R2l<-c()
  proj_grid<-proj_grid%>%
    group_by(nR)%>%
    mutate(pixn=length(nR))%>%
    filter(pixn>2)%>%
    mutate(minY=min(as.numeric(CAMPAGNE)), 
           OBJtp=ifelse(as.numeric(CAMPAGNE==minY), OBJECTID, NA),  
           ID1=paste0(minY,"_", mean(OBJtp, na.rm=T)))%>%
    dplyr::select(-c('minY', 'OBJtp', 'pixn'))
  
  st_write(proj_grid, dsn=paste0("D://These/04. BdD_geo/RPG/Wall/Filtered & gridded/pix_new", lg))

}



 
