library(sf)
library(stars)
library(terra)
library(dplyr)
library(ggplot2)

# ATTENTION
## Grilles de délimitation enregistrées dans "D://Arthur_data/BdD_geo/RPG/Wall/Grids/"
## obsolète à l'usage au 05/01/2026 - les chemins ne sont plus bons

# On charge une image satellite pour appliquer le CRS de transformation au fichier RPG
ImS2<-rast("D://Arthur_data/08. GIS_Prog/NDVI_T31UDR(8_4)_20160414.tif")
tile_ext<-read.csv2("D://Arthur_data/tile_ext_utm31.csv")
tile_ext<-as.data.frame(t(tile_ext%>%dplyr::select("X", "T31UES", "T31UFS", "T31UFR", "T31UDS", "T31UER")))
names(tile_ext)<-tile_ext[1,]
tile_ext<-tile_ext[-1,]
head(tile_ext)

newcrs<-raster::crs(ImS2)
rm(ImS2)
years<-c(2016, 2017, 2018, 2019, 2020)
cult2<-c(311, 312, 321, 322, 323, 36, 351, 352)
R1l<-c()
R2l<-c()

# -------------------------------------------------------------------------------------------------------------------------
# --------------------------------------- Découpage du territoire ---------------------------------------------------------

borders<-st_read("D://Arthur_data/04. BdD_geo/RPG/Wall/RPG_2016/SIGEC_PARC_AGRI_ANON__2016.shp")
borders<-st_transform(borders, crs = newcrs)

xmin<-as.numeric(st_bbox(borders)[1])
ymin<-as.numeric(st_bbox(borders)[2])
xmax<-as.numeric(st_bbox(borders)[3])
ymax<-as.numeric(st_bbox(borders)[4])

xdiff<-seq(xmin, xmax, length.out=11)
ydiff<-seq(ymin, ymax, length.out=11)
gridcoord<-expand.grid(x=xdiff, y=ydiff)

gridcoord<-gridcoord%>%
  group_by(x)%>%
  mutate(nR1=1:n())%>%
  ungroup()%>%
  group_by(y)%>%
  mutate(nR2=1:n())%>%
  ungroup()
  
gr<-st_make_grid(st_as_sf(gridcoord, coords = c("x","y")))
ggplot()+geom_sf(gr, mappin=aes())+geom_text(gridcoord, mapping =aes(x=x, y=y, label=nR2) )

for (var1 in 6){
  for (var2 in 9){
    
   

  xmin1 = round(as.numeric(unique(gridcoord$x)[var1]))-1000
  ymin1 = round(as.numeric(unique(gridcoord$y)[var2]))-1000
  xmax1 = round(as.numeric(unique(gridcoord$x)[var1+1]))+1000
  ymax1 = round(as.numeric(unique(gridcoord$y)[var2+1]))+1000
  cp<-st_crop(borders, xmin=xmin1, ymin=ymin1, xmax=xmax1, ymax=ymax1)
  if(length(cp$OBJECTID)==0){
    next
    }
  
  grid<-st_as_sf(st_make_grid(cp, cellsize = c(10, 10)))
  st_write(grid, dsn=paste("D://Arthur_data/04. BdD_geo/RPG/Wall/Grids/Gridfilt_", unique(gridcoord$nR2[gridcoord$x==unique(gridcoord$x)[var1]]),"_", unique(gridcoord$nR1[gridcoord$y==unique(gridcoord$y)[var2]]),"_", ".gpkg", sep=""), driver = "GPKG")
  }
}


# --------------------------------------------###--------------------------------------------------------------------------



