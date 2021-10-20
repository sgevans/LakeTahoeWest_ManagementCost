library(raster)
library(sf)
library(rgdal)

## not currently working. Something wrong with PTEIR shapefiles
## saved RData object should still be good for LTW, LTB, CA

dirData <- '~/Dropbox/Shared_files/LTW_Landis_to_OpCost/CSVs_data_required/'

stands.map <- readGDAL(paste(dirData,'stands_3ha.tif',sep=''))


fgdb <- '/Users/timholland/Dropbox/Shared_files/LTW_Landis_to_OpCost/CSVs_data_required/shapefiles/ManagementZone_LakeTahoe.gdb'
fmz_ltb <- readOGR(dsn=fgdb,layer="ForestManagementZone_LakeTahoe")
#fmz_ltw <- readOGR(dsn=fgdb,layer="ForestManagementZone_LakeTahoeWest")

fmz_ltb@data[grepl("Defense",fmz_ltb@data$Name),"zonenum"]    <-1
fmz_ltb@data[grepl("Threat",fmz_ltb@data$Name),"zonenum"]     <-2
fmz_ltb@data[grepl("General",fmz_ltb@data$Name),"zonenum"]    <-3
fmz_ltb@data[grepl("Wilderness",fmz_ltb@data$Name),"zonenum"] <-4

fmz_ltb <- fmz_ltb["zonenum"]

fmz_ltb <- spTransform(fmz_ltb,crs(stands.map))


start.time <- Sys.time()


sort.map.data <- as.tibble(stands.map@data) %>%
  mutate(sort.map = row_number())

sort.map <- stands.map
sort.map@data <- data.frame(sort.map.data)

ca <- shapefile(paste(dirData,'shapefiles/CA_State_TIGER2016.shp',sep=''))
ca <- spTransform(ca,crs(stands.map))
ca@data <- data.frame("ca"=1)

print("CA shape imported")
print(Sys.time()-start.time)

# pteir.1 <- shapefile(paste(dirData,'shapefiles/Community_Fuel_Reduction_Areas.shp',sep=''))
# pteir.1 <- spTransform(pteir.1,crs(stands.map))
# pteir.1@data <- data.frame("pteir.1" = 1)
# 
# pteir.2 <- shapefile(paste(dirData,'shapefiles/Planned_CWPP_Projects.shp',sep=''))
# pteir.2 <- spTransform(pteir.2,crs(stands.map))
# pteir.2@data <- data.frame("pteir.2" = 1)

print("PTEIR shapes imported")
print(Sys.time()-start.time)

ltw.zones <- shapefile(paste(dirData,'shapefiles/ltwMgtZones180814.shp',sep=''))
ltw.zones.num <- ltw.zones["zonenum"]
ltw.zones.num.utm11 <- spTransform(ltw.zones.num,crs(stands.map))

stands <- shapefile('~/Dropbox/UCB/LTW - not shared/LTW_GIS/Stands_MgmtZones/Stands_MgmtZones_UTM11.shp')

print("done import")
print(Sys.time()-start.time)

stands.data <- stands@data
stands.data$threatcode <- NA
stands.data[stands.data$CentroidWi=="WUI-Defense","threatcode"] <- 1
stands.data[stands.data$CentroidWi=="WUI-Threat","threatcode"] <- 2
stands.data[stands.data$CentroidWi=="General Forest","threatcode"] <- 3
stands.data[stands.data$CentroidWi=="Wilderness","threatcode"] <- 4

stands@data <- stands.data
stands.threatnum <- stands["threatcode"]

r.raster <- raster()  

crs(r.raster) <- crs(stands.map)
extent(r.raster) <- extent(stands.map)
res(r.raster) <- 100

fmz.rast <- as(rasterize(fmz_ltb, r.raster), "SpatialGridDataFrame")
fmz.ltw.rast <- as(rasterize(ltw.zones.num.utm11, r.raster), "SpatialGridDataFrame")

#m.zones.spatial <- as(rasterize(m.zones.num.utm11, r.raster), "SpatialGridDataFrame")
ca.rast <- as(rasterize(ca, r.raster), "SpatialGridDataFrame")
stands.rast <- as(rasterize(stands.threatnum, r.raster), "SpatialGridDataFrame")
# pteir1.rast <- as(rasterize(pteir.1, r.raster), "SpatialGridDataFrame")
# pteir2.rast <- as(rasterize(pteir.2, r.raster), "SpatialGridDataFrame")

print("rasterizes done")
print(Sys.time()-start.time)

# pteir.rast <- max(pteir1.rast,pteir2.rast,na.rm=T)
# print("pteir sum done")
# print(Sys.time()-start.time)


intersect <- as.tibble(cbind(stands.map@data$band1,fmz.rast@data$zonenum,ca.rast@data[,1],stands.rast@data[,1],fmz.ltw.rast@data[,1])) %>%
  mutate(manage.zone = case_when(!is.na(V1) & !is.na(V2) ~ V2)) %>%
  mutate(ltw = case_when(!is.na(V1) & !is.na(V5) ~ 1,
                         !is.na(V1) & is.na(V5) ~ 0)) %>%
  mutate(ltb = case_when(!is.na(V1) ~ 1)) %>%
  mutate(ca = case_when(!is.na(V1) & !is.na(V3) ~ 1,
                        !is.na(V1) & is.na(V3) ~ 0)) %>%
  # mutate(pteir = case_when(!is.na(V1) & !is.na(V4) ~ 1,
  #                       !is.na(V1) & is.na(V4) ~ 0)) %>%
  # rename(threatzone = V6) %>%
  rename(threatzone = V4) %>%
  mutate(threatzone = as.numeric(threatzone)) %>%
  mutate(wui.defense = case_when(threatzone==1 ~ 1,
                                 threatzone!=1 & !is.na(threatzone) ~ 0)) %>%
  mutate(wui.threat = case_when(threatzone==2 ~ 1,
                                threatzone!=2 & !is.na(threatzone) ~ 0)) %>%
  mutate(general.forest = case_when(threatzone==3 ~ 1,
                                threatzone!=3 & !is.na(threatzone) ~ 0)) %>%
  mutate(wilderness = case_when(threatzone==4 ~ 1,
                                threatzone!=4 & !is.na(threatzone) ~ 0)) %>%
  mutate(wui = case_when(wui.threat==1 | wui.defense==1 ~ 1,
                         wui.threat!=1 & wui.defense!=1 & !is.na(ltb) ~ 0)) %>%
  mutate(non.wui = case_when(wui==0 ~ 1,
                            wui==1 ~ 0)) %>%
  mutate(ltw.wui = case_when(ltw==1 & wui==1 ~ 1,
                             ltw!=1 | wui!=1 & !is.na(wui) ~ 0)) %>%
  mutate(ltw.non.wui = case_when(ltw==1 & wui==0 ~ 1,
                             ltw!=1 | wui==1 & !is.na(wui) ~ 0)) %>%
  dplyr::select(-c(V1,V2,V3))

print("intersect done")
print(Sys.time()-start.time)
  
area.select <- raster(stands.map)
area.select$ltw <- intersect$ltw
area.select$ltb <- intersect$ltb
area.select$ca <- intersect$ca
area.select$threatzone <- intersect$threatzone
area.select$wui.defense <- intersect$wui.defense
area.select$wui.threat <- intersect$wui.threat
area.select$wui <- intersect$wui
area.select$non.wui <- intersect$non.wui
area.select$general.forest <- intersect$general.forest
area.select$wilderness <- intersect$wilderness
area.select$ltw.wui <- intersect$ltw.wui
area.select$ltw.non.wui <- intersect$ltw.non.wui
area.select$sort.map <- 1:length(area.select$ltw)
names(area.select) <- c("stand",names(area.select)[2:length(names(area.select))])


area.select.tib <- as.tibble(as.data.frame(area.select)) %>%
  filter(!is.na(stand))


save(area.select,file="~/Dropbox/Shared_files/LTW_Landis_to_OpCost/CSVs_data_required/area.select_raster_masks.RData")

rm(intersect)

print("done")
print(Sys.time()-start.time)
