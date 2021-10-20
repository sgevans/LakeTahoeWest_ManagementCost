library(stringr)
library(rgdal)

############### DEFINE DBH BINS #########################################

load(paste(dirCSV,'area.select_raster_masks.RData',sep='')) # load the file with spatial grouping info (LTW, LTB, CA, etc.)

dbhColSeq<-c(paste('dbh.',
                   str_pad(seq(0,38,2),width=2,pad='0',side='left'),
                   '_',
                   str_pad(seq(2,40,2),width=2,pad='0',side='left'),sep=''),
             'dbh.40_100')

dbhColSeq.timber <- dbhColSeq[grep("dbh.10_12",dbhColSeq):length(dbhColSeq)]


dbhGroups <- list(brush = grep('dbh.00_02',dbhColSeq):grep('dbh.00_02',dbhColSeq), # brush cut stems (0-2)
                  chip =grep('dbh.02_04',dbhColSeq):grep('dbh.08_10',dbhColSeq), # chip tree stems (2-10)
                  small=grep('dbh.10_12',dbhColSeq):grep('dbh.18_20',dbhColSeq), # small tree stems (10-20)
                  large=grep('dbh.20_22',dbhColSeq):grep('dbh.40_100',dbhColSeq), # large tree stems (20+)
                  vlarge = grep('dbh.40_100',dbhColSeq), 
                  timb.10.14 = grep('dbh.10_12',dbhColSeq):grep('dbh.12_14',dbhColSeq),
                  timb.14.20 = grep('dbh.14_16',dbhColSeq):grep('dbh.18_20',dbhColSeq),
                  timb.20.40 = grep('dbh.20_22',dbhColSeq):grep('dbh.38_40',dbhColSeq),
                  timb.40plus = grep('dbh.40_100',dbhColSeq))

spec.groups <- list(pine=list("PinuJeff","PinuCont","PinuLamb","PinuMont","PinuAlbi"),
                    truefir=list("AbieConc","AbieMagn"),
                    cedarseq = list("CaloDecu"))

unitConv <- list(AcresInHectare = 2.471,
                 KgInShortTon = 907.185,
                 CuftInM3 = 35.3147)

stands.map <- readGDAL(paste(dirCSV,'stands_3ha.tif',sep=""))

slope<-readGDAL(paste(dirCSV,'Slope_1ha_UTM11.tif',sep=""))
slope@data$band1[is.na(stands.map@data$band1)]   <- NA # mask to same area as other data

yarding<-readGDAL(paste(dirCSV,'yarding_distance_1ha_UTM11.tif',sep=""))
yarding@data$band1[is.na(stands.map@data$band1)]   <- NA # mask to same area as other data

# stands.management <- read_csv(paste(dirCSV,'Stands_MgmtZones.csv',sep='')) %>%
#   mutate(threat.code = case_when(centroid.threatzone=="WUI-Defense" ~ 1,
#                                  centroid.threatzone=="WUI-Threat" ~ 2,
#                                  centroid.threatzone=="General Forest" ~ 3,
#                                  centroid.threatzone=="Wilderness" ~ 4)) %>%
#   mutate(wui.defense = case_when(threat.code==1 ~ 1,
#                                 TRUE ~ 0)) %>%
#   mutate(wui.threat = case_when(threat.code==2 ~ 1,
#                                 TRUE ~ 0)) %>%
#   mutate(general.forest = case_when(threat.code==3 ~ 1,
#                                 TRUE ~ 0)) %>%
#   mutate(wilderness = case_when(threat.code==4 ~ 1,
#                                 TRUE ~ 0))
# 
# standsltb <- readGDAL('~/Dropbox/UCB/LTW - not shared/LTW_GIS/Stands_LTB_signed.tif')
# 
# stands.ltb.data <- as.tibble(standsltb@data) %>%
#   mutate(sort.map = row_number()) %>%
#   left_join(stands.management, by = c("band1"="id"))
# 
# standsltb@data <- data.frame(stands.ltb.data$threat.code)
