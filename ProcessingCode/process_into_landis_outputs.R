# # biomass standing: direct from LANDIS
# # stems standing: landis biomass to dbh distributions of biomass, divided by FIA estimates of biomass per stem at each dbh category
# # biomass harvested: biomass standing in each dbh category, multiplied by proportion of biomass harvested in each height category (from Nadia)
# # if stems 20-22" represent 0.1 of standing biomass, and they are harvested at 40%, then 0.04 of total standing biomass will be stems 20-22" that get harvested
# # stems harvested - calculated from biomass harvested in the same way that stems standing was calculated from biomass standing (using FIA averages)
# # merch cuft harvested: calculated from biomass harvested using FIA ratios that Bill found.

library(raster) # because otherwise it can't deal with the area.select object

biom.stnd.stack <- lapply(species.list, function(x) t.full[[x]][["biomass.standing"]])
biom.stnd.sum <- Reduce('+', biom.stnd.stack)
biom.stnd.prop <- biom.stnd.sum / rowSums(biom.stnd.sum)
biom.stnd.prop.cumsum <- t(apply(biom.stnd.prop,MARGIN=1,FUN=cumsum))

if (harvest.yn){
biom.harv.tot.stack <- lapply(species.list, function(x) t.full[[x]][["biomass.harvested.total"]])
biom.harv.tot.sum <- Reduce('+', biom.harv.tot.stack)
}

stems.stnd.stack <- lapply(species.list,function(x) t.full[[x]][["stems.standing"]]) 
stems.stnd.sum <- Reduce('+',stems.stnd.stack)
stems.stnd.prop <- stems.stnd.sum / rowSums(stems.stnd.sum)
stems.stnd.prop.cumsum <- t(apply(stems.stnd.prop,MARGIN=1,FUN=cumsum))

if (harvest.yn){
stems.harv.stack <- lapply(species.list,function(x) t.full[[x]][["stems.harvested"]]) 
stems.harv.sum <- Reduce('+',stems.harv.stack)
stems.harv.sum.dbh.weighted <- t(apply(stems.harv.sum, 1, function(x) x * seq(1,41,2)))

stems.harv.weighted.by.bole.stack <- lapply(species.list,function(x) t.full[[x]][["stems.harvested.weighted.by.bole.vol"]])
stems.harv.weighted.by.bole.sum <- Reduce('+',stems.harv.weighted.by.bole.stack)

cuft.harv.stack.pine <- lapply(spec.groups$pine, function(x) t.full[[x]][["cuft.harvested"]])
cuft.harv.sum.pine <- Reduce('+',cuft.harv.stack.pine)

cuft.harv.stack.truefir <- lapply(spec.groups$truefir, function(x) t.full[[x]][["cuft.harvested"]])
cuft.harv.sum.truefir <- Reduce('+',cuft.harv.stack.truefir)

cuft.harv.stack.cedarseq <- lapply(spec.groups$cedarseq, function(x) t.full[[x]][["cuft.harvested"]])
cuft.harv.sum.cedarseq <- Reduce('+',cuft.harv.stack.cedarseq)
}

conifer.summary <- tibble("biomass.AbieConc"=rowSums(t.full[["AbieConc"]][["biomass.standing"]])) %>%
  mutate(biomass.AbieMagn = rowSums(t.full[["AbieMagn"]][["biomass.standing"]])) %>% 
  mutate(biomass.CaloDecu = rowSums(t.full[["CaloDecu"]][["biomass.standing"]])) %>% 
  mutate(biomass.PinuCont = rowSums(t.full[["PinuCont"]][["biomass.standing"]])) %>% 
  mutate(biomass.PinuJeff = rowSums(t.full[["PinuJeff"]][["biomass.standing"]])) %>% 
  mutate(biomass.PinuLamb = rowSums(t.full[["PinuLamb"]][["biomass.standing"]])) %>% 
  mutate(biomass.PinuAlbi = rowSums(t.full[["PinuAlbi"]][["biomass.standing"]])) %>% 
  mutate(biomass.PinuMont = rowSums(t.full[["PinuMont"]][["biomass.standing"]])) %>% 
  mutate(biomass.TsugMert = rowSums(t.full[["TsugMert"]][["biomass.standing"]])) %>%
  mutate(biom.total.conifer = rowSums(.)) %>%
  mutate(sort.map = row_number()) %>%
  filter(!is.na(biom.total.conifer))

max.conifer.index <- unlist(apply(as.matrix(dplyr::select(conifer.summary,-c(biom.total.conifer,sort.map))),MARGIN=1,FUN=which.max))

conifer.index <- tibble("varname" = names(conifer.summary)) %>%
  mutate(index = row_number()) %>%
  filter(grepl("biomass.",varname)) %>%
  mutate(species = gsub("biomass.",'',varname)) %>%
  dplyr::select(-varname)

conifer.summary.dominanntsp <- conifer.summary %>%
  filter(!is.na(biomass.AbieConc)) %>%
  bind_cols(tibble("index"=max.conifer.index)) %>%
  mutate(binary.temp = case_when(biom.total.conifer==0 ~ 0,
                                 TRUE ~ 1)) %>%
  mutate(index = index * binary.temp) %>%
  left_join(conifer.index,by="index") %>%
  dplyr::select(-binary.temp) %>%
  rename(dominant.conifer = species)


landis.summary <- tibble("slope.percent" = slope@data$band1) %>%
  mutate(sort.map = row_number()) %>% # this data gets cut down to remove NA values and save memory, but this column
  # can be used to re-make the data map-able (do a full-join of it to a variable with the series 1:269136, and sort by that variable)
  mutate(yarding.dist.oneway.m = yarding@data$band1) %>%
  mutate(tpha.all = rowSums(stems.stnd.sum)) %>%
  mutate(tpha.brush = stems.stnd.sum[,dbhGroups$brush]) %>%  # NB this only works when brushcut is a single dbh category (i.e. 0-2"). If it changes (i.e. to be both 0-2" and 2-4") code will be needed to be slightly modified (similar to following lines)
  mutate(tpha.chip = rowSums(stems.stnd.sum[,dbhGroups$chip])) %>%
  mutate(tpha.small = rowSums(stems.stnd.sum[,dbhGroups$small])) %>%
  mutate(tpha.large = rowSums(stems.stnd.sum[,dbhGroups$large])) %>%
  mutate(tpha.vlarge = stems.stnd.sum[,dbhGroups$vlarge]) %>%
  mutate(tpa.all = tpha.all / unitConv$AcresInHectare) %>%
  mutate(tpa.brush = tpha.brush / unitConv$AcresInHectare) %>%
  mutate(tpa.chip = tpha.chip / unitConv$AcresInHectare) %>%
  mutate(tpa.small = tpha.small / unitConv$AcresInHectare) %>%
  mutate(tpa.large = tpha.large / unitConv$AcresInHectare) %>%
  mutate(tpa.vlarge = tpha.vlarge / unitConv$AcresInHectare) %>%
  mutate(dist.btw.trees.m = sqrt(11547 / tpha.all)) %>%  # Distance between trees derived from "square meters per tree" (i.e. 10000 / tph) divided by (sqrt(3) / 2)
  # based on assumption of triangular close-packing and that the area per tree is one third the area of a hexagon
  # with each side being the distance between two given trees.
  mutate(dist.btw.trees.m = replace(dist.btw.trees.m, is.infinite(dist.btw.trees.m), NA)) %>%
 left_join(conifer.summary.dominanntsp, by="sort.map") %>%
 mutate(biomass.total = readGDAL(paste(dirReplicate,"biomass/TotalBiomass-",ti-1,".img",sep=""))@data$band1 * 10000) %>% # convert from g/m2 to g/ha
 mutate(biomass.total.from.tree.totals = rowSums(biom.stnd.sum)) %>%
 mutate(biomass.poputrem = rowSums(t.full[["PopuTrem"]][["biomass.standing"]])) %>%
 mutate(biomass.fixnresp = readGDAL(paste(dirReplicate,"biomass/FixnResp-",ti-1,".img",sep=""))@data$band1 * 10000) %>% # convert from g/m2 to g/ha
 mutate(biomass.fixnseed = readGDAL(paste(dirReplicate,"biomass/FixnSeed-",ti-1,".img",sep=""))@data$band1 * 10000) %>% # convert from g/m2 to g/ha
 mutate(biomass.nonnresp = readGDAL(paste(dirReplicate,"biomass/NonnResp-",ti-1,".img",sep=""))@data$band1 * 10000) %>% # convert from g/m2 to g/ha
 mutate(biomass.nonnseed = readGDAL(paste(dirReplicate,"biomass/NonnSeed-",ti-1,".img",sep=""))@data$band1 * 10000) %>% # convert from g/m2 to g/ha
 mutate(biomass.conifer = biomass.AbieConc + biomass.AbieMagn + biomass.CaloDecu + biomass.PinuCont + biomass.PinuJeff + biomass.PinuLamb + biomass.PinuAlbi + biomass.PinuMont + biomass.TsugMert) %>%
 mutate(biomass.shrub = biomass.fixnresp + biomass.fixnseed + biomass.nonnresp + biomass.nonnseed) %>%
 dplyr::select(-c(biomass.fixnresp,biomass.fixnseed,biomass.nonnresp,biomass.nonnseed)) %>%
 mutate(biomass.total.from.tree.totals = rowSums(biom.stnd.sum,na.rm=T)) %>%
 mutate(biom.prop.tree = biomass.total.from.tree.totals / biomass.total) %>%
 mutate(biom.prop.conifer = biomass.conifer / biomass.total) %>%
 mutate(biom.prop.hardwood = biomass.poputrem / biomass.total) %>%
 mutate(biom.prop.shrub = biomass.shrub / biomass.total) %>%
 mutate(dbh.90p.biom.ind = apply(biom.stnd.prop.cumsum, MARGIN=1, function(x) which(x>0.90)[1])) %>%
 mutate(dbh.95p.biom.ind = apply(biom.stnd.prop.cumsum, MARGIN=1, function(x) which(x>0.95)[1])) %>%
 mutate(dbh.99p.biom.ind = apply(biom.stnd.prop.cumsum, MARGIN=1, function(x) which(x>0.99)[1])) %>%
 mutate(dbh.90p.stems.ind = apply(stems.stnd.prop.cumsum, MARGIN=1, function(x) which(x>0.90)[1])) %>%
 mutate(dbh.95p.stems.ind = apply(stems.stnd.prop.cumsum, MARGIN=1, function(x) which(x>0.95)[1])) %>%
 mutate(dbh.99p.stems.ind = apply(stems.stnd.prop.cumsum, MARGIN=1, function(x) which(x>0.99)[1])) %>%
 left_join(tibble("dbh.90p.biom.ind"=1:21,"dbh.90p.by.biom"=seq(1,41,2)),by="dbh.90p.biom.ind") %>%
 left_join(tibble("dbh.95p.biom.ind"=1:21,"dbh.95p.by.biom"=seq(1,41,2)),by="dbh.95p.biom.ind") %>%
 left_join(tibble("dbh.99p.biom.ind"=1:21,"dbh.99p.by.biom"=seq(1,41,2)),by="dbh.99p.biom.ind") %>%
 left_join(tibble("dbh.90p.stems.ind"=1:21,"dbh.90p.by.stems"=seq(1,41,2)),by="dbh.90p.stems.ind") %>%
 left_join(tibble("dbh.95p.stems.ind"=1:21,"dbh.95p.by.stems"=seq(1,41,2)),by="dbh.95p.stems.ind") %>%
 left_join(tibble("dbh.99p.stems.ind"=1:21,"dbh.99p.by.stems"=seq(1,41,2)),by="dbh.99p.stems.ind") %>%
 dplyr::select(-ends_with(".ind")) %>%
 dplyr::select(-starts_with("tpha")) %>%
 left_join(as.tibble(as.data.frame(area.select)),by="sort.map") %>%
  mutate(replicate = repl) %>%
  mutate(scenario = scen) %>%
  mutate(time = ti) 

if(harvest.yn){
landis.summary <- landis.summary %>%
  bind_cols(dplyr::select(harvTable,c(stand,prescript,rx.hand,rx.mech,biom.removed.total))) %>%
  mutate(rx.any = rx.mech + rx.hand) %>%
  mutate(biomass.removed.total = readGDAL(paste(dirReplicate,"harvest/biomass-removed-",ti,".img",sep=""))@data$band1 * 10000) %>% # convert from g/m to g/ha
  mutate(biomass.harv.brush = biom.harv.tot.sum[,dbhGroups$brush]) %>%
  mutate(biomass.harv.chip = rowSums(biom.harv.tot.sum[,dbhGroups$chip],na.rm=T)) %>%
  mutate(biomass.harv.small = rowSums(biom.harv.tot.sum[,dbhGroups$small],na.rm=T)) %>%
  mutate(biomass.harv.large = rowSums(biom.harv.tot.sum[,dbhGroups$large],na.rm=T)) %>%
  mutate(biomass.forbioenergy.removed.shorttons = (biomass.harv.brush + biomass.harv.chip)/907185) %>%
  mutate(trees.removed.chip.small.large = rowSums(stems.harv.sum[,-dbhGroups$brush],na.rm=T)) %>%
  mutate(trees.removed.brush = stems.harv.sum[,dbhGroups$brush]) %>%
  mutate(trees.removed.chip = rowSums(stems.harv.sum[,dbhGroups$chip],na.rm=T)) %>%
  mutate(trees.removed.small = rowSums(stems.harv.sum[,dbhGroups$small],na.rm=T)) %>%
  mutate(trees.removed.large = rowSums(stems.harv.sum[,dbhGroups$large],na.rm=T)) %>%
  mutate(dbh.avg.harv.all.in = rowSums(stems.harv.sum.dbh.weighted,na.rm=T) / rowSums(stems.harv.sum,na.rm=T)) %>%
  mutate(dbh.avg.harv.all.in = case_when(rowSums(stems.harv.sum,na.rm=T)==0 ~ 0, TRUE ~ dbh.avg.harv.all.in)) %>% 
  mutate(dbh.avg.harv.brushcut.in = stems.harv.sum.dbh.weighted[,dbhGroups$brush] / stems.harv.sum[,dbhGroups$brush]) %>%
  mutate(dbh.avg.harv.brushcut.in = case_when(stems.harv.sum[,dbhGroups$brush]==0 ~ 0, TRUE ~ dbh.avg.harv.brushcut.in)) %>% 
  mutate(dbh.avg.harv.chip.in = rowSums(stems.harv.sum.dbh.weighted[,dbhGroups$chip],na.rm=T) / rowSums(stems.harv.sum[,dbhGroups$chip],na.rm=T)) %>%
  mutate(dbh.avg.harv.chip.in = case_when(rowSums(stems.harv.sum[,dbhGroups$chip],na.rm=T)==0 ~ 0, TRUE ~ dbh.avg.harv.chip.in)) %>% 
  mutate(dbh.avg.harv.smalllog.in = rowSums(stems.harv.sum.dbh.weighted[,dbhGroups$small],na.rm=T) / rowSums(stems.harv.sum[,dbhGroups$small],na.rm=T)) %>%
  mutate(dbh.avg.harv.smalllog.in = case_when(rowSums(stems.harv.sum[,dbhGroups$small],na.rm=T)==0 ~ 0, TRUE ~ dbh.avg.harv.smalllog.in)) %>% 
  mutate(dbh.avg.harv.largelog.in = rowSums(stems.harv.sum.dbh.weighted[,dbhGroups$large],na.rm=T) / rowSums(stems.harv.sum[,dbhGroups$large],na.rm=T)) %>%
  mutate(dbh.avg.harv.largelog.in = case_when(rowSums(stems.harv.sum[,dbhGroups$large],na.rm=T)==0 ~ 0, TRUE ~ dbh.avg.harv.largelog.in)) %>% 
  mutate(twitchvol.avg.harv.all.cuft = rowSums(stems.harv.weighted.by.bole.sum,na.rm=T) / rowSums(stems.harv.sum,na.rm=T)) %>%
  mutate(twitchvol.avg.harv.all.cuft = case_when(rowSums(stems.harv.sum,na.rm=T)==0 ~ 0, TRUE ~ twitchvol.avg.harv.all.cuft)) %>% 
  mutate(twitchvol.avg.harv.brushcut.cuft = stems.harv.weighted.by.bole.sum[,dbhGroups$brush] / stems.harv.sum[,dbhGroups$brush]) %>%
  mutate(twitchvol.avg.harv.brushcut.cuft = case_when(stems.harv.sum[,dbhGroups$brush]==0 ~ 0, TRUE ~ twitchvol.avg.harv.brushcut.cuft)) %>% 
  mutate(twitchvol.avg.harv.chip.cuft = rowSums(stems.harv.weighted.by.bole.sum[,dbhGroups$chip],na.rm=T) / rowSums(stems.harv.sum[,dbhGroups$chip],na.rm=T)) %>% 
  mutate(twitchvol.avg.harv.chip.cuft = case_when(rowSums(stems.harv.sum[,dbhGroups$chip],na.rm=T)==0 ~ 0, TRUE ~ twitchvol.avg.harv.chip.cuft)) %>% 
  mutate(twitchvol.avg.harv.smalllog.cuft = rowSums(stems.harv.weighted.by.bole.sum[,dbhGroups$small],na.rm=T) / rowSums(stems.harv.sum[,dbhGroups$small],na.rm=T)) %>%
  mutate(twitchvol.avg.harv.smalllog.cuft = case_when(rowSums(stems.harv.sum[,dbhGroups$small])==0 ~ 0, TRUE ~ twitchvol.avg.harv.smalllog.cuft)) %>% 
  mutate(twitchvol.avg.harv.largelog.cuft = rowSums(stems.harv.weighted.by.bole.sum[,dbhGroups$large],na.rm=T) / rowSums(stems.harv.sum[,dbhGroups$large],na.rm=T)) %>%
  mutate(twitchvol.avg.harv.largelog.cuft = case_when(rowSums(stems.harv.sum[,dbhGroups$large],na.rm=T)==0 ~ 0, TRUE ~ twitchvol.avg.harv.largelog.cuft)) %>% 
  mutate(timber.pine.10.14.cuft = rowSums(cuft.harv.sum.pine[,dbhGroups$timb.10.14],na.rm=T)) %>%
  mutate(timber.pine.14.20.cuft = rowSums(cuft.harv.sum.pine[,dbhGroups$timb.14.20],na.rm=T)) %>%
  mutate(timber.pine.20.40.cuft = rowSums(cuft.harv.sum.pine[,dbhGroups$timb.20.40],na.rm=T)) %>%
  mutate(timber.pine.40plus.cuft = cuft.harv.sum.pine[,dbhGroups$timb.40plus]) %>%
  mutate(timber.truefir.10.14.cuft = rowSums(cuft.harv.sum.truefir[,dbhGroups$timb.10.14],na.rm=T)) %>%
  mutate(timber.truefir.14.20.cuft = rowSums(cuft.harv.sum.truefir[,dbhGroups$timb.14.20],na.rm=T)) %>%
  mutate(timber.truefir.20.40.cuft = rowSums(cuft.harv.sum.truefir[,dbhGroups$timb.20.40],na.rm=T)) %>%
  mutate(timber.truefir.40plus.cuft = cuft.harv.sum.truefir[,dbhGroups$timb.40plus]) %>%
  mutate(timber.cedarseq.10.14.cuft = rowSums(cuft.harv.sum.cedarseq[,dbhGroups$timb.10.14],na.rm=T)) %>%
  mutate(timber.cedarseq.14.20.cuft = rowSums(cuft.harv.sum.cedarseq[,dbhGroups$timb.14.20],na.rm=T)) %>%
  mutate(timber.cedarseq.20.40.cuft = rowSums(cuft.harv.sum.cedarseq[,dbhGroups$timb.20.40],na.rm=T)) %>%
  mutate(timber.cedarseq.40plus.cuft = cuft.harv.sum.cedarseq[,dbhGroups$timb.40plus])
}
  
landis.summary <- landis.summary %>%
 filter(!is.na(stand))
