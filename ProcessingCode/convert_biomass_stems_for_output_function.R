ConvertingBiomassStemsForOutput <- function(species, ti, harvTable, harvest.yn){
  # this function takes a file list for a given timestep, applies ageclass_biom_dbh_breaks
  # to all of the images in that ageclass, and summarizes.
  
  if (harvest.yn){
  harvTable.sp <- harvTable %>%   
    mutate(biom.sp = readGDAL(paste(dirReplicate,"biomass/",species,"-",(ti-1),".img",sep=""))@data$band1 * 10000) %>% 
    group_by(stand) %>%
    mutate(biom.sp.standtot = sum(biom.sp)) %>%
    ungroup() %>%
    mutate(biom.sp.propstand = case_when(biom.sp.standtot != 0 ~ biom.sp/biom.sp.standtot, TRUE ~ 0)) %>%
    mutate(harv.biom.sp.stand.gha = rowMeans(dplyr::select(.,ends_with(species))) * 1000000) %>%  # convert Mg per pixel to grams for consistency
    mutate(harv.biom.sp = harv.biom.sp.stand.gha * biom.sp.propstand) %>%
    mutate(harv.biom.sp.hand = harv.biom.sp * rx.hand) %>%
    mutate(harv.biom.sp.mech = harv.biom.sp * rx.mech) %>%
    dplyr::select(-contains("Biomass"))
  }
  
  # take prop.biom.dbh - formed one level up from ageclass_dbh - and filter by species
  # the spreading part of the ageclass_dbh modification was done outside of the loops / apply 
  # sequences so it is only done once.
  prop.biom.dbh.sp <- prop.biom.dbh %>%
    filter(spec.code==species) 
  
  # same as above with prop.biom.dbh.sp. Take object from global environment and filter by species. 
  g.stem.dbh.sp <- g.stem.dbh %>%
    filter(spec.code==species) %>%
    dplyr::select(dbhColSeq) 
  
  g.stem.dbh.sp <- as.numeric(g.stem.dbh.sp[1,])

if(harvest.yn){    
  biom.short.ton.cuft.conv <- convert.biom.to.cuft %>%
    filter(spec.code == species) %>%
    dplyr::select(-spec.code) 
  biom.cuft.per.dry.ton.conv <- as.numeric(biom.short.ton.cuft.conv[1,]) # convert (cuft / dry ton) to (cuft / green gram) 
  biom.cuft.per.green.ton.conv <- biom.cuft.per.dry.ton.conv / 2
  biom.cuft.per.green.gram.conv <- biom.cuft.per.green.ton.conv / 907185
  
  stems.bole.conv <- convert.nstems.to.bole.vol %>%
    filter(spec.code == species) %>%
    dplyr::select(-spec.code)
  stems.bole.conv <- as.numeric(stems.bole.conv[1,])}
  
  # Get the set of files that apply for this species in this timestep
  # *IMPORTANT*
  # This takes the biomass file from t-1 intead of t (aside: it will therefore fail if tSteps includes 0)
  # the reason this is being done is to establish stand conditions from the timestep
  # before harvest / management happened. Biomass files for a given timestep represent
  # conditions AFTER the management in that timestep has happened. 
  b.files.ti.sp <- files %>%
    filter(timestep==((ti)-1)) %>% # !!IMPORTANT!! See note above
    filter(spec.code==species)
  
  # now that file list is established for this timestep and each species, apply the ageclass summary function:
  
  # create list of ageclasses evaluated
  ag <- as.list(b.files.ti.sp$ageclass_n)
  
  # create stack of dbh matrices - one for each age class
  biom.stack <- lapply(ag,ageclass_biom_dbh_breaks,files.tib=b.files.ti.sp,prop.biom=prop.biom.dbh.sp) # function defined above
  
  # create single dbh matrix for the species in that timestep by adding together the ageclass matrices
  biom.stnd <- Reduce('+',biom.stack) 
  
  # the next ~12 lines create a matrix of proportions representing the proportion of harvest
  # that would be in each size category, for a given harvest system, were a pixel is indeed harvested
  # using that system. In a later step, this will be multiplied by harvest info. 
  # The following lines take two pieces of information: (1) the proportion of total standing biomass
  # that each pixel represents and (2) the proportion of standing biomass of a given size class that gets 
  # harvested under a given system (hand vs. mech). Those two proportions are multiplied by each other, and then 
  # the resulting proportion*proportion value is re-scaled again so that a given row sums to 1. 
  # Here is an example: say the 12"-14" category is 10% of total stand biomass and is harvested at 70% intensity.
  # The 18"-20" category is 5% of stand biomass and is harvested at 30% intensity. That means that, 
  # whatever the total amount harvested is, the ratio of harvested 12"-14" to 18"-20" will be 0.1*0.7 : 0.05*0.3
  # i.e. 0.07:0.015 or about 4.7:1 (i.e. 5 times as much 12-14 will be harvested because there is twice as much of it, 
  # and it is harvested 2.3 times more intensely). 
  biom.stnd.prop <- biom.stnd / rowSums(biom.stnd)  # create matrix that represents proportion of biomass in each size category
  biom.stnd.prop[is.nan(biom.stnd.prop)] <- 0 # row above creates many NaNs because of zero-sum rows. This sets those values to zero

  if (harvest.yn){
  biom.harv.hand.prop <- t(apply(biom.stnd.prop,1,function(x) x * harv.prop.dbh.hand))
  biom.harv.mech.prop <- t(apply(biom.stnd.prop,1,function(x) x * harv.prop.dbh.mech))
  
  biom.harv.hand.prop <- biom.harv.hand.prop / rowSums(biom.harv.hand.prop)
  biom.harv.hand.prop[is.nan(biom.harv.hand.prop)]  <- 0 
  
  biom.harv.mech.prop <- biom.harv.mech.prop / rowSums(biom.harv.mech.prop)
  biom.harv.mech.prop[is.nan(biom.harv.mech.prop)] <- 0 
  
  #### for some reason it is zeroing out a lot of rows in biom.harv.mech.prop. I think this might be the source of problem. 
  
  
  biom.harv.hand <- biom.harv.hand.prop * as.numeric(harvTable.sp$harv.biom.sp.hand)
  biom.harv.mech <- biom.harv.mech.prop * as.numeric(harvTable.sp$harv.biom.sp.mech)
  biom.harv.tot <- biom.harv.hand + biom.harv.mech

  cuft.harv.tot <- t(apply(biom.harv.tot,1,function(x) x * biom.cuft.per.green.gram.conv))
  
  # create a matrix of stems by pixel by dividing biomass matrix by vector of grams / stem
  # using MARGIN=1 means division is by row
  stems.harv <- t(apply(biom.harv.tot, 1, function(x) x/g.stem.dbh.sp))
  
  stems.harv.bole.vol.weighted <- t(apply(stems.harv,1,function(x) x * stems.bole.conv))
  }

  stems.stnd <- t(apply(biom.stnd, 1, function(x) x/g.stem.dbh.sp))
  
  # clean data so that zeros are filled in where they should be (i.e. in stands with zero biomass or zero harvest)
  # but NAs inserted in areas outside of study area. 
  biom.stnd[is.na(biom.stnd)]   <- 0
  stems.stnd[is.na(stems.stnd)] <- 0

  biom.stnd[is.na(stands.map@data$band1)]   <- NA
  stems.stnd[is.na(stands.map@data$band1)]  <- NA
  
  if (harvest.yn){
  biom.harv.tot[is.na(biom.harv.tot)]   <- 0
  stems.harv[is.na(stems.harv)] <- 0
  
  biom.harv.tot[is.na(stands.map@data$band1)]   <- NA
  stems.harv[is.na(stands.map@data$band1)]  <- NA
  }

  if (harvest.yn){
  return(list("biomass.standing"=biom.stnd,
              "stems.standing"=stems.stnd,
              "biomass.harvested.total"=biom.harv.tot,
              "biomass.harvested.mech"=biom.harv.mech,
              "biomass.harvested.hand"=biom.harv.hand,
              "cuft.harvested"=cuft.harv.tot,
              "stems.harvested"=stems.harv, 
              "stems.harvested.weighted.by.bole.vol"=stems.harv.bole.vol.weighted, 
              "harvest.table"=harvTable.sp))
  }

  if (!harvest.yn){
    return(list("biomass.standing"=biom.stnd,
                "stems.standing"=stems.stnd))
  }
  
}