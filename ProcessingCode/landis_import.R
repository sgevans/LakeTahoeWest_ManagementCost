############### INTRO ###################################################

# Imports and processes LANDIS spatial (.img) data.

# Loads LANDIS maps on standing biomass and harvested biomass
# and calculates breakdown by biomass and by number of stems for 
# standing biomass by speciesas well as for biomass harvested by species
#calculates biomass in each size class, by each species

#requires: ageclass_dbh (a tibble made by FIA_weibull_by_ageclass.R)
#requires packages rgdal and tidyverse

# a note on units: harvest data from Landis is in Mg/ha (i.e. Mg/pixel) while
# biomass data from Landis is in g/m. This code translates everything into g/ha 
# (i.e. it multiples harvest data by 1000000 and biomass by 10000)

# Tim Holland & Sam Evans
# July 2018

############### PACKAGES REQUIRED #######################################

library(rgdal)
library(tidyverse)


############### CREATE FILES TABLE FOR BIOMASS AND SPECIES LIST ##########

setwd(dirReplicate) # set to sub-folder for biomass by age

# get list of files from the folder and create a tibble with their attributes

files <- as.tibble(list.files('spp-bio-age/')) %>%  
  rename('file'=value) %>% 
  filter(str_detect(file,'.img')) %>% # only interested in the acutal map files (the .img ones)
  separate(file,into=c('spec.code','ageclass','timestep'),sep='-',remove=FALSE) %>% # pulling out info that describes what each file is
  mutate(timestep=gsub('.img','',timestep)) %>% # cleaning
  mutate(ageclass_n = as.numeric(gsub('ageclass','',ageclass))) %>% # giving ageclass a numeric variable
  mutate(timestep = as.numeric(timestep)) %>% # giving timestep a numeric variable
  mutate(file = paste("spp-bio-age/",file,sep="")) %>%
  arrange(timestep,spec.code,ageclass)

species.age <- unique(files$spec.code)

files.no.age <- as.tibble(list.files('biomass')) %>%
  rename('file'=value) %>% 
  filter(str_detect(file,'.img')) %>% # only interested in the acutal map files (the .img ones)
  separate(file,into=c('spec.code','timestep'),sep='-',remove=FALSE) %>% # pulling out info that describes what each file is
  mutate(timestep=gsub('.img','',timestep)) %>% # cleaning
  mutate(timestep = as.numeric(timestep)) %>%
  mutate(file = paste("biomass/",file,sep="")) %>%
  filter(!(spec.code %in% species.age)) %>% # make sure it does not duplicate any species that have been split by ageclass in spp-bio-age
  mutate(ageclass = "ageclass1") %>%
  mutate(ageclass_n = 1)

files <- bind_rows(files,files.no.age) %>%
  filter(!(spec.code %in% c("FixnResp","FixnSeed","NonnResp","NonnSeed","TotalBiomass")))
  
# return set of unique species names
species <- unique(files$spec.code)
species.list <- as.list(species) # create a simple list that just has species names
names(species.list) <- species # each item of the list named by species so any list based on this template will be similarly named

files.biomass <- as.tibble(list.files('biomass')) %>%
  rename('file'=value) %>% 
  filter(str_detect(file,'.img')) %>% # only interested in the acutal map files (the .img ones)
  separate(file,into=c('spec.code','timestep'),sep='-',remove=FALSE) %>% # pulling out info that describes what each file is
  mutate(timestep=gsub('.img','',timestep)) %>% # cleaning
  mutate(timestep = as.numeric(timestep)) %>%
  mutate(file = paste("biomass/",file,sep=""))

if (harvest.yn) { #this prevents the following two sections from being run in Scenario1 (where no harvest takes place)
############### LOAD LANDIS HARVEST DATA (TABULAR) AND BIOMASS-CUFT CONVERSIONS ####

# this takes tabular harvest data for all years. harv.ti will (later) be created
# by filtering by all years. 
harv <- read_csv(paste(dirReplicate,"harvest/log.csv",sep="")) %>%
  dplyr::select(-starts_with("X")) %>%
  dplyr::select(-starts_with("Cohorts"))

# this takes a table that was created by dividing one FIA table by another: the total 
# AG biomass harvested by species, state-wide in California by the cubic feet of timber 
# produced by species, state-wide. The ratio between these two, that is broken down by size-class
# is a real-world esetimate of how much timber is produced by biomass of different species*size
# groups
convert.biom.to.cuft <- adjust_FIA_data(read_csv(paste(dirCSV,"SR18_by_29_merch_bole_cuft_per_dry_short_ton_biomass.csv",sep="")))

convert.nstems.to.bole.vol <- adjust_FIA_data(read_csv(paste(dirCSV,"SR18_by_9_merch_bole_cuft_per_tree.csv",sep="")))


############### LOAD HARVEST PRESCRIPTIONS ##############################

# the proportions used here are based on estimated from a combination of timber
# cruise data and harvest data in the Lake Tahoe basin, compiled & analyzed 
# by Nadia Tase. Initially, those reported proportions only included scenarios 2
# and 3. However, scenario 1 is no management except fire suppression, so we 
# have simply set the harvest values to zero. Scenario 4 is the same as scenario
# 2 as far as harvest goes (the difference has to do with use of prescribed fire)

harv.prop.dbh <- read_csv(paste(dirCSV,"harvest_scenarios_percent_removals.csv",sep="")) %>%
  mutate(dbh.bin=paste("dbh.",str_pad(dbh.LL,width=2,side="left",pad="0"),"_",str_pad(dbh.UL,width=2,side="left",pad="0"),sep="")) %>%
  gather(key=scen_syst, value=perc.harv,
         scen1.mech,scen1.hand,scen2.mech,scen2.hand,scen3.mech,scen3.hand,scen4.mech,scen4.hand) %>% # reshape data
  mutate(prop.harv = perc.harv/100) %>% # convert percentages to proportions for simpler multiplication later
  separate(scen_syst,c("scenario","system")) %>% # add variable for scenario and hand vs. mech system
  filter(scenario==paste("scen",scenario.num,sep="")) %>% # only one scenario is relevant for each run. 'scenario.num' is defined in control file
  dplyr::select(c(dbh.bin,system,prop.harv))

# create two vectors, one for hand and one for mech, that both represent the proportion harvested at each size class under each system
harv.prop.dbh.hand <- harv.prop.dbh %>%
  filter(system=="hand") %>%
  dplyr::select(-system) %>%
  spread(key=dbh.bin,value=prop.harv) %>%
  dplyr::select(dbhColSeq)

harv.prop.dbh.mech <- harv.prop.dbh %>%  # same as above, for mechanical system
  filter(system=="mech") %>%
  dplyr::select(-system) %>%
  spread(key=dbh.bin,value=prop.harv) %>%
  dplyr::select(dbhColSeq)

harv.prop.dbh.hand <- as.numeric(harv.prop.dbh.hand[1,]) # convert tibble into single vector (corresponding to first row of tibble)
harv.prop.dbh.mech <- as.numeric(harv.prop.dbh.mech[1,])

} #end harvest data section (that gets turned off in Scenario1)


############### DEFINE FUNCTION FOR MAKING DBH MATRIX ########################

ageclass_biom_dbh_breaks <- function(age,files.tib, prop.biom){
  # This function takes a vector of biomass values (representing pixels) and decomposes it
  # into dbh classes based on proportions worked out from FIA data.
  dbh.mat <- prop.biom %>% 
    filter(ageclass.id==(age)) %>%
    dplyr::select(dbhColSeq)   
  img <- readGDAL(paste(dirReplicate,files.tib$file[files.tib$ageclass_n==age],sep=""))@data*10000 # convert all biomass data (in g/m2) to g/ha at import
  img.matrix <- outer(img$band1,as.numeric(dbh.mat[1,]),FUN='*')
  img.matrix[is.na(img.matrix)] <- 0
  return(img.matrix)
}


############## IMPORT MANAGEMENT IMGs ########################################

ProcessHarvestData <- function(ti){
  
# subset harvest data to only this timestep
harv.ti <- harv %>% filter(Time==ti)

# load data from image file
# convert to tibble and merge with stand numbers
harvTable <- as.tibble(readGDAL(paste(dirReplicate,"harvest/prescripts-",ti,".img",sep=""))@data) %>%
  mutate(sort = row_number()) %>% # get sort order as a safeguard for future sort problems
  mutate(stand=stands.map@data$band1) %>% # get stand numbers
  rename(prescript = band1) %>%
  mutate(rx.hand = case_when(prescript==2 ~ 1, # create binary variable for hand treatment 
                             TRUE ~ 0)) %>% 
  mutate(rx.mech = case_when(prescript==3 ~ 1, # create binary variable for mech treatment
                             TRUE ~ 0)) %>%
  left_join(harv.ti,by=c("stand"="Stand")) %>%
  arrange(sort) %>% # make sure no sort errors before binding new spatial data
  bind_cols(as.tibble(readGDAL(paste(dirReplicate,"harvest/biomass-removed-",ti,".img",sep=""))@data * 10000)) %>%
  rename(biom.removed.total = band1) %>%
  mutate(biom.removed.hand = biom.removed.total * rx.hand) %>%
  mutate(biom.removed.mech = biom.removed.total * rx.mech) %>%
  dplyr::select(-contains("Cohorts")) %>%
  dplyr::select(-starts_with("X")) %>%
  dplyr::select(-c(NumberOfSites,HarvestedSites,MgBioRemovedPerDamagedHa))

return(harvTable)
}


############### PROCESS IMGs OF STANDING BIOMASS #############################

# notes on loop vs apply structure: 
# this code cycles through data on three levels: timestep (100), species (8), and ageclass-within-species (usually 5).
# Using 'apply' is faster than looping (double-checked with these .img files specifically), so we use functions with lapply
# to cycle through the species and ageclasses. However, for the time being, we are using a loop structure
# to cycle through timesteps. The reason for the decision to stick with loops for timesteps was honestly 
# just that lists nested inside lists being operated upon by functions nested within functions was making my 
# brain hurt at a certain point. There are only 100 timesteps, so I don't think the time penalty will be too bad.

source(paste(dirCode,"convert_biomass_stems_for_output_function.R",sep=""))

opcost.yrlist <- list()

for (ti in tSteps) {
  startTime.ti <- Sys.time()
  if(harvest.yn){harvTable <- ProcessHarvestData(ti)}
  t.full <- lapply(species.list,ConvertingBiomassStemsForOutput, ti=ti, harvTable=harvTable, harvest.yn=harvest.yn)
  source(paste(dirCode,"process_into_landis_outputs.R",sep=""))
  opcost.yrlist[[paste('t',str_pad(ti,3,'0',side='left'),sep='')]] <- landis.summary
  print("Time elapsed for timestep:")
  print(Sys.time()-startTime.ti)
  print("Total time elapsed so far")
  print(Sys.time()-startTime)
    }



############### CLEAN-UP ##################################################### 

# rm(dbh.cols,tempDBH,tempIMG,filesTemp)
# #rm(tempPROD,tempSUM)
# rm(ti)
# rm(files)
# rm(dbhGroups)
# rm(species,species.age)
# rm(files.no.age)