############## INTRO ###################################################

# Control file to run FIA-LANDIS-OpCost analysis from one place 

# Steps 1 through 3 only have to be done once.
# Step 4 is done for every replicate.

# Tim Holland and Sam Evans
# June 2018


############### SET DIRECTORIES AND TIMESTEPS ############################
 
library(tidyverse)
library(stringr)
library(rgdal)
library(EnvStats)
library(RColorBrewer)
library(rgdal)
library(raster)

## this section sets directories that don't change for different scenarios / replicates
## lower down, code will loop through scenarios / replicates

# self-contained directory holding all relevant code, data, and replicates
dirMain <- '~/Dropbox/LTW_Landis_to_OpCost/'


# dirCode is where the code is stored
#dirCode        <- paste(dirMain,'Code/',sep="") #probably out of date at the moment
dirCode <- '~/Dropbox/LTW_Landis_to_OpCost/Code/'

# dirCSV is central folder where we keep our data CSVs and other data (slope, stand, & yarding tifs)(i.e. non-LANDIS inputs)
dirCSV        <- paste(dirMain,'CSVs_data_required/',sep="")
#dirCSV        <- '~/Dropbox/Shared_files/Lake_Tahoe_West/OpCost_R/CSVs_data_required/'

dirOutput <- paste(dirMain,'Outputs/',sep='')

# What timesteps should be used? 
# This variable will not be used until step 4, but easier to modify it here.
# Keep it small while writing code (e.g. 1:2). Final analysis should be set to 1:100
# Do not use t=zero because stand conditions need to be taken from preceding timestep 
# (i.e. harvest in t=1 is made from a stand at condition t=0)

#tSteps  <- c(1,seq(10,100,10))
tSteps <- 1:100

# keep to 2:4 (at most) until I write a way to turn off harvest analysis
# for scenario 1 (where no harvest happens)
scenlist <- 2:3

# replicates to be used (will be the same across all scenarios)
repslist <- 5:10


############### START TIMER #############################################

startTime <- Sys.time()


############### CALL ANALYSIS SCRIPTS ################################### 

stepStart <- Sys.time()
## STEP 0.5 #
# Load some grouping variables and yarding / slope vars that don't change from replicate to replicate
source(paste(dirCode,"define_groups_and_invariate_variables.R",sep=""))
print("Time elapsed step 0.5:")
print(Sys.time()-stepStart)

stepStart <- Sys.time()
## STEP 1 ##
# Load in FIA data and restructure it
source(paste(dirCode,"FIA_load_FIA_data.R",sep=""))
print("Time elapsed step 1:")
print(Sys.time()-stepStart)

stepStart <- Sys.time()
## STEP 1.5 ##
# Define a simple function to restructure some FIA conversion factors into a format suited to this analysis
source(paste(dirCode,"adjust_FIA_data_function.R",sep=""))
print("Time elapsed step 1.5:")
print(Sys.time()-stepStart)

stepStart <- Sys.time()
## STEP 2 ##
# Define the function that will estimate Weibull distribution parameters
# for a defined group of trees in FIA data
source(paste(dirCode,"FIA_dbh_dist_func_Weibull.R",sep=""))
print("Time elapsed step 2:")
print(Sys.time()-stepStart)

stepStart <- Sys.time()
## STEP 3 ##
# Use function defined above to estimate Weibull parameters for the 
# species*ageclass groupings as defined by LANDIS (using CSV provided by Alec)
source(paste(dirCode,"FIA_weibull_by_ageclass.R",sep=""))
print("Time elapsed step 3:")
print(Sys.time()-stepStart)

stepStart <- Sys.time()
## STEP 3.5 #
# Define function for adjusting FIA data (conversi on factors)
source(paste(dirCode,"adjust_FIA_data_function.R",sep=""))
print("Time elapsed step 3.5:")
print(Sys.time()-stepStart)

############### START LOOP FOR MULTIPLE REPLICATES ###################

for (repl in repslist){
  
  for (scen in scenlist){
  
    if(scen==1){harvest.yn <- FALSE} 
    if(scen>=2){harvest.yn <- TRUE} 
    
    # dirScenario - path to the folder within which replicates are held *MAKE SURE BOTH LINES ARE CONSISTENT WITH EACH OTHER*
    dirScenario <- paste(dirMain,'Landis_inputs/Scenario',scen,'/', sep='')
    scenario.num <- scen    # this must be set to accord with scenario folder above in order for harvest estimates to be correct
    
    # Define replicate: 
    replicate <- repl
    
    # determine what files to unzip from replicate
    file.list <- as.tibble(unzip(paste(dirScenario,"repzip",replicate,"/replicate", replicate, '.zip', sep=""),list=TRUE)) %>%
      rename(filepath = Name) %>%
      dplyr::select(-c(Length,Date)) %>%
      separate(filepath,sep='/',into=c("replicate","sub1","sub2","sub3"),remove=F) %>%
      filter(is.na(sub3)) %>%
      dplyr::select(-sub3) %>%
      mutate(file = case_when(is.na(sub2) ~ sub1, 
                              !is.na(sub2) ~ sub2)) %>%
      filter(!grepl("smolder-consumption",filepath)) %>%
      filter(!grepl("special-dead-wood",filepath)) %>%
      filter(!grepl("flaming-consumption",filepath)) %>%
      filter(!grepl("fire-spread-probability",filepath)) %>%
      filter(!grepl("day-of-fire",filepath)) %>%
      filter(sub1 %in% c("harvest","scrapple-fire","spp-bio-age","biomass")) %>%
      separate(file, sep="\\.", into=c("filename","filetype"), remove=F)
      
    # unzip replicate folder  
    unzip(paste(dirScenario,"repzip",replicate,"/replicate", replicate, '.zip', sep=""), exdir=dirScenario, files = file.list$filepath)
    
    # folder for specific replicate
    dirReplicate <- paste(dirScenario,"replicate",replicate,"/",sep="")
    
  stepStart <- Sys.time()
  ## STEP 4 ##
  # Import LANDIS biomass data and process into DHB-based size categories
  # using the Weibull parameters (of the distribution of stem-sizes for a
  # specific species*ageclass combination)
  source(paste(dirCode,"landis_import.R",sep=""))
  print("Time elapsed step 4:")
  print(Sys.time()-stepStart)
    
  source(paste(dirCode,"get_means_from_opcost_yrlist.R",sep=""))
  
  save(opcost.yrlist, file = paste(dirOutput,"opcost.yrlist.scen",scenario.num,".rep",replicate,".RData",sep=""))
  save(opcost.means.ltb, file = paste(dirOutput,"opcost.means.ltb.scen",scenario.num,".rep",replicate,".RData",sep=""))
  save(opcost.means.ltw, file = paste(dirOutput,"opcost.means.ltw.scen",scenario.num,".rep",replicate,".RData",sep=""))
  #  assign(paste("opcost.yrlist.scen",scenario.num,".rep",replicate,sep=""),opcost.yrlist)

  # Remove replicate file (not the zip file though)
  unlink(paste(dirScenario,"replicate", replicate, sep=""), recursive=TRUE)
  
  
  }
}

############### STOP TIMER ##############################################

print("Total time elapsed:")
print(Sys.time()-startTime)
# 
rm(opcost.yrlist)
rm(startTime,stepStart, startTime.ti)
rm(dirCSV,dirCode,dirOutput,dirScenario,dirFIA,dirReplicate,replicate,scenario.num,startTime,tSteps) # internal to this file
rm(dbhGroups,slope,spec.groups,stands.map,unitConv,yarding,dbhColSeq,dbhColSeq.timber) # introduced by "define_groups_and_invariate_variables.R"
rm(plots.all,trees.all) # introduced by "FIA_load_FIA_data.R"
rm(dbh.weibull) # introduced by "FIA_dbh_dist_func_Weibull.R"
rm(age_classes,ageclass_dbh,g.stem.dbh,prop.biom.dbh) # introduced by "FIA_Weibull_by_ageclass.R"
rm(adjust_FIA_data) # introdced by "adjust_FIA_data_function.R"
rm(t.full)
rm(stems.harv.stack,stems.harv.sum,stems.harv.sum.dbh.weighted,stems.harv.weighted.by.bole.stack,stems.harv.weighted.by.bole.sum)
rm(cuft.harv.stack.cedarseq,cuft.harv.stack.pine,cuft.harv.stack.truefir,cuft.harv.sum.cedarseq,cuft.harv.sum.pine,cuft.harv.sum.pine,cuft.harv.sum.truefir)
rm(convert.biom.to.cuft,convert.nstems.to.bole.vol,harv.prop.dbh,harv.prop.dbh.hand,harv.prop.dbh.mech)
rm(files.biomass,harv.prop.dbh,harvTable,landis.summary,spec.codes,species.list)
rm(ageclass_biom_dbh_breaks,ConvertingBiomassStemsForOutput,ProcessHarvestData)
rm(harv)
rm(stems.stnd.stack,stems.stnd.sum)
