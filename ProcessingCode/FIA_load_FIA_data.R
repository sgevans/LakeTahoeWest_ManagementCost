library(tidyverse)

#### KNOWN ERROR: if you run this code and see the following error message: 
##  "unable to find an inherited method for function ‘select’ for signature ‘"tbl_df"’"
## detach the package "raster" (there is a conflict with function 'select')

dirFIA <- paste(dirCSV,"fia_data/",sep="") 

spec.codes <- read_csv(paste(dirFIA,"Landis_species_codes.csv",sep=""))

plp2 <- read_csv(paste(dirFIA,"PLOT_PNW_2010_CA_2001-2010.csv",sep="")) %>%
  dplyr::select(INVYR,PLT_CN,ADJ_FACTOR_MACR,ADJ_FACTOR_SUBP,ADJ_FACTOR_MICR,ADJ_FACTOR_EXPCURR,EXPVOL,EXPCURR)

plp <- read_csv(paste(dirFIA,"PLOT_PNW_CA_2005-2016.csv",sep="")) %>%
  dplyr::select(INVYR,PLT_CN,ADJ_FACTOR_MACR,ADJ_FACTOR_SUBP,ADJ_FACTOR_MICR,ADJ_FACTOR_EXPCURR,EXPVOL,EXPCURR) %>%
  bind_rows(plp2) %>%
  filter(!is.na(EXPVOL)) %>%
  distinct(PLT_CN,.keep_all = T)

plots.all <- read_csv(paste(dirFIA,"PLOT_CA_2001-2016.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(CN,ECOSUBCD,LAT,LON) %>%
  dplyr::rename(PLT_CN=CN) %>%
  filter(str_detect(ECOSUBCD, "M261E")) #if including entire Sierra Nevada
 # filter(ECOSUBCD %in% c("M261Ej","M261Ek","M261El","M261Er","M261Eh","M261Et")) #if only including subregions within study area + buffer


tb.all <- read_csv(paste(dirFIA,"TREE_REGIONAL_BIOMASS_PNW_CA_2001-2016.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(TRE_CN,REGIONAL_DRYBIOT)

trees.all <- read_csv(paste(dirFIA,"TREE_PNWRS_VW_CA_2001-2016.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(CN,PLT_CN,BHAGE,TOTAGE,DIA,SPCD,TPA_UNADJ) %>%
  dplyr::rename(TRE_CN=CN) %>%
  right_join(plots.all,by="PLT_CN") %>%
  left_join(tb.all,by="TRE_CN") %>%
  left_join(spec.codes,by=c("SPCD"="SPCD_FIA")) %>%
  left_join(plp,by="PLT_CN") %>%
  filter(!is.na(SpecCode)) %>%
  filter(!is.na(TRE_CN)) %>%
  filter(!is.na(BHAGE)) %>%
  mutate(adj_factor_use = case_when(TPA_UNADJ==0.999188 ~ ADJ_FACTOR_MACR,
                                    TPA_UNADJ==6.018046 ~ ADJ_FACTOR_SUBP,
                                    TPA_UNADJ==74.965282 ~ ADJ_FACTOR_MICR)) %>%
  mutate(AG_biomass_lbs = REGIONAL_DRYBIOT * EXPVOL * TPA_UNADJ * adj_factor_use) %>%
  mutate(AG_carbon_met_tons = AG_biomass_lbs / 2204.62 / 2) 

#### pretty sure this is not used. Leaving it for now to double check
#age.max <- trees.all %>%
#  group_by(PLT_CN) %>%
#  summarise(maxAge = max(BHAGE),
#            dbhMean = mean(DIA),
#            dbhSD = sd(DIA),
#            nStems = n(),
#            biomass=sum(AG_carbon_met_tons)*2)

rm(plp,plp2,tb.all,dir)
