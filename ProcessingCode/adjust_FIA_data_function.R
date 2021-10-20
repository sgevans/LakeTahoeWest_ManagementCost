adjust_FIA_data <- function(srTib){
# a small function to take data in the format that it is produced by FIA
# (i.e. dbh groups starting at 5-6.9, odd-number dbh groups, and larger species groups)
# and getting it to match the format used in this analysis. 
  
  
  sp.codes <- read_csv(paste(dirCSV,"species_group_FIA_to_landis_code.csv",sep=""))
  
  modTib <- srTib %>%
    rename(SpeciesGroup = 'Species group') %>%
    full_join(sp.codes, by = 'SpeciesGroup') %>%
    filter(!is.na(spec.code)) %>%
    dplyr::select(-SpeciesGroup) %>%
    mutate(dbh.00_02 = .$'5.0-6.9' * 1/6) %>%
    mutate(dbh.02_04 = .$'5.0-6.9' * 3/6) %>%
    mutate(dbh.04_06 = .$'5.0-6.9' * 5/6) %>%
    mutate(dbh.06_08 = (.$'5.0-6.9' + .$'7.0-8.9') / 2) %>%
    mutate(dbh.08_10 = (.$'7.0-8.9' + .$'9.0-10.9') / 2) %>% 
    mutate(dbh.10_12 = (.$'9.0-10.9' + .$'11.0-12.9') / 2) %>% 
    mutate(dbh.12_14 = (.$'11.0-12.9' + .$'13.0-14.9') / 2) %>% 
    mutate(dbh.14_16 = (.$'13.0-14.9' + .$'15.0-16.9') / 2) %>% 
    mutate(dbh.16_18 = (.$'15.0-16.9' + .$'17.0-18.9') / 2) %>% 
    mutate(dbh.18_20 = (.$'17.0-18.9' + .$'19.0-20.9') / 2) %>% 
    mutate(dbh.20_22 = (.$'19.0-20.9' + .$'21.0-28.9') / 2) %>% 
    mutate(dbh.22_24 = .$'21.0-28.9') %>% 
    mutate(dbh.24_26 = .$'21.0-28.9') %>% 
    mutate(dbh.26_28 = .$'21.0-28.9') %>% 
    mutate(dbh.28_30 = (.$'21.0-28.9' + .$'29.0+') / 2) %>% 
    mutate(dbh.30_32 = .$'29.0+') %>% 
    mutate(dbh.32_34 = .$'29.0+') %>% 
    mutate(dbh.34_36 = .$'29.0+') %>% 
    mutate(dbh.36_38 = .$'29.0+') %>% 
    mutate(dbh.38_40 = .$'29.0+') %>% 
    mutate(dbh.40plus = .$'29.0+') %>%
    dplyr::select(-ends_with('9')) %>%
    dplyr::select(-ends_with('+')) %>%
    dplyr::select(-starts_with('X'))
  
return(modTib)  
}