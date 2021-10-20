opcost.means.ltb <- tibble()
opcost.means.ltw <- tibble()

for (ti in 1:length(names(opcost.yrlist))) {

  tmp <- opcost.yrlist[[(ti)]] %>%
    select_if((is.numeric))
  
  tmp.ltw <- tmp %>% filter(ltw==1)
    
  tmp.means.ltb <- as.tibble(t(colSums(tmp,na.rm=T))/83241) %>%
    mutate(scenario = 2) %>%
    mutate(replicate = 3) %>%
    mutate(year = ti)
  
  tmp.means.ltw <- as.tibble(t(colSums(tmp.ltw,na.rm=T))/23778) %>%
    mutate(scenario = 2) %>%
    mutate(replicate = 3) %>%
    mutate(year = ti)
  
  opcost.means.ltb <- bind_rows(opcost.means.ltb,tmp.means.ltb) 
  opcost.means.ltw <- bind_rows(opcost.means.ltb,tmp.means.ltw) 
  
}