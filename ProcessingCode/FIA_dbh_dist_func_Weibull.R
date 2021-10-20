####
#creates a function called "dbh.weibull" that takes a set of 
#fia data at the tree level and calculates weibull parameters (2: shape & scale)
#for the distribution of dbhs in that set of trees. Much of the code
#is sorting out the different sampling intensities in different parts
#of FIA plots (subplot, plot, macroplot). It corrects for that by
#sub-sampling the over-sampled scales. This means that values will 
#differ between runs



library(EnvStats)

dir <- paste(dirCSV,"fia_data/",sep="") 

dbh.weibull <- function(trees){

tpa.factors <- c(0.999188, 6.018046, 74.965282)

tempMACR <- trees %>%
  filter(TPA_UNADJ==tpa.factors[1])
tempSUBP <- trees %>%
  filter(TPA_UNADJ==tpa.factors[2])
tempMICR <- trees %>%
  filter(TPA_UNADJ==tpa.factors[3])

tempMACR.samp<-tempMACR[sample(length(rownames(tempMACR)),round(length(rownames(tempMACR))*tpa.factors[1]/tpa.factors[1]),replace=T),]
tempSUBP.samp<-tempSUBP[sample(length(rownames(tempSUBP)),round(length(rownames(tempSUBP))*tpa.factors[2]/tpa.factors[1]),replace=T),]
tempMICR.samp<-tempMICR[sample(length(rownames(tempMICR)),round(length(rownames(tempMICR))*tpa.factors[3]/tpa.factors[1]),replace=T),]

dbh.dist <-c(tempMACR.samp$DIA,tempSUBP.samp$DIA,tempMICR.samp$DIA)
#hist(dbh.dist,breaks=seq(0,80,1),xlim=c(0,100))

weib <- eweibull(dbh.dist)

list(weib$parameters, dbh.dist)

}

rm(dir)
