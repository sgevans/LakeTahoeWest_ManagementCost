#Requires the function dbh_weibull (defined by
#"FIA_dbu_dist_func_Weibull.R"

library(tidyverse)
library(RColorBrewer)
library(EnvStats)

c5 <- brewer.pal(5,"Spectral")
c5trans <- paste(c5,"99",sep="")

age_classes <- read_csv(paste(dirCSV,"age_classes.csv",sep=""))
diagPlots <- F

ageclass_dbh <- tibble(spec.code="a",ageclass.text="a",age.min=0,age.max=0,weib.shape=0,weib.scale=0)
seq_base <- seq(0,80,0.25)

for (i in 1:length(rownames(age_classes))){
  trees.temp <- trees.all %>%
    filter(SpecCode==as.character(age_classes[(i),"species"])) %>%
    filter(BHAGE>=as.numeric(age_classes[(i),"ageclass_min"])) %>%
    filter(BHAGE<as.numeric(age_classes[(i),"ageclass_max"]))
  w.temp1 <- dbh.weibull(trees.temp)
  w.temp2 <- dbh.weibull(trees.temp)
  w.temp3 <- dbh.weibull(trees.temp)
  w.temp4 <- dbh.weibull(trees.temp)
  w.temp5 <- dbh.weibull(trees.temp)
  ageclass_dbh[(i),"spec.code"]<-as.character(age_classes[(i),"species"])
  ageclass_dbh[(i),"ageclass.text"]<-as.character(age_classes[(i),"ageclass_text"])
  ageclass_dbh[(i),"ageclass.id"]<-as.character(age_classes[(i),"ageclass_ID"])
  ageclass_dbh[(i),"age.min"]<-as.numeric(age_classes[(i),"ageclass_min"])
  ageclass_dbh[(i),"age.max"]<-as.numeric(age_classes[(i),"ageclass_max"])
  ageclass_dbh[(i),"weib.shape"]<-mean(c(w.temp1[[1]]["shape"],w.temp2[[1]]["shape"],w.temp3[[1]]["shape"],w.temp4[[1]]["shape"],w.temp5[[1]]["shape"]))
  ageclass_dbh[(i),"weib.scale"]<-mean(c(w.temp1[[1]]["scale"],w.temp2[[1]]["scale"],w.temp3[[1]]["scale"],w.temp4[[1]]["scale"],w.temp5[[1]]["scale"]))
  for (c in seq(0,38,2)){
    ageclass_dbh[(i),paste("dbh.",str_pad(c,2,pad="0"),"_",str_pad(c+2,2,pad="0"),sep="")]<-0.5*sum(dweibull(seq(c+0.25,c+1.75,0.5),shape=as.numeric(ageclass_dbh[(i),"weib.shape"]),scale=as.numeric(ageclass_dbh[(i),"weib.scale"])))
    }

# if (diagPlots){
# jpeg(filename=paste("fit",(i),as.character(age_classes[(i),"species"]),as.character(age_classes[(i),"ageclass_text"],".jpg",sep="")),width=480,height=480)
# hist(w.temp1[[2]],col="#55555555",breaks=seq(0,80,2),
#      main=paste(as.character(age_classes[(i),"species"]),as.character(age_classes[(i),"ageclass_text"])))
# hist(w.temp2[[2]],col="#55555555",breaks=seq(0,80,2),add=T)
# hist(w.temp3[[2]],col="#55555555",breaks=seq(0,80,2),add=T)
# hist(w.temp4[[2]],col="#55555555",breaks=seq(0,80,2),add=T)
# hist(w.temp5[[2]],col="#55555555",breaks=seq(0,80,2),add=T)
# points(seq_base,2*length(w.temp1[[2]])*dweibull(seq_base,shape=as.numeric(w.temp1[[1]]["shape"]),scale=as.numeric(w.temp1[[1]]["scale"])),type="l",col="red",lwd=2)
#   dev.off()  }
  
  }

ageclass_dbh$dbh.40_100 = 1 - rowSums(ageclass_dbh[,grep("dbh.00_02", colnames(ageclass_dbh)):length(colnames(ageclass_dbh))])
ageclass_dbh[ageclass_dbh$dbh.40_100<0,"dbh.40_100"] <- 0
ageclass_dbh[,grep("dbh.00_02", colnames(ageclass_dbh)):length(colnames(ageclass_dbh))] <- ageclass_dbh[,grep("dbh.00_02", colnames(ageclass_dbh)):length(colnames(ageclass_dbh))] / rowSums(ageclass_dbh[,grep("dbh.00_02", colnames(ageclass_dbh)):length(colnames(ageclass_dbh))])

par(mfrow=c(3,2),mar=c(3,1,1,0))

for (i in 1:6){
  spec.temp <- unique(ageclass_dbh$spec.code)[(i)]
  ageclass_dbh_temp <- ageclass_dbh %>% 
    filter(spec.code==spec.temp) %>% 
    arrange(age.min)

  # plot(c(0,60),c(0,0.18),type="n",yaxt="n",main=spec.temp)
  # text(35,0.17,"Age (years)",pos=4,cex=1.1)
  # text(35,c(0.15,0.135,0.12,0.105,0.09),ageclass_dbh_temp$ageclass.text,col=rev(c5),pos=4,cex=1.1)

  # seq_weibull <- dweibull(seq_base,shape=as.numeric(ageclass_dbh_temp[1,"weib.shape"]),scale=as.numeric(ageclass_dbh_temp[1,"weib.scale"]))
  # points(seq_base,seq_weibull,type="l",col=c5[5],lwd=2)
  # seq_weibull <- dweibull(seq_base,shape=as.numeric(ageclass_dbh_temp[2,"weib.shape"]),scale=as.numeric(ageclass_dbh_temp[2,"weib.scale"]))
  # points(seq_base,seq_weibull,type="l",col=c5[4],lwd=2)
  # seq_weibull <- dweibull(seq_base,shape=as.numeric(ageclass_dbh_temp[3,"weib.shape"]),scale=as.numeric(ageclass_dbh_temp[3,"weib.scale"]))
  # points(seq_base,seq_weibull,type="l",col=c5[3],lwd=2)
  # seq_weibull <- dweibull(seq_base,shape=as.numeric(ageclass_dbh_temp[4,"weib.shape"]),scale=as.numeric(ageclass_dbh_temp[4,"weib.scale"]))
  # points(seq_base,seq_weibull,type="l",col=c5[2],lwd=2)
  # seq_weibull <- dweibull(seq_base,shape=as.numeric(ageclass_dbh_temp[5,"weib.shape"]),scale=as.numeric(ageclass_dbh_temp[5,"weib.scale"]))
  # points(seq_base,seq_weibull,type="l",col=c5[1],lwd=2)
}

par(mfrow=c(1,1))

biomass <- trees.all %>%
  filter(!is.na(DIA)&!is.na(REGIONAL_DRYBIOT)) %>%
  mutate(dbh.cut = cut(DIA,breaks=c(seq(0,40,2),100),right=F)) %>%
  group_by(dbh.cut,SpecCode) %>%
  summarise(biom.lbs=mean(REGIONAL_DRYBIOT,na.rm=T)) %>%
  separate(dbh.cut,sep=",",into=c("dbh.start","dbh.end")) %>%
  mutate(dbh.end=as.numeric(gsub("[^0-9.]", "", dbh.end))) %>%
  mutate(dbh.start=as.numeric(gsub("[^0-9.]", "", dbh.start))) %>%
  mutate(dbh.bin=paste("dbh.",str_pad(dbh.start,2,pad="0"),"_",str_pad(dbh.end,2,pad="0"),sep=""))

ageclass_dbh <- ageclass_dbh %>%
  gather(value=prop.stems,key=dbh.bin,colnames(ageclass_dbh)[8:28]) %>%
  full_join(biomass,by=c("dbh.bin","spec.code"="SpecCode")) %>%
  mutate(biom.intvar=biom.lbs*prop.stems)

temp <- ageclass_dbh %>%
  group_by(spec.code,ageclass.text) %>%
  summarise(biom.grptot = sum(biom.intvar,na.rm=T))

ageclass_dbh <- ageclass_dbh %>%
  left_join(temp,by=c("spec.code","ageclass.text")) %>%
  mutate(prop.biom=biom.intvar/biom.grptot) %>%
  mutate(biom.g.stem = biom.lbs * 453.6) %>%
  dplyr::select(-c(biom.intvar,biom.lbs,biom.grptot)) %>%
  arrange(spec.code,ageclass.id)

prop.biom.dbh <- ageclass_dbh %>% 
  dplyr::select(c(spec.code,ageclass.id,dbh.bin,prop.biom)) %>%
  spread(key=dbh.bin,value=prop.biom) 

g.stem.dbh <- ageclass_dbh %>% 
  dplyr::select(c(spec.code,ageclass.id,dbh.bin,biom.g.stem)) %>% 
  spread(key=dbh.bin,value=biom.g.stem) %>% # reshape so each size class is a column
  filter(ageclass.id==1) 


rm(temp,trees.temp,w.temp1,w.temp2,w.temp3,w.temp4,w.temp5,ageclass_dbh_temp,c,c5,c5trans,diagPlots,i,seq_base,spec.temp,biomass)
#rm(seq_weibull)
