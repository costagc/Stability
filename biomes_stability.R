library(raster)
library(maptools)
library(rgdal)

setwd("~/Documents/Dados/Rasters/1k layers")
landmask<-raster("000/landmask.asc")
bioclim_01<-raster("000/bioclim_01.asc")/landmask
bioclim_12<-raster("000/bioclim_12.asc")/landmask
bioclim_15<-raster('000/bioclim_15.asc')/landmask

present_vars_list<-list(bioclim_01,bioclim_12,bioclim_15);rm(bioclim_01,bioclim_12,bioclim_15)

WWFbiomes<-readOGR(dsn='/Users/gabriel/Dropbox/Diversos/GIS shapes/WWF Ecoregions', layer='ecoregions_SA')

#1 = Tropical moist forests
#2 = Dry broadleaf forests
#4 = Temperate forests
#7 = Cerrado (savanna)
#8 = Patagonian grasslands
#9 = Pantanal
#10 = Montane grasslands
#12 = Deserts
#13 = Xeric shrublands (Caatinga)
#14 = Mangrooves
#98 = Lake
#99 = Rock and Ice

WWFbiomes$BIOME[WWFbiomes$BIOME==2]<-13
WWFbiomes$BIOME[WWFbiomes$BIOME==9]<-7

biomes_rst<-list()

slots<-cbind(c(sort(unique(WWFbiomes$BIOME))),1:10)


for(i in sort(unique(WWFbiomes$BIOME))){
  e<-extent(WWFbiomes[WWFbiomes$BIOME==i,])
  ebiome<-crop(present_vars_list[[1]],e,snap="out")
  crop <- setValues(ebiome, NA)
  myshp.r <- rasterize(WWFbiomes[WWFbiomes$BIOME==i,], crop,field=i)
  biomes_rst[[slots[slots[,1]==i,2]]]<-myshp.r
}



varsbybiomes<-list(list(),list(),list())
slots<-cbind(c(sort(unique(WWFbiomes$BIOME))))

for(j in 1:length(present_vars_list)){
  for(i in sort(unique(WWFbiomes$BIOME))){
    e<-extent(WWFbiomes[WWFbiomes$BIOME==i,])
    ebiome<-crop(present_vars_list[[j]],e,snap="out")
    crop <- setValues(ebiome, NA)
    myshp.r <- rasterize(WWFbiomes[WWFbiomes$BIOME==i,], crop) 
    varsbybiomes[[j]][[which(slots==i)]] <- mask(x=ebiome, mask=myshp.r)
  }
}

par(mfrow=c(1,3))

hist(varsbybiomes[[1]][[1]],col="grey",breaks=20,freq=F)
hist(varsbybiomes[[1]][[4]],add=T,col="white",breaks=20,freq=F)

hist(varsbybiomes[[2]][[1]],col="grey",breaks=20,freq=F)
hist(varsbybiomes[[2]][[4]],add=T,col="white",breaks=20,freq=F)

hist(varsbybiomes[[3]][[1]],col="grey",breaks=20,freq=F)
hist(varsbybiomes[[3]][[4]],add=T,col="white",breaks=20,freq=F)

library(randomForest)

biome.df.list<-list()

for(i in 1:length(varsbybiomes[[1]])){
  biome.stack<-stack(varsbybiomes[[1]][[i]],varsbybiomes[[2]][[i]],varsbybiomes[[3]][[i]])
  names(biome.stack)<-c("temperature","precipitation","cv_precipitation")
  biome.df.list[[i]]<-as.data.frame(biome.stack)
}

### Model calibration

biome.df.R<-lapply(biome.df.list,FUN=function(x) as.vector(na.omit(x[sample(1:round(nrow(x)),round(nrow(x)*0.05)),])))

biomes<-list()
for(i in 1:length(biome.df.R)){
  biomes[[i]]<-rep(i,nrow(biome.df.R[[i]]) ) 
  }

biomes<-do.call("c", biomes)
RF_data<-data.frame(do.call("rbind", biome.df.R),biomes)

mod_RF<-randomForest(as.factor(biomes)~temperature+precipitation+cv_precipitation,data=RF_data, localImp=T)

print(mod_RF)
importance(mod_RF)
varImpPlot(mod_RF)
names(mod_RF)
mod_RF$localImp

?randomForest


### Model prediction data

present_vars<-stack(present_vars_list)
names(present_vars)<-c("temperature","precipitation","cv_precipitation")

SA_data<-as.data.frame(present_vars)
SA_data<-na.omit(SA_data)

pred_Rforest<-predict(mod_RF,SA_data)

biomes_pred<-present_vars[[3]]
biomes_pred[!is.na(biomes_pred[])]<-pred_Rforest


plot(biomes_pred)



var="bioclim_01"
bio01_list<-lapply(list.files(rec=T,pattern=paste(var,".asc",sep=""),full=T),raster)
var="bioclim_12"
bio12_list<-lapply(list.files(rec=T,pattern=paste(var,".asc",sep=""),full=T),raster)
var="bioclim_15"
bio15_list<-lapply(list.files(rec=T,pattern=paste(var,".asc",sep=""),full=T),raster)


vars_list<-list(bio01_list,bio12_list,bio15_list)
rm(bio01_list,bio12_list,bio15_list)

biomes.list<-list()

for(i in 1:length(vars_list[[1]])){
  past_vars<-stack(vars_list[[1]][[i]]/landmask,vars_list[[2]][[i]]/landmask,vars_list[[3]][[i]]/landmask)
  names(past_vars)<-c("temperature","precipitation","cv_precipitation")
  SA_past<-as.data.frame(past_vars)
  SA_past<-na.omit(SA_past)
  past_Rforest<-predict(mod_RF,SA_past)
  biomes_pred_past<-present_vars[[3]]
  biomes_pred_past[!is.na(biomes_pred[])]<-past_Rforest
  biomes.list[[i]]<-biomes_pred_past
}

plot(biomes.list[[1]])

stability<-landmask
stability[which(getValues(stability)==1)]<-0

for(i in 1:c(length(biomes.list)-1)){
  stability[which(getValues(biomes.list[[i]])!=getValues(biomes.list[[i+1]]))]<-stability[which(getValues(biomes.list[[i]])!=getValues(biomes.list[[i+1]]))] + 1
}


blue2red <- colorRampPalette(c("darkblue","darkblue","blue","yellow","orange","orange","red","red","darkred"),space = "rgb")

### Model fit

slots<-cbind(c(sort(unique(WWFbiomes$BIOME))))

biomes_SA_list<-list()

for(i in 1:10){
  biomes_SA_list[[i]] <- rasterize(WWFbiomes[WWFbiomes$BIOME==slots[i,1],], landmask,field=i)
}

biomes_rst<-landmask
for(i in 1:10){
  biomes_rst[!is.na(biomes_SA_list[[i]])]<-biomes_SA_list[[i]][!is.na(biomes_SA_list[[i]])]
}
plot(biomes_rst,col=c("darkgreen","turquoise4","orange","yellow","orange4","red","indianred3","purple","black","black"),main="Biomes",legend=F)

RF_fit_table<-matrix(nr=nrow(SA_data),nc=100)

for(i in 46:100){
  biome.df.R_<-lapply(biome.df.list,FUN=function(x) as.vector(na.omit(x[sample(1:round(nrow(x)),round(nrow(x)*0.05)),])))
  biomes_<-list()
  for(j in 1:length(biome.df.R_)){
    biomes_[[j]]<-rep(slots[j,],nrow(biome.df.R_[[j]]) ) 
  }
  biomes_<-do.call("c", biomes_)
  RF_data_<-data.frame(do.call("rbind", biome.df.R_),as.factor(biomes_))
  mod_RF_<-randomForest(as.factor(biomes_)~temperature+precipitation+cv_precipitation,data=RF_data_)
  pred_Rforest_<-predict(mod_RF_,SA_data)
  RF_fit_table[,i]<-as.numeric(as.character(pred_Rforest_))
}

biomes_data<-biomes_rst[!is.na(present_vars[[3]][])]

RF_fit_table[biomes_data==13,2]

biomes_acc<-landmask
biomes_acc[]<-NA
biomes_acc[!is.na(present_vars[[3]][])]<-rowMeans(apply(RF_fit_table[,1:45],2,function(x) biomes_data==x))

biomes_rst[biomes_rst>7]<-NA
stability[is.na(biomes_rst)]<-NA
biomes_acc[is.na(biomes_rst)]<-NA


windows(14,7)
par(mfrow=c(1,2))
plot(biomes_rst,col=c("darkgreen","turquoise4","orange","yellow","orange4","red","indianred3"),main="Biomes",legend=F)
plot(stability,col=blue2red(21),main="Stability")
require(gplots)
windows(14,7)
par(mfrow=c(1,2))
plot(biomes_rst,col=c("darkgreen","turquoise4","orange","yellow","orange4","red","indianred3"),main="Biomes",legend=F)
plot(stability,col=rich.colors(21),main="Stability")

#1 = Tropical moist forests
#2 = Temperate woodlands
#3 = Savannas
#4 = Patagonia grasslands
#5 = Montane grasslands
#6 = Deserts
#7 = Seasonal Dry Forests

plot(biomes_acc,col=rev(blue2red(25)),main="Accuracy")
plot(biomes_acc,col=rev(rich.colors(25)),main="Accuracy")


writeRaster(stability,"stability.asc")
writeRaster(biomes_rst,"biomes.asc")

