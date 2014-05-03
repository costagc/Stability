require(gdata)
require(raster)
require(rasterVis)
require(gplots)
require(calibrate)
require(RSvgDevice)
setwd('~/Desktop/Cenosys paper/Figure raw files')
loc <- read.xls('table_marie.xlsx')

biomes <- raster('biomes.asc')
biomes <- as.factor(biomes)
stability <- raster('stability.asc')

rat <- levels(biomes)[[1]]
rat[["landcover"]] <- c('Tropical moist forests','Temperate woodlands','Savannas','Patagonia grasslands',
                        'Montane grasslands','Deserts','Seasonal Dry Forests')
levels(biomes) <- rat

pdf('fig2a.pdf',height=6,width=4.8,useDingbats=FALSE)
plot(biomes,col=c("darkgreen","turquoise4","orange","yellow","orange4","red","indianred3"),legend=F)
dev.off()

pdf('test.pdf',height=6,width=4.8,useDingbats=FALSE)
plot(stability,col=rich.colors(21))
scalebar(1000,xy=c(-55,-45),type='bar',divs=4,below='Km')
points(loc$Long,loc$Lat,col='white',pch=16,cex=1)
points(loc$Long,loc$Lat,col='red',pch=16,cex=0.7)
textxy(loc$Long,loc$Lat,loc$ID,cex=1,col='red')
dev.off()



