library(sp)
library(dplyr)
library(ncdf4)
library(readxl) #Excel
library(raster)
library(rgdal)
setwd("D:/MASTERGIS/ExtracionPISCO")
#..load the folder or file
Longitud_Latitud <- read_excel("Excel/Centroides.xlsx") %>% arrange_all()
subcuenca<-readOGR('SHP/Sub_Piura.shp')
View(Longitud_Latitud)
# ..load the data .nc
raster_pp<-raster::brick("DatosPISCO/PrecipDialy.nc")
View(raster_pp)
plot(raster_pp[[1]])
#...coordinate assignement
sp::coordinates(Longitud_Latitud)<- ~X+Y
plot(Longitud_Latitud)
# match the projection of raster with point to extract
raster::projection(Longitud_Latitud)<- raster::projection(raster_pp)
#...Extract the values
points_long_lati<- raster::extract(raster_pp[[1]], Longitud_Latitud, cellnumbers=T)[,1]
data_long_lati<-t(raster_pp[points_long_lati])
colnames(data_long_lati)<-as.character(Longitud_Latitud$NN) #assigment of subbasin colnames
View(data_long_lati)
#create my file cvs 
write.csv (data_long_lati, "Excel/DailyRainfall2.csv")
#Display for the all the precipitation date
plot(as.numeric(data_long_lati),
     type="p",
     col="#1B9E77",
     lwd = 3,
     xlab="Número de datos",
     main='Precipitaciones de las subcuencas de la cuenca de río Piura',
     ylab="Precipitaciones diarias (mm)")
length(as.numeric(data_long_lati))

colnames(as.numeric(data_long_lati))
colnames(data_long_lati)
rownames(data_long_lati)
View(row.names(data_long_lati))

plot(data_long_lati[1])
