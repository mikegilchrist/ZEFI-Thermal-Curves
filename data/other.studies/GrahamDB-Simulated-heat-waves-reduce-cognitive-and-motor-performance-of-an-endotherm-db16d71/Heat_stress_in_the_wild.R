#####################
#Heat stress in the wild

#R script to accompany Danner, Coomes, and Derryberry. Simulated heat waves reduce cognitive and motor performance of an endotherm. Ecology and Evolution. 
#####################

#####################
#Table of contents

#I. Prepare spatial data
#II. Calculate the number of days that exceed threshold values for zebra finches
#III. Calculate the proportion of range that experiences temps at or above threshold during the summer
#IV. Plot maps of days equal to or above 40C and 44C
#####################

########################
#I. Prepare spatial data
########################

library("rgdal")
library("raster")

#Download temperature data from the following website and save in a folder on your computer: http://www.bom.gov.au/jsp/awap/temp/archive.jsp?colour=colour&map=maxave&year=2017&month=1&day=31&period=daily&area=nat

setwd("~/ZB summer temps 201819")
getwd()

#create list of data files
fls <- NULL
fls <- list.files() # you can add a pattern if you only want files of a certain type

#create a stack of data files
dst <- NULL
dst = stack(fls[1])
for (ifile in 2:length(fls)) dst = addLayer(dst, raster(fls[ifile]))
#plot(dst, col=terrain.colors(25))
 
 #Convert to a RasterBrick, which is similar to, but more efficient than, a RasterStack.
dstb <- brick(dst)

############################
#End I. Prepare spatial data
############################

#####################
#II. Calculate the number of days that exceed threshold values for zebra finches
#####################

# StackApply
t40 <- stackApply(dstb, indices=1, fun=function(x, ...){sum(x >= 40)})
t44 <- stackApply(dstb, indices=1, fun=function(x, ...){sum(x >= 44)})
plot(t40, col=c(0, rev(heat.colors(25))))
plot(t44, col=c(0, rev(heat.colors(25))))
#This is out of 31+31+28 (90) days in Dec-Feb.
summary(t40) #For 40C: Gives min (0) and max (89)  89/90 #0.99
hist(t40)
summary(t44) #For 44C: Gives min (0) and max (61)  61/90 #0.68
hist(t44)

#####################
#End II. Calculate the number of days that exceed threshold values for zebra finches
#####################

#####################
#III. Calculate the proportion of range that experiences temps at or above threshold during the summer
#####################
 #40C
length(t40[t40>0]) / (length(t40[t40>0]) + length(t40[t40==0])) #0.4535629
 #44C
length(t44[t44>0]) / (length(t44[t44>0]) + length(t44[t44==0])) #0.3399758
#####################
#End III. Calculate the proportion of range that experiences temps at or above threshold during the summer
#####################

#####################
#IV. Plot maps of days equal to or above 40C and 44C
#####################

#Clip climate data to ZB range

#Import ZB range (Using shp file extracted from BirdLife Int. database)
setwd("~/BirdLifeInternational/Range maps/BOTW/Shapefiles/Australian zebra finch range")
getwd()
ogrListLayers("auszbrange.shp")
ZBrange <- readOGR(dsn="auszbrange.shp", layer="auszbrange")
plot(ZBrange, col="white", lwd=0.5, border="dark gray", add=FALSE)

#Mask temp data with ZB range
Tclip40 <- mask(t40, ZBrange)
Tclip44 <- mask(t44, ZBrange)

#Figure 6A:
library(maps)
data(worldMapEnv)
quartz()
par(cex=1.2)
par(cex.axis=1.2)
par(cex.lab=1.2)
map('world', region = c('australia'), add=FALSE, lwd=0.5, col="dark gray")
plot(Tclip40, col=c(rev(heat.colors(25))), add=TRUE)
map('world', region = c('australia'), add=TRUE, lwd=0.5, col="black")
plot(ZBrange, col="transparent", lwd=0.5, border="dark gray", add=TRUE)
axis(1, line=-2)
axis(2, line=-1)
title(ylab="Latitude", outer=TRUE, line=-2)
title(xlab="Longitude", outer=TRUE, line=-3.5)
mtext("Number of days at or above 40ºC", side=4, line=-3, cex=1.2)
#Save as pdf.

#To make map for 44C (Figure 6B), run code above with Tclip44.
#####################
#End IV. Plot maps of days equal to or above 40C and 44C
#####################