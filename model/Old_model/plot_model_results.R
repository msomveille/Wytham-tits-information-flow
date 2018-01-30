
library(spatstat)
library(spdep)
library(maptools)
library(rgdal)
library(ncf)
library(png)
library(raster)
library(sp)
library(sna)
library(mapplots)
library(igraph)
library(lattice)

## Plot the flow of information in wytham woods (i.e. results of the model)

setwd("/Users/mariussomveille/Desktop/Oxford/Project_Ben_Robin/Wytham-tits-information-flow")

## Import and process the data

load("data/movements_data.RData")
loggers_coords <- read.csv("data/Wytham_loggers_coordinates.csv")

wyt <-readOGR("/Users/mariussomveille/Desktop/Oxford/Project_Ben_Robin/Wytham-tits-information-flow/data", "perimeter poly with clearings_region")
poly.sp<-SpatialPolygons(list(wyt@polygons[[1]]))
poly.owin<-as(poly.sp,"owin")


## Compute distance between feeders
feeders.distances <- dist(loggers_coords[,2:3], upper=T, diag=T)




## FIGURE 1 -- phase diagrams for 2 patches plus examples

resultsModel_data <- read.csv("model/Old_model/resultsModel_2patches_SS.csv")

# first part
par(mfrow=c(2,3), mar=c(2.8,3.5,0.5,0.5), mgp=c(1.75,0.5,0))

UP1 = 40
x = resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)]
y = resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)]
z = rep(0, length(x))
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] > 0.33 & resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] < 0.66 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] < 0.01)] <- 1
z[which(resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] >= 0.01)] <- 2
z[which(resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] >= 0.1)] <- 3
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] > 0.66 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] < 0.01)] <- 4
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] < 0.33 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] < 0.01)] <- 5
xx <- seq(1.0, 5, 0.2)
yy <- seq(0, 0.0100, 0.0005)
z = z[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = y[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
x = x[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = c(rep(0,21), y)
x = c(xx, x)
z = c(rep(3,21), z)
rbPal <- colorRampPalette(c("grey", "green", "dark green", "orange", "blue"))
datcol <- rbPal(5)[as.numeric(cut(z, breaks=c(0.9,1.9,2.9,3.9,4.9,5.9)))]
plot(x, y, col=datcol, pch=15, ylim=c(0,0.01), xlim=c(1,5), xlab="", ylab=substitute(paste("Movement rate (", italic(m), ")")), cex.lab=1.3, cex=1.4)

#mtext(substitute(paste("Intermediate learning rate (", italic(α), " = 0.005)")), side=3, line=0.3, at=3, cex=0.7)

x = resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)]
y = resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)]
z = rep(0, length(x))
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)] > 0.33 & resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)] < 0.66 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)] < 0.01)] <- 1
z[which(resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)] >= 0.01)] <- 2
z[which(resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)] >= 0.1)] <- 3
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] > 0.66 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)] < 0.01)] <- 4
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] < 0.33 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)] < 0.01)] <- 5
xx <- seq(1.0, 5, 0.2)
yy <- seq(0, 0.0100, 0.0005)
z = z[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = y[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
x = x[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = c(rep(0,21), y)
x = c(xx, x)
z = c(rep(3,21), z)
rbPal <- colorRampPalette(c("grey", "green", "dark green", "orange", "blue"))
datcol <- rbPal(5)[as.numeric(cut(z, breaks=c(0.9,1.9,2.9,3.9,4.9,5.9)))]
plot(x, y, col=datcol, pch=15, ylim=c(0,0.01), xlim=c(1,5), xlab="", ylab="", cex.lab=1.3, cex=1.4)

#mtext(substitute(paste("Fast learning rate (", italic(α), " = 0.01)")), side=3, line=0.3, at=3, cex=0.8)

plot(c(1,1), c(1,2), cex=c(6,4)*1.2, xlab="", ylab="", xlim=c(0.97,1.03), ylim=c(0.5,2.5), col=c("blue","orange"), axes=F)
arrows(1, 1.31, 1, 1.79, length=0.05, col="dark grey", lwd=1.3, code=3)
mtext(expression("P"[1]*": 40 inds"), side=3, line=-3.4, at=0.97, cex=0.8)
mtext(expression("P"[2]*": 60 inds"), side=1, line=-2.9, at=0.97, cex=0.8)



UP1 = 49
x = resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)]
y = resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)]
z = rep(0, length(x))
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] > 0.33 & resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] < 0.66 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] < 0.01)] <- 1
z[which(resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] >= 0.01)] <- 2
z[which(resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] >= 0.1)] <- 3
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] > 0.66 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] < 0.01)] <- 4
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] < 0.33 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] < 0.01)] <- 5
xx <- seq(1.0, 5, 0.2)
yy <- seq(0, 0.0100, 0.0005)
z = z[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = y[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
x = x[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = c(rep(0,21), y)
x = c(xx, x)
z = c(rep(3,21), z)
rbPal <- colorRampPalette(c("grey", "green", "dark green", "orange", "blue"))
datcol <- rbPal(5)[as.numeric(cut(z, breaks=c(0.9,1.9,2.9,3.9,4.9,5.9)))]
plot(x, y, col=datcol, pch=15, ylim=c(0,0.01), xlim=c(1,5), xlab=substitute(paste("Conformity strength (", italic(λ), ")")), ylab=substitute(paste("Movement rate (", italic(m), ")")), cex.lab=1.3, cex=1.4)

#mtext(substitute(paste("Intermediate learning rate (", italic(α), " = 0.005)")), side=3, line=0.3, at=3, cex=0.7)

x = resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)]
y = resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)]
z = rep(0, length(x))
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)] > 0.33 & resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)] < 0.66 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)] < 0.01)] <- 1
z[which(resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)] >= 0.01)] <- 2
z[which(resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)] >= 0.1)] <- 3
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] > 0.66 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)] < 0.01)] <- 4
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.005)] < 0.33 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)] < 0.01)] <- 5
xx <- seq(1.0, 5, 0.2)
yy <- seq(0, 0.0100, 0.0005)
z = z[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = y[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
x = x[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = c(rep(0,21), y)
x = c(xx, x)
z = c(rep(3,21), z)
rbPal <- colorRampPalette(c("grey", "green", "dark green", "orange", "blue"))
datcol <- rbPal(5)[as.numeric(cut(z, breaks=c(0.9,1.9,2.9,3.9,4.9,5.9)))]
plot(x, y, col=datcol, pch=15, ylim=c(0,0.01), xlim=c(1,5), xlab=substitute(paste("Conformity strength (", italic(λ), ")")), ylab="", cex.lab=1.3, cex=1.4)

#mtext(substitute(paste("Fast learning rate (", italic(α), " = 0.01)")), side=3, line=0.3, at=3, cex=0.8)

plot(c(1,1), c(1,2), cex=c(5.1,4.9)*1.2, xlab="", ylab="", xlim=c(0.97,1.03), ylim=c(0.5,2.5), col=c("blue","orange"), axes=F)
arrows(1, 1.27, 1, 1.74, length=0.05, col="dark grey", lwd=1.3, code=3)
mtext(expression("P"[1]*": 49 inds"), side=3, line=-3.4, at=0.97, cex=0.8)
mtext(expression("P"[2]*": 51 inds"), side=1, line=-2.9, at=0.97, cex=0.8)




# Second parts with examples
#par(mfrow=c(2,3), mar=c(2.5,1,0.5,0.1), mgp=c(1.5,0.5,0))

par(mfrow=c(2,1), mar=c(2.5,3,0.5,0.4), mgp=c(1.5,0.5,0))
resultsModel <- read.csv("model/Old_model/resultsModel_2patches1.csv", header =F)
plot(1:151, resultsModel[,1], col="black", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,3]+resultsModel[1,5],resultsModel[1,2]+resultsModel[1,4]+resultsModel[1,6])), xlab="", ylab="Number of individuals", axes=F, cex.lab=1.2, lwd=1.5)
axis(side=1)
axis(side=2)
points(1:151, resultsModel[,3], col="orange", type="l", lwd=1.5)
points(1:151, resultsModel[,5], col="blue", type="l", lwd=1.5)
mtext(expression("P"[1]*""), side=3, at=75, cex=1.2, line=-0.75)

plot(1:151, resultsModel[,1], col="black", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,3]+resultsModel[1,5],resultsModel[1,2]+resultsModel[1,4]+resultsModel[1,6])), xlab="Time (days)", ylab="Number of individuals", axes=F, cex.lab=1.2, lwd=1.5)
axis(side=1)
axis(side=2)
points(1:151, resultsModel[,4], col="orange", type="l", lwd=1.5)
points(1:151, resultsModel[,6], col="blue", type="l", lwd=1.5)
mtext(expression("P"[2]*""), side=3, at=75, cex=1.2, line=-0.75)


par(mfrow=c(2,1), mar=c(2.5,3,0.5,0.4), mgp=c(1.5,0.5,0))
resultsModel <- read.csv("model/Old_model/resultsModel_2patches2.csv", header =F)
plot(1:151, resultsModel[,1], col="black", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,3]+resultsModel[1,5],resultsModel[1,2]+resultsModel[1,4]+resultsModel[1,6])), xlab="", ylab="Number of individuals", axes=F, cex.lab=1.2, lwd=1.5)
axis(side=1)
axis(side=2)
points(1:151, resultsModel[,3], col="orange", type="l", lwd=1.5)
points(1:151, resultsModel[,5], col="blue", type="l", lwd=1.5)
mtext(expression("P"[1]*""), side=3, at=75, cex=1.2, line=-0.75)

plot(1:151, resultsModel[,1], col="black", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,3]+resultsModel[1,5],resultsModel[1,2]+resultsModel[1,4]+resultsModel[1,6])), xlab="Time (days)", ylab="Number of individuals", axes=F, cex.lab=1.2, lwd=1.5)
axis(side=1)
axis(side=2)
points(1:151, resultsModel[,4], col="orange", type="l", lwd=1.5)
points(1:151, resultsModel[,6], col="blue", type="l", lwd=1.5)
mtext(expression("P"[2]*""), side=3, at=75, cex=1.2, line=-0.25)


par(mfrow=c(2,1), mar=c(2.5,3,0.5,0.4), mgp=c(1.5,0.5,0))
resultsModel <- read.csv("model/Old_model/resultsModel_2patches3.csv", header =F)
plot(1:151, resultsModel[,1], col="black", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,3]+resultsModel[1,5],resultsModel[1,2]+resultsModel[1,4]+resultsModel[1,6])), xlab="", ylab="Number of individuals", axes=F, cex.lab=1.2, lwd=1.5)
axis(side=1)
axis(side=2)
points(1:151, resultsModel[,3], col="orange", type="l", lwd=1.5)
points(1:151, resultsModel[,5], col="blue", type="l", lwd=1.5)
mtext(expression("P"[1]*""), side=3, at=75, cex=1.2, line=-0.75)

plot(1:151, resultsModel[,1], col="black", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,3]+resultsModel[1,5],resultsModel[1,2]+resultsModel[1,4]+resultsModel[1,6])), xlab="Time (days)", ylab="Number of individuals", axes=F, cex.lab=1.2, lwd=1.5)
axis(side=1)
axis(side=2)
points(1:151, resultsModel[,4], col="orange", type="l", lwd=1.5)
points(1:151, resultsModel[,6], col="blue", type="l", lwd=1.5)
mtext(expression("P"[2]*""), side=3, at=75, cex=1.2, line=-0.75)







## FIGURE 2 -- phase diagrams for 3 patches plus examples

resultsModel_data <- read.csv("model/Old_model/resultsModel_3patches_SS.csv")

# first part
par(mfrow=c(2,4), mar=c(2.8,3,0.5,0.1), mgp=c(1.75,0.5,0))

# 55/45  & dist = 1.5

plot(c(1,1,2.414214), c(1,2,1.5), cex=c(5,5.5,4.5)*1.2, xlab="", ylab="", xlim=c(0.5,3.5), ylim=c(0.5,2.5), col=c("black","orange","blue"), axes=F)
arrows(1, 1.26, 1, 1.74, length=0.05, col="dark grey", lwd=1.3, code=3)
arrows(1.3, 1.1, 2.15, 1.4, length=0.05, col="dark grey", lwd=1.3, code=3)
arrows(1.32, 1.88, 2.15, 1.6, length=0.05, col="dark grey", lwd=1.3, code=3)
mtext("1", side=1, line=-5.8, at=0.85, cex=0.75, col="dark grey")
mtext("1.5", side=1, line=-3.9, at=1.8, cex=0.75, col="dark grey")
mtext("1.5", side=1, line=-6.4, at=1.6, cex=0.75, col="dark grey")
mtext(expression("P"[1]*": 55 inds"), side=3, line=-1.5, at=1, cex=0.75)
mtext(expression("P"[2]*": 45 inds"), side=3, line=-4, at=2.45, cex=0.75)
mtext(expression("P"[3]*": 50 inds"), side=1, line=-1, at=1, cex=0.75)

UP1 = 55
UP2 = 45
distt = 1.5
x = resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)]
y = resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)]
z = rep(0, length(x))
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] > 0.33 & resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] < 0.66 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)] < 0.01)] <- 1
z[which(resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] >= 0.01)] <- 2
z[which(resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] >= 0.1)] <- 3
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] > 0.66 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] < 0.01)] <- 4
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] < 0.33 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] < 0.01)] <- 5
xx <- seq(1.0, 5, 0.2)
yy <- seq(0, 0.0100, 0.0005)
z = z[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = y[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
x = x[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = c(rep(0,21), y)
x = c(xx, x)
z = c(rep(3,21), z)
rbPal <- colorRampPalette(c("grey", "green", "dark green", "orange", "blue"))
datcol <- rbPal(5)[as.numeric(cut(z, breaks=c(0.9,1.9,2.9,3.9,4.9,5.9)))]
plot(x, y, col=datcol, pch=15, ylim=c(0,0.01), xlim=c(1,5), xlab="", ylab=substitute(paste("Movement rate (", italic(m), ")")), cex.lab=1.3, cex=1.4)


# 55/45  &  dist = 5

UP1 = 55
UP2 = 45
distt = 5
x = resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)]
y = resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)]
z = rep(0, length(x))
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] > 0.33 & resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] < 0.66 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)] < 0.01)] <- 1
z[which(resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] >= 0.01)] <- 2
z[which(resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] >= 0.1)] <- 3
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] > 0.66 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] < 0.01)] <- 4
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] < 0.33 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] < 0.01)] <- 5
xx <- seq(1.0, 5, 0.2)
yy <- seq(0, 0.0100, 0.0005)
z = z[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = y[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
x = x[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = c(rep(0,21), y)
x = c(xx, x)
z = c(rep(3,21), z)
rbPal <- colorRampPalette(c("grey", "green", "dark green", "orange", "blue"))
datcol <- rbPal(5)[as.numeric(cut(z, breaks=c(0.9,1.9,2.9,3.9,4.9,5.9)))]
plot(x, y, col=datcol, pch=15, ylim=c(0,0.01), xlim=c(1,5), xlab="", ylab="", cex.lab=1.3, cex=1.4)

plot(c(1,1,5.974937), c(1,2,1.5), cex=c(5,5.5,4.5)*1.2, xlab="", ylab="", xlim=c(0.44,7.14), ylim=c(0.5,2.5), col=c("black","orange","blue"), axes=F)
arrows(1, 1.26, 1, 1.74, length=0.05, col="dark grey", lwd=1.3, code=3)
arrows(1.8, 1.06, 5.4, 1.4, length=0.05, col="dark grey", lwd=1.3, code=3)
arrows(1.85, 1.94, 5.4, 1.6, length=0.05, col="dark grey", lwd=1.3, code=3)
mtext("1", side=1, line=-5.8, at=0.6, cex=0.75, col="dark grey")
mtext("5", side=1, line=-3.9, at=3.5, cex=0.75, col="dark grey")
mtext("5", side=1, line=-6.4, at=3.5, cex=0.75, col="dark grey")
mtext(expression("P"[1]*": 55 inds"), side=3, line=-1.5, at=1, cex=0.75)
mtext(expression("P"[2]*": 45 inds"), side=3, line=-4, at= 5.974937, cex=0.75)
mtext(expression("P"[3]*": 50 inds"), side=1, line=-1, at=1, cex=0.75)


# 45/55  & dist = 1.5

plot(c(1,1,2.414214), c(1,2,1.5), cex=c(5,4.5,5.5)*1.2, xlab="", ylab="", xlim=c(0.5,3.5), ylim=c(0.5,2.5), col=c("black","orange","blue"), axes=F)
arrows(1, 1.26, 1, 1.77, length=0.05, col="dark grey", lwd=1.3, code=3)
arrows(1.3, 1.1, 2.08, 1.4, length=0.05, col="dark grey", lwd=1.3, code=3)
arrows(1.28, 1.88, 2.08, 1.6, length=0.05, col="dark grey", lwd=1.3, code=3)
mtext("1", side=1, line=-5.8, at=0.85, cex=0.75, col="dark grey")
mtext("1.5", side=1, line=-3.9, at=1.8, cex=0.75, col="dark grey")
mtext("1.5", side=1, line=-6.4, at=1.55, cex=0.75, col="dark grey")
mtext(expression("P"[1]*": 45 inds"), side=3, line=-1.5, at=1, cex=0.75)
mtext(expression("P"[2]*": 55 inds"), side=3, line=-4, at=2.45, cex=0.75)
mtext(expression("P"[3]*": 50 inds"), side=1, line=-1, at=1, cex=0.75)

UP1 = 45
UP2 = 55
distt = 1.5
x = resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)]
y = resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)]
z = rep(0, length(x))
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] > 0.33 & resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] < 0.66 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)] < 0.01)] <- 1
z[which(resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] >= 0.01)] <- 2
z[which(resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] >= 0.1)] <- 3
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] > 0.66 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] < 0.01)] <- 4
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] < 0.33 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] < 0.01)] <- 5
xx <- seq(1.0, 5, 0.2)
yy <- seq(0, 0.0100, 0.0005)
z = z[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = y[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
x = x[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = c(rep(0,21), y)
x = c(xx, x)
z = c(rep(3,21), z)
rbPal <- colorRampPalette(c("grey", "green", "dark green", "orange", "blue"))
datcol <- rbPal(5)[as.numeric(cut(z, breaks=c(0.9,1.9,2.9,3.9,4.9,5.9)))]
plot(x, y, col=datcol, pch=15, ylim=c(0,0.01), xlim=c(1,5), xlab=substitute(paste("Conformity strength (", italic(λ), ")")), ylab=substitute(paste("Movement rate (", italic(m), ")")), cex.lab=1.3, cex=1.4)


# 45/55  & dist = 5

UP1 = 45
UP2 = 55
distt = 5
x = resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)]
y = resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)]
z = rep(0, length(x))
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] > 0.33 & resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] < 0.66 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Alpha == 0.01)] < 0.01)] <- 1
z[which(resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] >= 0.01)] <- 2
z[which(resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] >= 0.1)] <- 3
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] > 0.66 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] < 0.01)] <- 4
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] < 0.33 & resultsModel_data$Var[which(resultsModel_data$Patch1_Us == UP1 & resultsModel_data$Patch2_Us == UP2 & resultsModel_data$Distance == distt & resultsModel_data$Alpha == 0.01)] < 0.01)] <- 5
xx <- seq(1.0, 5, 0.2)
yy <- seq(0, 0.0100, 0.0005)
z = z[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = y[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
x = x[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = c(rep(0,21), y)
x = c(xx, x)
z = c(rep(3,21), z)
rbPal <- colorRampPalette(c("grey", "green", "dark green", "orange", "blue"))
datcol <- rbPal(5)[as.numeric(cut(z, breaks=c(0.9,1.9,2.9,3.9,4.9,5.9)))]
plot(x, y, col=datcol, pch=15, ylim=c(0,0.01), xlim=c(1,5), xlab=substitute(paste("Conformity strength (", italic(λ), ")")), ylab="", cex.lab=1.3, cex=1.4)

plot(c(1,1,5.974937), c(1,2,1.5), cex=c(5,4.5,5.5)*1.2, xlab="", ylab="", xlim=c(0.44,7.14), ylim=c(0.5,2.5), col=c("black","orange","blue"), axes=F)
arrows(1, 1.26, 1, 1.77, length=0.05, col="dark grey", lwd=1.3, code=3)
arrows(1.8, 1.06, 5.2, 1.4, length=0.05, col="dark grey", lwd=1.3, code=3)
arrows(1.77, 1.94, 5.2, 1.6, length=0.05, col="dark grey", lwd=1.3, code=3)
mtext("1", side=1, line=-5.8, at=0.6, cex=0.75, col="dark grey")
mtext("5", side=1, line=-3.9, at=3.5, cex=0.75, col="dark grey")
mtext("5", side=1, line=-6.4, at=3.5, cex=0.75, col="dark grey")
mtext(expression("P"[1]*": 45 inds"), side=3, line=-1.5, at=1, cex=0.75)
mtext(expression("P"[2]*": 55 inds"), side=3, line=-4, at= 5.974937, cex=0.75)
mtext(expression("P"[3]*": 50 inds"), side=1, line=-1, at=1, cex=0.75)




# Second parts with examples
#par(mfrow=c(2,3), mar=c(2.5,2.5,0.5,0.1), mgp=c(1.5,0.5,0))

# Mixture of traditions
par(mfrow=c(3,1), mar=c(2.8,2.8,1,0.3), mgp=c(1.5,0.5,0))
resultsModel <- read.csv("model/Old_model/resultsModel_3patches1.csv", header =F)
plot(1:151, resultsModel[,1], col="black", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,4]+resultsModel[1,7], resultsModel[1,2]+resultsModel[1,5]+resultsModel[1,8], resultsModel[1,3]+resultsModel[1,6]+resultsModel[1,9])), xlab="", ylab="Number of individuals", axes=F, cex.lab=1.3, lwd=1.5)
axis(side=1)
axis(side=2)
points(1:151, resultsModel[,4], col="orange", type="l", lwd=1.5)
points(1:151, resultsModel[,7], col="blue", type="l", lwd=1.5)
mtext(expression("P"[1]*""), side=3, at=75, cex=1.15, line=-0.75)

plot(1:151, resultsModel[,2], col="black", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,4]+resultsModel[1,7], resultsModel[1,2]+resultsModel[1,5]+resultsModel[1,8], resultsModel[1,3]+resultsModel[1,6]+resultsModel[1,9])), xlab="", ylab="Number of individuals", axes=F, cex.lab=1.3, lwd=1.5)
axis(side=1)
axis(side=2)
points(1:151, resultsModel[,5], col="orange", type="l", lwd=1.5)
points(1:151, resultsModel[,8], col="blue", type="l", lwd=1.5)
mtext(expression("P"[2]*""), side=3, at=75, cex=1.15, line=-0.75)

plot(1:151, resultsModel[,3], col="black", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,4]+resultsModel[1,7], resultsModel[1,2]+resultsModel[1,5]+resultsModel[1,8], resultsModel[1,3]+resultsModel[1,6]+resultsModel[1,9])), xlab="Time (days)", ylab="Number of individuals", axes=F, cex.lab=1.3, lwd=1.5)
axis(side=1)
axis(side=2)
points(1:151, resultsModel[,6], col="orange", type="l", lwd=1.5)
points(1:151, resultsModel[,9], col="blue", type="l", lwd=1.5)
mtext(expression("P"[3]*""), side=3, at=75, cex=1.15, line=-0.75)


# Domination of blue
par(mfrow=c(3,1), mar=c(2.8,2.8,1,0.3), mgp=c(1.5,0.5,0))
resultsModel <- read.csv("model/Old_model/resultsModel_3patches2.csv", header =F)
plot(1:151, resultsModel[,1], col="black", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,4]+resultsModel[1,7], resultsModel[1,2]+resultsModel[1,5]+resultsModel[1,8], resultsModel[1,3]+resultsModel[1,6]+resultsModel[1,9])), xlab="", ylab="Number of individuals", axes=F, cex.lab=1.3, lwd=1.5)
axis(side=1)
axis(side=2)
points(1:151, resultsModel[,4], col="orange", type="l", lwd=1.5)
points(1:151, resultsModel[,7], col="blue", type="l", lwd=1.5)
mtext(expression("P"[1]*""), side=3, at=75, cex=1.15, line=-0.75)

plot(1:151, resultsModel[,2], col="black", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,4]+resultsModel[1,7], resultsModel[1,2]+resultsModel[1,5]+resultsModel[1,8], resultsModel[1,3]+resultsModel[1,6]+resultsModel[1,9])), xlab="", ylab="Number of individuals", axes=F, cex.lab=1.3, lwd=1.5)
axis(side=1)
axis(side=2)
points(1:151, resultsModel[,5], col="orange", type="l", lwd=1.5)
points(1:151, resultsModel[,8], col="blue", type="l", lwd=1.5)
mtext(expression("P"[2]*""), side=3, at=75, cex=1.15, line=-0.25)

plot(1:151, resultsModel[,3], col="black", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,4]+resultsModel[1,7], resultsModel[1,2]+resultsModel[1,5]+resultsModel[1,8], resultsModel[1,3]+resultsModel[1,6]+resultsModel[1,9])), xlab="Time (days)", ylab="Number of individuals", axes=F, cex.lab=1.3, lwd=1.5)
axis(side=1)
axis(side=2)
points(1:151, resultsModel[,6], col="orange", type="l", lwd=1.5)
points(1:151, resultsModel[,9], col="blue", type="l", lwd=1.5)
mtext(expression("P"[3]*""), side=3, at=75, cex=1.15, line=-0.75)



# Domination of orange
par(mfrow=c(3,1), mar=c(2.8,2.8,1,0.3), mgp=c(1.5,0.5,0))
resultsModel <- read.csv("model/Old_model/resultsModel_3patches3.csv", header =F)
plot(1:151, resultsModel[,1], col="black", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,4]+resultsModel[1,7], resultsModel[1,2]+resultsModel[1,5]+resultsModel[1,8], resultsModel[1,3]+resultsModel[1,6]+resultsModel[1,9])), xlab="", ylab="Number of individuals", axes=F, cex.lab=1.3, lwd=1.5)
axis(side=1)
axis(side=2)
points(1:151, resultsModel[,4], col="orange", type="l", lwd=1.5)
points(1:151, resultsModel[,7], col="blue", type="l", lwd=1.5)
mtext(expression("P"[1]*""), side=3, at=75, cex=1.15, line=-0.75)

plot(1:151, resultsModel[,2], col="black", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,4]+resultsModel[1,7], resultsModel[1,2]+resultsModel[1,5]+resultsModel[1,8], resultsModel[1,3]+resultsModel[1,6]+resultsModel[1,9])), xlab="", ylab="Number of individuals", axes=F, cex.lab=1.3, lwd=1.5)
axis(side=1)
axis(side=2)
points(1:151, resultsModel[,5], col="orange", type="l", lwd=1.5)
points(1:151, resultsModel[,8], col="blue", type="l", lwd=1.5)
mtext(expression("P"[2]*""), side=3, at=75, cex=1.15, line=-0.25)

plot(1:151, resultsModel[,3], col="black", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,4]+resultsModel[1,7], resultsModel[1,2]+resultsModel[1,5]+resultsModel[1,8], resultsModel[1,3]+resultsModel[1,6]+resultsModel[1,9])), xlab="Time (days)", ylab="Number of individuals", axes=F, cex.lab=1.3, lwd=1.5)
axis(side=1)
axis(side=2)
points(1:151, resultsModel[,6], col="orange", type="l", lwd=1.5)
points(1:151, resultsModel[,9], col="blue", type="l", lwd=1.5)
mtext(expression("P"[3]*""), side=3, at=75, cex=1.15, line=-0.75)



# Local traditions
par(mfrow=c(3,1), mar=c(2.8,2.8,1,0.3), mgp=c(1.5,0.5,0))
resultsModel <- read.csv("model/Old_model/resultsModel_3patches4.csv", header =F)
plot(1:151, resultsModel[,1], col="black", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,4]+resultsModel[1,7], resultsModel[1,2]+resultsModel[1,5]+resultsModel[1,8], resultsModel[1,3]+resultsModel[1,6]+resultsModel[1,9])), xlab="", ylab="Number of individuals", axes=F, cex.lab=1.3, lwd=1.5)
axis(side=1)
axis(side=2)
points(1:151, resultsModel[,4], col="orange", type="l", lwd=1.5)
points(1:151, resultsModel[,7], col="blue", type="l", lwd=1.5)
mtext(expression("P"[1]*""), side=3, at=75, cex=1.15, line=-0.75)

plot(1:151, resultsModel[,2], col="black", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,4]+resultsModel[1,7], resultsModel[1,2]+resultsModel[1,5]+resultsModel[1,8], resultsModel[1,3]+resultsModel[1,6]+resultsModel[1,9])), xlab="", ylab="Number of individuals", axes=F, cex.lab=1.3, lwd=1.5)
axis(side=1)
axis(side=2)
points(1:151, resultsModel[,5], col="orange", type="l", lwd=1.5)
points(1:151, resultsModel[,8], col="blue", type="l", lwd=1.5)
mtext(expression("P"[2]*""), side=3, at=75, cex=1.15, line=-0.75)

plot(1:151, resultsModel[,3], col="black", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,4]+resultsModel[1,7], resultsModel[1,2]+resultsModel[1,5]+resultsModel[1,8], resultsModel[1,3]+resultsModel[1,6]+resultsModel[1,9])), xlab="Time (days)", ylab="Number of individuals", axes=F, cex.lab=1.3, lwd=1.5)
axis(side=1)
axis(side=2)
points(1:151, resultsModel[,6], col="orange", type="l", lwd=1.5)
points(1:151, resultsModel[,9], col="blue", type="l", lwd=1.5)
mtext(expression("P"[3]*""), side=3, at=75, cex=1.15, line=-0.75)






## FIGURE 3 -- phase diagrams for Wytham plus examples

resultsModel_data <- read.csv("model/Old_model/resultsModel_wytham_SS.csv")

# first part
par(mfrow=c(3,1), mar=c(2.8,3,3,0.5), mgp=c(1.75,0.5,0))

# Direct distance 
alph = 0.005
x = resultsModel_data$Beta[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")]
y = resultsModel_data$Lambda[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")]
z = rep(0, length(x))
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")] > 0.33 & resultsModel_data$Total_prop_Ls[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")] < 0.66 & resultsModel_data$Var[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")] < 0.01)] <- 1
z[which(resultsModel_data$Var[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")] >= 0.01)] <- 2
z[which(resultsModel_data$Var[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")] >= 0.1)] <- 3
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")] > 0.66 & resultsModel_data$Var[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")] < 0.01)] <- 4
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")] < 0.33 & resultsModel_data$Var[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")] < 0.01)] <- 5
xx <- seq(1.0, 5, 0.2)
yy <- seq(0, 0.0100, 0.0005)
z = z[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = y[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
x = x[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = c(rep(0,21), y)
x = c(xx, x)
z = c(rep(3,21), z)
rbPal <- colorRampPalette(c("grey", "green", "dark green", "orange", "blue"))
datcol <- rbPal(5)[as.numeric(cut(z, breaks=c(0.9,1.9,2.9,3.9,4.9,5.9)))]
plot(x, y, col=datcol, pch=15, ylim=c(0,0.1), xlim=c(1,5), xlab="", ylab=substitute(paste("Movement rate (", italic(m), ")")), cex.lab=1.3, cex=1.4)

alph = 0.01
x = resultsModel_data$Beta[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")]
y = resultsModel_data$Lambda[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")]
z = rep(0, length(x))
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")] > 0.33 & resultsModel_data$Total_prop_Ls[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")] < 0.66 & resultsModel_data$Var[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")] < 0.01)] <- 1
z[which(resultsModel_data$Var[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")] >= 0.01)] <- 2
z[which(resultsModel_data$Var[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")] >= 0.1)] <- 3
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")] > 0.66 & resultsModel_data$Var[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")] < 0.01)] <- 4
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")] < 0.33 & resultsModel_data$Var[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "directDist")] < 0.01)] <- 5
xx <- seq(1.0, 5, 0.2)
yy <- seq(0, 0.0100, 0.0005)
z = z[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = y[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
x = x[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = c(rep(0,21), y)
x = c(xx, x)
z = c(rep(3,21), z)
rbPal <- colorRampPalette(c("grey", "green", "dark green", "orange", "blue"))
datcol <- rbPal(5)[as.numeric(cut(z, breaks=c(0.9,1.9,2.9,3.9,4.9,5.9)))]
plot(x, y, col=datcol, pch=15, ylim=c(0,0.1), xlim=c(1,5), xlab="", ylab=substitute(paste("Movement rate (", italic(m), ")")), cex.lab=1.3, cex=1.4)


# Forest distance
alph = 0.005
x = resultsModel_data$Beta[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "forestDist")]
y = resultsModel_data$Lambda[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "forestDist")]
z = rep(0, length(x))
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "forestDist")] > 0.33 & resultsModel_data$Total_prop_Ls[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "forestDist")] < 0.66 & resultsModel_data$Var[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "forestDist")] < 0.01)] <- 1
z[which(resultsModel_data$Var[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "forestDist")] >= 0.01)] <- 2
z[which(resultsModel_data$Var[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "forestDist")] >= 0.1)] <- 3
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "forestDist")] > 0.66 & resultsModel_data$Var[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "forestDist")] < 0.01)] <- 4
z[which(resultsModel_data$Total_prop_Ls[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "forestDist")] < 0.33 & resultsModel_data$Var[which(resultsModel_data$Alpha == alph & resultsModel_data$Environment == "forestDist")] < 0.01)] <- 5
xx <- seq(1.0, 5, 0.2)
yy <- seq(0, 0.0100, 0.0005)
z = z[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = y[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
x = x[which(match(as.character(x), as.character(xx), nomatch=0) > 0)]
y = c(rep(0,21), y)
x = c(xx, x)
z = c(rep(3,21), z)
rbPal <- colorRampPalette(c("grey", "green", "dark green", "orange", "blue"))
datcol <- rbPal(5)[as.numeric(cut(z, breaks=c(0.9,1.9,2.9,3.9,4.9,5.9)))]
plot(x, y, col=datcol, pch=15, ylim=c(0,0.1), xlim=c(1,5), xlab=substitute(paste("Conformity strength (", italic(λ), ")")), ylab=substitute(paste("Movement rate (", italic(m), ")")), cex.lab=1.3, cex=1.4)



# Second parts with examples

par(mfrow=c(3,3), mar=c(2.5,2.75,0.3,0.3), mgp=c(1.5,0.5,0))

# Mixture of traditions
resultsModel <- read.csv("model/Old_model/resultsModel_wytham1.csv")
resultsModel <- abs(resultsModel)
## Plot the map after 20 days
time.point <- 20
model_res <- cbind(as.matrix(resultsModel)[time.point,1:65], as.matrix(resultsModel)[time.point,66:130], as.matrix(resultsModel)[time.point,131:195])
model_res_list <- as.list(as.data.frame(t(model_res)))
plot(poly.owin, main="")
for(i in 1:60){
	add.pie(z=model_res_list[[i]], x=loggers_coords[i,"x"], y=loggers_coords[i,"y"], labels="", radius=loggers[i,5]*2, col=c("grey", "orange", "blue"))
}
## Plot the map after 150 days
final.time.point <- 150
model_res <- cbind(as.matrix(resultsModel)[final.time.point,1:65], as.matrix(resultsModel)[final.time.point,66:130], as.matrix(resultsModel)[final.time.point,131:195])
model_res_list <- as.list(as.data.frame(t(model_res)))
plot(poly.owin, main="")
for(i in 1:60){
	add.pie(z=model_res_list[[i]], x=loggers_coords[i,"x"], y=loggers_coords[i,"y"], labels="", radius=loggers[i,5]*2, col=c("grey", "orange", "blue"))
}
## Plot the proportion of lefts among solvers over time
model_res <- list()
prop <- matrix(nrow=final.time.point, ncol=65)
propL <- matrix(nrow=final.time.point, ncol=65)
for(i in 1:final.time.point){
	model_res[[i]] <- cbind(as.matrix(resultsModel)[i,1:65], as.matrix(resultsModel)[i,66:130], as.matrix(resultsModel)[i,131:195])
	prop[i,] <- apply(model_res[[i]], 1, function(x) (x[2]+x[3])/sum(x))
	propL[i,] <- apply(model_res[[i]], 1, function(x) x[2]/(x[2]+x[3]))
}
plot(1:final.time.point, propL[,1], col="dark grey", pch=3, xlab="", ylab=expression("Prevalence of behaviour s"[1]*""), ylim=c(0,1), type="l", axes=F, cex.lab=1.4, lwd=1.2)
for(i in 2:65){
	points(1:final.time.point, propL[,i], col="dark grey", pch=3, ylim=c(0,1), type="l", lwd=1.2)
}
axis(side=1)
axis(side=2)


# Behaviour s1 dominates
resultsModel <- read.csv("model/Old_model/resultsModel_wytham2.csv")
resultsModel <- abs(resultsModel)
## Plot the map after 20 days
time.point <- 20
model_res <- cbind(as.matrix(resultsModel)[time.point,1:65], as.matrix(resultsModel)[time.point,66:130], as.matrix(resultsModel)[time.point,131:195])
model_res_list <- as.list(as.data.frame(t(model_res)))
plot(poly.owin, main="")
for(i in 1:60){
	add.pie(z=model_res_list[[i]], x=loggers_coords[i,"x"], y=loggers_coords[i,"y"], labels="", radius=loggers[i,5]*2, col=c("grey", "orange", "blue"))
}
## Plot the map after 150 days
final.time.point <- 150
model_res <- cbind(as.matrix(resultsModel)[final.time.point,1:65], as.matrix(resultsModel)[final.time.point,66:130], as.matrix(resultsModel)[final.time.point,131:195])
model_res_list <- as.list(as.data.frame(t(model_res)))
plot(poly.owin, main="")
for(i in 1:60){
	add.pie(z=model_res_list[[i]], x=loggers_coords[i,"x"], y=loggers_coords[i,"y"], labels="", radius=loggers[i,5]*2, col=c("grey", "orange", "blue"))
}
## Plot the proportion of lefts among solvers over time
model_res <- list()
prop <- matrix(nrow=final.time.point, ncol=65)
propL <- matrix(nrow=final.time.point, ncol=65)
for(i in 1:final.time.point){
	model_res[[i]] <- cbind(as.matrix(resultsModel)[i,1:65], as.matrix(resultsModel)[i,66:130], as.matrix(resultsModel)[i,131:195])
	prop[i,] <- apply(model_res[[i]], 1, function(x) (x[2]+x[3])/sum(x))
	propL[i,] <- apply(model_res[[i]], 1, function(x) x[2]/(x[2]+x[3]))
}
plot(1:final.time.point, propL[,1], col="dark grey", pch=3, xlab="", ylab=expression("Prevalence of behaviour s"[1]*""), ylim=c(0,1), type="l", axes=F, cex.lab=1.4, lwd=1.2)
for(i in 2:65){
	points(1:final.time.point, propL[,i], col="dark grey", pch=3, ylim=c(0,1), type="l", lwd=1.2)
}
axis(side=1)
axis(side=2)


# Local traditions
resultsModel <- read.csv("model/Old_model/resultsModel_wytham3.csv")
resultsModel <- abs(resultsModel)
## Plot the map after 20 days
time.point <- 20
model_res <- cbind(as.matrix(resultsModel)[time.point,1:65], as.matrix(resultsModel)[time.point,66:130], as.matrix(resultsModel)[time.point,131:195])
model_res_list <- as.list(as.data.frame(t(model_res)))
plot(poly.owin, main="")
for(i in 1:60){
	add.pie(z=model_res_list[[i]], x=loggers_coords[i,"x"], y=loggers_coords[i,"y"], labels="", radius=loggers[i,5]*2, col=c("grey", "orange", "blue"))
}
## Plot the map after 150 days
final.time.point <- 150
model_res <- cbind(as.matrix(resultsModel)[final.time.point,1:65], as.matrix(resultsModel)[final.time.point,66:130], as.matrix(resultsModel)[final.time.point,131:195])
model_res_list <- as.list(as.data.frame(t(model_res)))
plot(poly.owin, main="")
for(i in 1:60){
	add.pie(z=model_res_list[[i]], x=loggers_coords[i,"x"], y=loggers_coords[i,"y"], labels="", radius=loggers[i,5]*2, col=c("grey", "orange", "blue"))
}
## Plot the proportion of lefts among solvers over time
model_res <- list()
prop <- matrix(nrow=final.time.point, ncol=65)
propL <- matrix(nrow=final.time.point, ncol=65)
for(i in 1:final.time.point){
	model_res[[i]] <- cbind(as.matrix(resultsModel)[i,1:65], as.matrix(resultsModel)[i,66:130], as.matrix(resultsModel)[i,131:195])
	prop[i,] <- apply(model_res[[i]], 1, function(x) (x[2]+x[3])/sum(x))
	propL[i,] <- apply(model_res[[i]], 1, function(x) x[2]/(x[2]+x[3]))
}
plot(1:final.time.point, propL[,1], col="dark grey", pch=3, xlab="Time (days)", ylab=expression("Prevalence of behaviour s"[1]*""), ylim=c(0,1), type="l", axes=F, cex.lab=1.4, lwd=1.2)
for(i in 2:65){
	points(1:final.time.point, propL[,i], col="dark grey", pch=3, ylim=c(0,1), type="l", lwd=1.2)
}
axis(side=1)
axis(side=2)




## FIGURE 4 -- randomization analysis Wytham

# conformity = 1
resultsModel_random_forestDist_data_conf1 <- read.csv("model/Old_model/resultsModel_wytham_sumstats_random_conformity1.csv", header=F)
diff_centr_conf1 <- resultsModel_random_forestDist_data_conf1[,6] - resultsModel_random_forestDist_data_conf1[,5]
diff_pools_conf1 <- resultsModel_random_forestDist_data_conf1[,3] - resultsModel_random_forestDist_data_conf1[,4]
length(which(resultsModel_random_forestDist_data_conf1[,1] > 0.01))/100
length(which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] < 0.33))/100
length(which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] > 0.66))/100
length(which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] > 0.33 & resultsModel_random_forestDist_data_conf1[,2] < 0.66))/100

# conformity = 1.2
resultsModel_random_forestDist_data_conf1.2 <- read.csv("model/Old_model/resultsModel_wytham_sumstats_random_conformity1.2.csv", header=F)
diff_centr_conf1.2 <- resultsModel_random_forestDist_data_conf1.2[,6] - resultsModel_random_forestDist_data_conf1.2[,5]
diff_pools_conf1.2 <- resultsModel_random_forestDist_data_conf1.2[,3] - resultsModel_random_forestDist_data_conf1.2[,4]
length(which(resultsModel_random_forestDist_data_conf1.2[,1] > 0.01))/100
length(which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] < 0.33))/100
length(which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] > 0.66))/100
length(which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] > 0.33 & resultsModel_random_forestDist_data_conf1.2[,2] < 0.66))/100

# conformity = 4
resultsModel_random_forestDist_data_conf4 <- read.csv("model/Old_model/resultsModel_wytham_sumstats_random_conformity4.csv", header=F)
diff_centr_conf4 <- resultsModel_random_forestDist_data_conf4[,6] - resultsModel_random_forestDist_data_conf4[,5]
diff_pools_conf4 <- resultsModel_random_forestDist_data_conf4[,3] - resultsModel_random_forestDist_data_conf4[,4]
length(which(resultsModel_random_forestDist_data_conf4[,1] > 0.01))/100
length(which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] < 0.33))/100
length(which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] > 0.66))/100
length(which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] > 0.33 & resultsModel_random_forestDist_data_conf4[,2] < 0.66))/100


par(mfrow=c(3,3), mar=c(2.5,2.5,2.5,0.1), mgp=c(1.5,0.5,0))

plot(resultsModel_random_forestDist_data_conf1[,2], resultsModel_random_forestDist_data_conf1[,1], pch=20, xlim=c(0,1), ylim=c(0,0.12), xlab="", ylab=expression("Variance in prevalence of behaviour s"[1]*""), axes=F, col="green", cex.lab=1.1, main=substitute(paste("No conformity (", italic(λ), " = 1)")))
points(resultsModel_random_forestDist_data_conf1[,2][which(resultsModel_random_forestDist_data_conf1[,1] > 0.1)], resultsModel_random_forestDist_data_conf1[,1][which(resultsModel_random_forestDist_data_conf1[,1] > 0.1)], pch=20, col="dark green")
points(resultsModel_random_forestDist_data_conf1[,2][which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] < 0.33)], resultsModel_random_forestDist_data_conf1[,1][which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] < 0.33)], pch=20, col="blue")
points(resultsModel_random_forestDist_data_conf1[,2][which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] > 0.33)], resultsModel_random_forestDist_data_conf1[,1][which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] > 0.33)], pch=20, col="orange")
points(resultsModel_random_forestDist_data_conf1[,2][which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] > 0.33 & resultsModel_random_forestDist_data_conf1[,2] < 0.66)], resultsModel_random_forestDist_data_conf1[,1][which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] > 0.33 & resultsModel_random_forestDist_data_conf1[,2] < 0.66)], pch=20, col="dark grey")
abline(h=0.01)
axis(side=1)
axis(side=2)

plot(resultsModel_random_forestDist_data_conf1.2[,2], resultsModel_random_forestDist_data_conf1.2[,1], pch=20, xlim=c(0,1), ylim=c(0,0.15), xlab="", ylab="", axes=F, col="green", cex.lab=1.1, main=substitute(paste("Weak conformity (", italic(λ), " = 1.2)")))
points(resultsModel_random_forestDist_data_conf1.2[,2][which(resultsModel_random_forestDist_data_conf1.2[,1] > 0.1)], resultsModel_random_forestDist_data_conf1.2[,1][which(resultsModel_random_forestDist_data_conf1.2[,1] > 0.1)], pch=20, col="dark green")
points(resultsModel_random_forestDist_data_conf1.2[,2][which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] < 0.33)], resultsModel_random_forestDist_data_conf1.2[,1][which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] < 0.33)], pch=20, col="blue")
points(resultsModel_random_forestDist_data_conf1.2[,2][which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] > 0.33)], resultsModel_random_forestDist_data_conf1.2[,1][which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] > 0.33)], pch=20, col="orange")
points(resultsModel_random_forestDist_data_conf1.2[,2][which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] > 0.33 & resultsModel_random_forestDist_data_conf1.2[,2] < 0.66)], resultsModel_random_forestDist_data_conf1.2[,1][which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] > 0.33 & resultsModel_random_forestDist_data_conf1.2[,2] < 0.66)], pch=20, col="dark grey")
abline(h=0.01)
axis(side=1)
axis(side=2)

plot(resultsModel_random_forestDist_data_conf4[,2], resultsModel_random_forestDist_data_conf4[,1], pch=20, xlim=c(0,1), ylim=c(0,0.2), xlab="", ylab="", axes=F, col="green", cex.lab=1.1, main=substitute(paste("Strong conformity (", italic(λ), " = 4)")))
points(resultsModel_random_forestDist_data_conf4[,2][which(resultsModel_random_forestDist_data_conf4[,1] > 0.1)], resultsModel_random_forestDist_data_conf4[,1][which(resultsModel_random_forestDist_data_conf4[,1] > 0.1)], pch=20, col="dark green")
points(resultsModel_random_forestDist_data_conf4[,2][which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] < 0.33)], resultsModel_random_forestDist_data_conf4[,1][which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] < 0.33)], pch=20, col="blue")
points(resultsModel_random_forestDist_data_conf4[,2][which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] > 0.33)], resultsModel_random_forestDist_data_conf4[,1][which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] > 0.33)], pch=20, col="orange")
points(resultsModel_random_forestDist_data_conf4[,2][which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] > 0.33 & resultsModel_random_forestDist_data_conf4[,2] < 0.66)], resultsModel_random_forestDist_data_conf4[,1][which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] > 0.33 & resultsModel_random_forestDist_data_conf4[,2] < 0.66)], pch=20, col="dark grey")
abline(h=0.01)
axis(side=1)
axis(side=2)



plot(resultsModel_random_forestDist_data_conf1[,2], diff_pools_conf1, pch=20, ylab="Difference in pools of naives", xlab="", axes=F, col="green", cex.lab=1.1, xlim=c(0,1))
points(resultsModel_random_forestDist_data_conf1[,2][which(resultsModel_random_forestDist_data_conf1[,1] > 0.1)], diff_pools_conf1[which(resultsModel_random_forestDist_data_conf1[,1] > 0.1)], pch=20, col="dark green")
points(resultsModel_random_forestDist_data_conf1[,2][which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] < 0.33)], diff_pools_conf1[which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] < 0.33)], pch=20, col="blue")
points(resultsModel_random_forestDist_data_conf1[,2][which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] > 0.33)], diff_pools_conf1[which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] > 0.33)], pch=20, col="orange")
points(resultsModel_random_forestDist_data_conf1[,2][which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] > 0.33 & resultsModel_random_forestDist_data_conf1[,2] < 0.66)], diff_pools_conf1[which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] > 0.33 & resultsModel_random_forestDist_data_conf1[,2] < 0.66)], pch=20, col="dark grey")
axis(side=1)
axis(side=2)

plot(resultsModel_random_forestDist_data_conf1.2[,2], diff_pools_conf1.2, pch=20, xlab="", ylab="", axes=F, col="green", cex.lab=1.1, xlim=c(0,1))
points(resultsModel_random_forestDist_data_conf1.2[,2][which(resultsModel_random_forestDist_data_conf1.2[,1] > 0.1)], diff_pools_conf1.2[which(resultsModel_random_forestDist_data_conf1.2[,1] > 0.1)], pch=20, col="dark green")
points(resultsModel_random_forestDist_data_conf1.2[,2][which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] < 0.33)], diff_pools_conf1.2[which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] < 0.33)], pch=20, col="blue")
points(resultsModel_random_forestDist_data_conf1.2[,2][which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] > 0.33)], diff_pools_conf1.2[which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] > 0.33)], pch=20, col="orange")
points(resultsModel_random_forestDist_data_conf1.2[,2][which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] > 0.33 & resultsModel_random_forestDist_data_conf1.2[,2] < 0.66)], diff_pools_conf1.2[which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] > 0.33 & resultsModel_random_forestDist_data_conf1.2[,2] < 0.66)], pch=20, col="dark grey")
axis(side=1)
axis(side=2)

plot(resultsModel_random_forestDist_data_conf4[,2], diff_pools_conf4, pch=20, xlab="", ylab="", axes=F, col="green", cex.lab=1.1, xlim=c(0,1))
points(resultsModel_random_forestDist_data_conf4[,2][which(resultsModel_random_forestDist_data_conf4[,1] > 0.1)], diff_pools_conf4[which(resultsModel_random_forestDist_data_conf4[,1] > 0.1)], pch=20, col="dark green")
points(resultsModel_random_forestDist_data_conf4[,2][which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] < 0.33)], diff_pools_conf4[which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] < 0.33)], pch=20, col="blue")
points(resultsModel_random_forestDist_data_conf4[,2][which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] > 0.33)], diff_pools_conf4[which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] > 0.33)], pch=20, col="orange")
points(resultsModel_random_forestDist_data_conf4[,2][which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] > 0.33 & resultsModel_random_forestDist_data_conf4[,2] < 0.66)], diff_pools_conf4[which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] > 0.33 & resultsModel_random_forestDist_data_conf4[,2] < 0.66)], pch=20, col="dark grey")
axis(side=1)
axis(side=2)


plot(resultsModel_random_forestDist_data_conf1[,2], diff_centr_conf1, pch=20, ylab="Difference in centrality of initial patches", xlab=expression("Total prevalence of behaviour s"[1]*""), axes=F, col="green", cex.lab=1.1, xlim=c(0,1))
points(resultsModel_random_forestDist_data_conf1[,2][which(resultsModel_random_forestDist_data_conf1[,1] > 0.1)], diff_centr_conf1[which(resultsModel_random_forestDist_data_conf1[,1] > 0.1)], pch=20, col="dark green")
points(resultsModel_random_forestDist_data_conf1[,2][which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] < 0.33)], diff_centr_conf1[which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] < 0.33)], pch=20, col="blue")
points(resultsModel_random_forestDist_data_conf1[,2][which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] > 0.33)], diff_centr_conf1[which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] > 0.33)], pch=20, col="orange")
points(resultsModel_random_forestDist_data_conf1[,2][which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] > 0.33 & resultsModel_random_forestDist_data_conf1[,2] < 0.66)], diff_centr_conf1[which(resultsModel_random_forestDist_data_conf1[,1] < 0.01 & resultsModel_random_forestDist_data_conf1[,2] > 0.33 & resultsModel_random_forestDist_data_conf1[,2] < 0.66)], pch=20, col="dark grey")
axis(side=1)
axis(side=2)

plot(resultsModel_random_forestDist_data_conf1.2[,2], diff_centr_conf1.2, pch=20, xlab=expression("Total prevalence of behaviour s"[1]*""), ylab="", axes=F, col="green", cex.lab=1.1, xlim=c(0,1))
points(resultsModel_random_forestDist_data_conf1.2[,2][which(resultsModel_random_forestDist_data_conf1.2[,1] > 0.1)], diff_centr_conf1.2[which(resultsModel_random_forestDist_data_conf1.2[,1] > 0.1)], pch=20, col="dark green")
points(resultsModel_random_forestDist_data_conf1.2[,2][which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] < 0.33)], diff_centr_conf1.2[which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] < 0.33)], pch=20, col="blue")
points(resultsModel_random_forestDist_data_conf1.2[,2][which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] > 0.33)], diff_centr_conf1.2[which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] > 0.33)], pch=20, col="orange")
points(resultsModel_random_forestDist_data_conf1.2[,2][which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] > 0.33 & resultsModel_random_forestDist_data_conf1.2[,2] < 0.66)], diff_centr_conf1.2[which(resultsModel_random_forestDist_data_conf1.2[,1] < 0.01 & resultsModel_random_forestDist_data_conf1.2[,2] > 0.33 & resultsModel_random_forestDist_data_conf1.2[,2] < 0.66)], pch=20, col="dark grey")
axis(side=1)
axis(side=2)

plot(resultsModel_random_forestDist_data_conf4[,2], diff_centr_conf4, pch=20, xlab=expression("Total prevalence of behaviour s"[1]*""), ylab="", axes=F, col="green", cex.lab=1.1, xlim=c(0,1))
points(resultsModel_random_forestDist_data_conf4[,2][which(resultsModel_random_forestDist_data_conf4[,1] > 0.1)], diff_centr_conf4[which(resultsModel_random_forestDist_data_conf4[,1] > 0.1)], pch=20, col="dark green")
points(resultsModel_random_forestDist_data_conf4[,2][which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] < 0.33)], diff_centr_conf4[which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] < 0.33)], pch=20, col="blue")
points(resultsModel_random_forestDist_data_conf4[,2][which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] > 0.33)], diff_centr_conf4[which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] > 0.33)], pch=20, col="orange")
points(resultsModel_random_forestDist_data_conf4[,2][which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] > 0.33 & resultsModel_random_forestDist_data_conf4[,2] < 0.66)], diff_centr_conf4[which(resultsModel_random_forestDist_data_conf4[,1] < 0.01 & resultsModel_random_forestDist_data_conf4[,2] > 0.33 & resultsModel_random_forestDist_data_conf4[,2] < 0.66)], pch=20, col="dark grey")
axis(side=1)
axis(side=2)


























resultsModel_random_forestDist_data <- read.csv("model/Old_model/resultsModel_wytham_sumstats_random_forest.csv")

# beta = 1
length(which(resultsModel_random_forestDist_data$Var[which(resultsModel_random_forestDist_data$Beta == 1)] > 0.01))/100
length(which(resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 1 & resultsModel_random_forestDist_data$Var < 0.01)] > 0.66))/100
length(which(resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 1 & resultsModel_random_forestDist_data$Var < 0.01)] < 0.33))/100
length(which(resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 1 & resultsModel_random_forestDist_data$Var < 0.01)] < 0.66 & resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 1 & resultsModel_random_forestDist_data$Var < 0.01)] > 0.33))/100
plot(resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 1)], resultsModel_random_forestDist_data$Var[which(resultsModel_random_forestDist_data$Beta == 1)], pch=20, xlim=c(0,1), ylim=c(0,0.02), xlab="Total prevalence of behaviour s1", ylab="Variance of prevS1 in sub-populations", main="β = 1")
abline(h=0.01)
points(0.5864, 3.0347e-04, col="green", pch=20, cex=2)













## PLOT RESULTS

par(mfrow=c(1,4), mar=c(2.5,2.5,1.5,1.5), mgp=c(1.5,0.5,0))

resultsModel <- read.csv("model/Old_model/resultsModel_wytham.csv")
resultsModel <- abs(resultsModel)

## Plot the map after 20 days
time.point <- 20
model_res <- cbind(as.matrix(resultsModel)[time.point,1:65], as.matrix(resultsModel)[time.point,66:130], as.matrix(resultsModel)[time.point,131:195])
model_res_list <- as.list(as.data.frame(t(model_res)))
plot(poly.owin, main="After 20 days")
for(i in 1:60){
	add.pie(z=model_res_list[[i]], x=loggers_coords[i,"x"], y=loggers_coords[i,"y"], labels="", radius=loggers[i,5]*2, col=c("light grey", "red", "blue"))
}


final.time.point <- 150

## Plot the map after 150 days
model_res <- cbind(as.matrix(resultsModel)[final.time.point,1:65], as.matrix(resultsModel)[final.time.point,66:130], as.matrix(resultsModel)[final.time.point,131:195])
model_res_list <- as.list(as.data.frame(t(model_res)))
plot(poly.owin, main="After 150 days")
for(i in 1:60){
	add.pie(z=model_res_list[[i]], x=loggers_coords[i,"x"], y=loggers_coords[i,"y"], labels="", radius=loggers[i,5]*2, col=c("light grey", "red", "blue"))
}

## Plot the proportion of solvers over time
model_res <- list()
prop <- matrix(nrow=final.time.point, ncol=65)
propL <- matrix(nrow=final.time.point, ncol=65)
for(i in 1:final.time.point){
	model_res[[i]] <- cbind(as.matrix(resultsModel)[i,1:65], as.matrix(resultsModel)[i,66:130], as.matrix(resultsModel)[i,131:195])
	prop[i,] <- apply(model_res[[i]], 1, function(x) (x[2]+x[3])/sum(x))
	propL[i,] <- apply(model_res[[i]], 1, function(x) x[2]/(x[2]+x[3]))
}
plot(1:final.time.point, prop[,11], col="red", pch=3, xlab="Time (1–150 days)", ylab="Proportion of solvers in sub-population", ylim=c(0,1), type="l", cex.lab=1.1)
points(1:final.time.point, prop[,50], col="red", pch=3, ylim=c(0,1), type="l")
points(1:final.time.point, prop[,59], col="red", pch=3, ylim=c(0,1), type="l")
points(1:final.time.point, prop[,3], col="blue", pch=3, ylim=c(0,1), type="l")
points(1:final.time.point, prop[,7], col="blue", pch=3, ylim=c(0,1), type="l")
points(1:final.time.point, prop[,34], col="green", pch=3, ylim=c(0,1), type="l")
points(1:final.time.point, prop[,46], col="green", pch=3, ylim=c(0,1), type="l")
points(1:final.time.point, prop[,36], col="green", pch=3, ylim=c(0,1), type="l")
abline(v=20)


## Plot the proportion of lefts among solvers over time
plot(1:final.time.point, propL[,1], col="grey", pch=3, xlab="Time (1–150 days)", ylab="Prevalence of s1 among solvers (prevS1)", ylim=c(0,1), type="l", cex.lab=1.1)
for(i in 2:65){
	points(1:final.time.point, propL[,i], col="grey", pch=3, ylim=c(0,1), type="l")
}






## PLOT EXAMPLES FOR RANDOMISED IC

par(mfrow=c(1,3), mar=c(2.5,2.5,1.5,1.5), mgp=c(1.5,0.5,0))

resultsModel <- read.csv("model/Old_model/resultsModel_wytham.csv")
resultsModel <- abs(resultsModel)

## Plot the map after 20 days
time.point <- 20
model_res <- cbind(as.matrix(resultsModel)[time.point,1:65], as.matrix(resultsModel)[time.point,66:130], as.matrix(resultsModel)[time.point,131:195])
model_res_list <- as.list(as.data.frame(t(model_res)))
plot(poly.owin, main="After 20 days")
for(i in 1:60){
	add.pie(z=model_res_list[[i]], x=loggers_coords[i,"x"], y=loggers_coords[i,"y"], labels="", radius=loggers[i,5]*2, col=c("light grey", "red", "blue"))
}

IC = c(21,12,16,37,29)
points(x=loggers_coords$x[IC[1]], y=loggers_coords$y[IC[1]]+120, pch=8, col="red")
points(x=loggers_coords$x[IC[2]], y=loggers_coords$y[IC[2]]+120, pch=8, col="red")
points(x=loggers_coords$x[IC[3]], y=loggers_coords$y[IC[3]]+120, pch=8, col="red")
points(x=loggers_coords$x[IC[4]], y=loggers_coords$y[IC[4]]+120, pch=8, col="blue")
points(x=loggers_coords$x[IC[5]], y=loggers_coords$y[IC[5]]+120, pch=8, col="blue")
final.time.point <- 150

## Plot the map after 150 days
model_res <- cbind(as.matrix(resultsModel)[final.time.point,1:65], as.matrix(resultsModel)[final.time.point,66:130], as.matrix(resultsModel)[final.time.point,131:195])
model_res_list <- as.list(as.data.frame(t(model_res)))
plot(poly.owin, main="After 150 days")
for(i in 1:60){
	add.pie(z=model_res_list[[i]], x=loggers_coords[i,"x"], y=loggers_coords[i,"y"], labels="", radius=loggers[i,5]*2, col=c("light grey", "red", "blue"))
}
## Plot the proportion of lefts among solvers over time
model_res <- list()
prop <- matrix(nrow=final.time.point, ncol=65)
propL <- matrix(nrow=final.time.point, ncol=65)
for(i in 1:final.time.point){
	model_res[[i]] <- cbind(as.matrix(resultsModel)[i,1:65], as.matrix(resultsModel)[i,66:130], as.matrix(resultsModel)[i,131:195])
	prop[i,] <- apply(model_res[[i]], 1, function(x) (x[2]+x[3])/sum(x))
	propL[i,] <- apply(model_res[[i]], 1, function(x) x[2]/(x[2]+x[3]))
}
plot(1:final.time.point, propL[,1], col="grey", pch=3, xlab="Time (1–150 days)", ylab="Prevalence of s1 among solvers (prevS1)", ylim=c(0,1), type="l", cex.lab=1.1)
for(i in 2:65){
	points(1:final.time.point, propL[,i], col="grey", pch=3, ylim=c(0,1), type="l")
}








## PLOT PHASE DIAGRAM

resultsModel_data <- read.csv("model/Old_model/resultsModel_wytham_SS.csv")


par(mfrow=c(2,2), mar=c(2.5,2.5,2.5,0.1), mgp=c(1.5,0.5,0))

# Direct distance between patches

plot(resultsModel_data$Beta[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & (resultsModel_data$Var < 0.01 | resultsModel_data$Var == "NaN"))], resultsModel_data$Lambda[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & (resultsModel_data$Var < 0.01 | resultsModel_data$Var == "NaN"))], pch=15, ylim=c(0,0.1), xlim=c(1,5), col="grey", xlab="β", ylab="λ", main="Direct Distance\nα = 0.001", cex=0.9, cex.main=0.8)
points(resultsModel_data$Beta[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.01)], pch=15, col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.1)], pch=15, col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, col="blue", cex=0.9)

plot(resultsModel_data$Beta[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & (resultsModel_data$Var < 0.01 | resultsModel_data$Var == "NaN"))], resultsModel_data$Lambda[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & (resultsModel_data$Var < 0.01 | resultsModel_data$Var == "NaN"))], pch=15, ylim=c(0,0.1), xlim=c(1,5), col="grey", xlab="β", ylab="λ", main="Direct Distance\nα = 0.005", cex=0.9, cex.main=0.8)
points(resultsModel_data$Beta[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.01)], pch=15, col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.1)], pch=15, col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, col="blue", cex=0.9)

plot(resultsModel_data$Beta[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & (resultsModel_data$Var < 0.01 | resultsModel_data$Var == "NaN"))], resultsModel_data$Lambda[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & (resultsModel_data$Var < 0.01 | resultsModel_data$Var == "NaN"))], pch=15, ylim=c(0,0.1), xlim=c(1,5), col="grey", xlab="β", ylab="λ", main="Direct Distance\nα = 0.01", cex=0.9, cex.main=0.8)
points(resultsModel_data$Beta[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Environment == "directDist" & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, col="blue", cex=0.9)


# Forest distance between patches

plot(resultsModel_data$Beta[which(resultsModel_data$Environment == "forestDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & (resultsModel_data$Var < 0.01 | resultsModel_data$Var == "NaN"))], resultsModel_data$Lambda[which(resultsModel_data$Environment == "forestDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & (resultsModel_data$Var < 0.01 | resultsModel_data$Var == "NaN"))], pch=15, ylim=c(0,0.1), xlim=c(1,5), col="grey", xlab="β", ylab="λ", main="Forest Distance\nα = 0.005", cex=0.9, cex.main=0.8)
points(resultsModel_data$Beta[which(resultsModel_data$Environment == "forestDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Environment == "forestDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.01)], pch=15, col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Environment == "forestDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Environment == "forestDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.1)], pch=15, col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Environment == "forestDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Environment == "forestDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Environment == "forestDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Environment == "forestDist" & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, col="blue", cex=0.9)








## PLOT RESULTS RANDOMIZATION

resultsModel_random_directDist_data <- read.csv("model/Old_model/resultsModel_wytham_sumstats_random_directDist.csv")
resultsModel_random_forestDist_data <- read.csv("model/Old_model/resultsModel_wytham_sumstats_random_forest.csv")

par(mfrow=c(1,3), mar=c(2.5,2.5,2.5,0.1), mgp=c(1.5,0.5,0))

# Forest distance between patches

# beta = 1
length(which(resultsModel_random_forestDist_data$Var[which(resultsModel_random_forestDist_data$Beta == 1)] > 0.01))/100
length(which(resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 1 & resultsModel_random_forestDist_data$Var < 0.01)] > 0.66))/100
length(which(resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 1 & resultsModel_random_forestDist_data$Var < 0.01)] < 0.33))/100
length(which(resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 1 & resultsModel_random_forestDist_data$Var < 0.01)] < 0.66 & resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 1 & resultsModel_random_forestDist_data$Var < 0.01)] > 0.33))/100
plot(resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 1)], resultsModel_random_forestDist_data$Var[which(resultsModel_random_forestDist_data$Beta == 1)], pch=20, xlim=c(0,1), ylim=c(0,0.02), xlab="Total prevalence of behaviour s1", ylab="Variance of prevS1 in sub-populations", main="β = 1")
abline(h=0.01)
points(0.5864, 3.0347e-04, col="green", pch=20, cex=2)

# beta = 1.2
length(which(resultsModel_random_forestDist_data$Var[which(resultsModel_random_forestDist_data$Beta == 1.2)] > 0.01))/100
length(which(resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 1.2 & resultsModel_random_forestDist_data$Var < 0.01)] > 0.66))/100
length(which(resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 1.2 & resultsModel_random_forestDist_data$Var < 0.01)] < 0.33))/100
plot(resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 1.2)], resultsModel_random_forestDist_data$Var[which(resultsModel_random_forestDist_data$Beta == 1.2)], pch=20, xlim=c(0,1), ylim=c(0,0.15), xlab="Total prevalence of behaviour s1", ylab="Variance of prevS1 in sub-populations", main="β = 1.2", col=datcol)

points(resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 1.2 & resultsModel_random_forestDist_data$PoolL / (resultsModel_random_forestDist_data$PoolL + resultsModel_random_forestDist_data$PoolR) > 0.25)], resultsModel_random_forestDist_data$Var[which(resultsModel_random_forestDist_data$Beta == 1.2 & resultsModel_random_forestDist_data$PoolL / (resultsModel_random_forestDist_data$PoolL + resultsModel_random_forestDist_data$PoolR) > 0.25)], pch=20, xlim=c(0,1), ylim=c(0,0.15), xlab="Total prevalence of behaviour s1", ylab="Variance of prevS1 in sub-populations", main="β = 1.2", col="orange")
points(resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 1.2 & resultsModel_random_forestDist_data$PoolL / (resultsModel_random_forestDist_data$PoolL + resultsModel_random_forestDist_data$PoolR) > 0.5)], resultsModel_random_forestDist_data$Var[which(resultsModel_random_forestDist_data$Beta == 1.2 & resultsModel_random_forestDist_data$PoolL / (resultsModel_random_forestDist_data$PoolL + resultsModel_random_forestDist_data$PoolR) > 0.5)], pch=20, xlim=c(0,1), ylim=c(0,0.15), xlab="Total prevalence of behaviour s1", ylab="Variance of prevS1 in sub-populations", main="β = 1.2", col="red")
points(resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 1.2 & resultsModel_random_forestDist_data$PoolL / (resultsModel_random_forestDist_data$PoolL + resultsModel_random_forestDist_data$PoolR) > 0.75)], resultsModel_random_forestDist_data$Var[which(resultsModel_random_forestDist_data$Beta == 1.2 & resultsModel_random_forestDist_data$PoolL / (resultsModel_random_forestDist_data$PoolL + resultsModel_random_forestDist_data$PoolR) > 0.75)], pch=20, xlim=c(0,1), ylim=c(0,0.15), xlab="Total prevalence of behaviour s1", ylab="Variance of prevS1 in sub-populations", main="β = 1.2", col="brown4")
abline(h=0.01)
points(0.9951, 8.0505e-05, col="green", pch=20, cex=2)

PoolL_prop <- resultsModel_random_forestDist_data$PoolL / (resultsModel_random_forestDist_data$PoolL + resultsModel_random_forestDist_data$PoolR)

plot(PoolL_prop, resultsModel_random_forestDist_data$Total_prop_Ls, pch=20, xlim=c(0,1), xlab="Pool of naïve individuals initially in contact with s1 compared to s2", ylab="Total prevalence of behaviour s1")

rbPal <- colorRampPalette(c("blue", "yellow","red"))
datcol <- rbPal(10)[as.numeric(cut(PoolL_prop[which(resultsModel_random_forestDist_data$Beta == 1.2)], breaks=seq(0,1,0.1)))]
#datcol[which(is.na(datcol)==T)] <- "white"
legend("bottomleft", inset=.04, bg="grey", box.col="grey", title="Richness\nin migrants", c("> 100","75–100", "50–75", "25–50", "1–25", "0"), fill=c(rev(rbPal(5)),"white"), cex=1.5)



# beta = 5
length(which(resultsModel_random_forestDist_data$Var[which(resultsModel_random_forestDist_data$Beta == 5)] > 0.01))/100
length(which(resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 5 & resultsModel_random_forestDist_data$Var < 0.01)] > 0.66))/100
length(which(resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 5 & resultsModel_random_forestDist_data$Var < 0.01)] < 0.33))/100
plot(resultsModel_random_forestDist_data$Total_prop_Ls[which(resultsModel_random_forestDist_data$Beta == 5)], resultsModel_random_forestDist_data$Var[which(resultsModel_random_forestDist_data$Beta == 5)], pch=20, xlim=c(0,1), ylim=c(0,0.2), xlab="Total prevalence of behaviour s1", ylab="Variance of prevS1 in sub-populations", main="β = 5")
abline(h=0.01)
points(0.6591, 0.1521, col="green", pch=20, cex=2)









hist(resultsModel_random_directDist_data$Var[which(resultsModel_random_directDist_data$Beta == 1)], xlim=c(0,1))
which(resultsModel_random_directDist_data$Var[which(resultsModel_random_directDist_data$Beta == 1)] > 0.01)
hist(resultsModel_random_data$Total_prop_Ls[which(resultsModel_random_data$Beta == 1)], xlim=c(0,1))
mean(resultsModel_random_data$Total_prop_Ls[which(resultsModel_random_data$Beta == 1)])

hist(resultsModel_random_data$Var[which(resultsModel_random_data$Beta == 1.2)], xlim=c(0,0.4))
hist(resultsModel_random_data$Total_prop_Ls[which(resultsModel_random_data$Beta == 1.2)], xlim=c(0,1))

length(which(resultsModel_random_directDist_data$Var[which(resultsModel_random_directDist_data$Beta == 1.2)] > 0.01))/100
length(which(resultsModel_random_directDist_data$Total_prop_Ls[which(resultsModel_random_directDist_data$Beta == 1.2 & resultsModel_random_directDist_data$Var < 0.01)] > 0.66))/100
length(which(resultsModel_random_directDist_data$Total_prop_Ls[which(resultsModel_random_directDist_data$Beta == 1.2 & resultsModel_random_directDist_data$Var < 0.01)] < 0.33))/100

plot(resultsModel_random_directDist_data$Total_prop_Ls[which(resultsModel_random_directDist_data$Beta == 1.2)], resultsModel_random_directDist_data$Var[which(resultsModel_random_directDist_data$Beta == 1.2)], pch=20)
abline(h=0.01)

hist(resultsModel_random_data$Var[which(resultsModel_random_data$Beta == 5)], xlim=c(0,0.4))
hist(resultsModel_random_data$Total_prop_Ls[which(resultsModel_random_data$Beta == 5)], xlim=c(0,1))
length(which(resultsModel_random_data$Var[which(resultsModel_random_data$Beta == 5)] > 0.01))
length(which(resultsModel_random_data$Total_prop_Ls[which(resultsModel_random_data$Beta == 5 & resultsModel_random_data$Var < 0.01)] > 0.66))
length(which(resultsModel_random_data$Total_prop_Ls[which(resultsModel_random_data$Beta == 5 & resultsModel_random_data$Var < 0.01)] < 0.33))
plot(resultsModel_random_data$Total_prop_Ls[which(resultsModel_random_data$Beta == 5)], resultsModel_random_data$Var[which(resultsModel_random_data$Beta == 5)], pch=20)
abline(h=0.01)




# Conformity mechanism simulation

inds <- c(rep("N",20), rep("L", 1), rep("R",0))
inds2 <- vector()
for(i in 1:100000){
	inds.obs <- sample(inds, 8)
	if(length(which(inds.obs == "L")) > length(which(inds.obs == "R"))){
		inds2[i] <- "L"
	}else if(length(which(inds.obs == "R")) > length(which(inds.obs == "L"))){
		inds2[i] <- "R"
	}else if(length(which(inds.obs == "L" | inds.obs == "R")) == 0){
		inds2[i] <- "N"
	}else{
		inds2[i] <- sample(c("L","R"), 1)
	}
}
#1 - (length(which(inds2=="N")) / length(inds2))
length(which(inds2=="L")) / (length(which(inds2=="L")) + length(which(inds2=="R")))

plot(c(0.5,0.6,0.7,0.8,0.9,1), c(0.5,0.6,0.7,0.8,0.9,1), type="l")
points(c(0.5,0.6,0.7,0.8,0.9,1), c(0.5,0.625,0.746,0.852,0.939,1), type="l", col="red")


inds <- c(rep("N",20), rep("L", 15), rep("R",15))
inds2 <- vector()
for(i in 1:100000){
	inds.obs <- sample(inds, 3)
	if(length(which(inds.obs == "L")) > length(which(inds.obs == "R")) & length(which(inds.obs == "L")) > length(which(inds.obs == "N"))){
		inds2[i] <- "L"
	}else if(length(which(inds.obs == "R")) > length(which(inds.obs == "L")) & length(which(inds.obs == "R")) > length(which(inds.obs == "N"))){
		inds2[i] <- "R"
	}else if(length(which(inds.obs == "N")) > length(which(inds.obs == "L")) & length(which(inds.obs == "N")) > length(which(inds.obs == "R"))){
		inds2[i] <- "N"
	}else{
		inds2[i] <- sample(inds.obs, 1)
	}
}
#length(which(inds2=="L")) / length(inds2)
#length(which(inds2=="R")) / length(inds2)
#length(which(inds2=="N")) / length(inds2)
1 - (length(which(inds2=="N")) / length(inds2))
length(which(inds2=="L")) / (length(which(inds2=="L")) + length(which(inds2=="R")))








## 2 PATCHES

#resultsModel <- read.csv("model/Old_model/resultsModel_2patches.csv", header =F)

par(mfrow=c(1,2), mar=c(2.5,2.5,0.5,0.1), mgp=c(1.5,0.5,0))

resultsModel <- resultsModel_4951_a0.01_b4_l0.006
plot(1:151, resultsModel[,1], col="grey", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,3]+resultsModel[1,5],resultsModel[1,2]+resultsModel[1,4]+resultsModel[1,6])), xlab="Days", ylab="Number of individuals")
points(1:151, resultsModel[,3], col="red", type="l")
points(1:151, resultsModel[,5], col="blue", type="l")

plot(1:151, resultsModel[,2], col="grey", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,3]+resultsModel[1,5],resultsModel[1,2]+resultsModel[1,4]+resultsModel[1,6])), xlab="Days", ylab="Number of individuals")
points(1:151, resultsModel[,4], col="red", type="l")
points(1:151, resultsModel[,6], col="blue", type="l")

#patch1_propL <- resultsModel[150,3] / (resultsModel[150,3]+resultsModel[150,5])
#patch2_propL <- resultsModel[150,4] / (resultsModel[150,4]+resultsModel[150,6])

#mean(c(patch1_propL, patch2_propL)) - var(c(patch1_propL, patch2_propL))

#mtest <- moran.test(propL[50,], fd, zero.policy=TRUE)
#moran.results <- c(mtest$statistic, mtest$p.value)




#resultsModel_data <- read.csv("/Users/mariussomveille/Desktop/output_2patches.csv")
resultsModel_data <- read.csv("model/Old_model/resultsModel_2patches_SS.csv")

par(mfrow=c(5,3), mar=c(2.5,2.5,0.5,0.1), mgp=c(1.5,0.5,0))

# IC = 25/75

plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)


# IC = 40/60

plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 40 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

# IC = 49/51

plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 49 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)



# IC = 50/50

plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

# IC = 51/49

plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.001 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.005 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 51 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)











## 3 PATCHES

resultsModel <- read.csv("model/Old_model/resultsModel_3patches.csv", header=F)

par(mfrow=c(1,3), mar=c(2.5,2.5,0.5,0.1), mgp=c(1.5,0.5,0))

plot(1:151, resultsModel[,1], col="grey", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,4]+resultsModel[1,7], resultsModel[1,2]+resultsModel[1,5]+resultsModel[1,8], resultsModel[1,3]+resultsModel[1,6]+resultsModel[1,9])), xlab="Days", ylab="Number of individuals")
points(1:151, resultsModel[,4], col="red", type="l")
points(1:151, resultsModel[,7], col="blue", type="l")

plot(1:151, resultsModel[,2], col="grey", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,4]+resultsModel[1,7], resultsModel[1,2]+resultsModel[1,5]+resultsModel[1,8], resultsModel[1,3]+resultsModel[1,6]+resultsModel[1,9])), xlab="Days", ylab="Number of individuals")
points(1:151, resultsModel[,5], col="red", type="l")
points(1:151, resultsModel[,8], col="blue", type="l")

plot(1:151, resultsModel[,3], col="grey", type="l", ylim=c(0,max(resultsModel[1,1]+resultsModel[1,4]+resultsModel[1,7], resultsModel[1,2]+resultsModel[1,5]+resultsModel[1,8], resultsModel[1,3]+resultsModel[1,6]+resultsModel[1,9])), xlab="Days", ylab="Number of individuals")
points(1:151, resultsModel[,6], col="red", type="l")
points(1:151, resultsModel[,9], col="blue", type="l")




resultsModel_data <- read.csv("/Users/mariussomveille/Desktop/Oxford/Project_Ben_Robin/Wytham-tits-information-flow/model/Old_model/resultsModel_3patches_SS.csv")

par(mfrow=c(5,3), mar=c(2.5,2.5,0.5,0.1), mgp=c(1.5,0.5,0))

# IC = 50/50/50

# dist = 1
plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

# dist = 1.5
plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

# dist = 5
plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 50 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)


# IC = 45/55/50

# dist = 1
plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

# dist = 1.5
plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

# dist = 5
plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)


# IC = 55/45/50

# dist = 1
plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

# dist = 1.5
plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

# dist = 5
plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)


# IC = 55/55/50

# dist = 1
plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

# dist = 1.5
plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

# dist = 5
plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 55 & resultsModel_data$Patch2_Us == 55 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)



# IC = 45/45/50

# dist = 1
plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

# dist = 1.5
plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)

# dist = 5
plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="β", ylab="λ", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 45 & resultsModel_data$Patch2_Us == 45 & resultsModel_data$Distance == 5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)
















plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 100 & resultsModel_data$Patch2_Us == 100 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 100 & resultsModel_data$Patch2_Us == 100 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="Beta", ylab="Lambda", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 100 & resultsModel_data$Patch2_Us == 100 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 100 & resultsModel_data$Patch2_Us == 100 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 100 & resultsModel_data$Patch2_Us == 100 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 100 & resultsModel_data$Patch2_Us == 100 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 100 & resultsModel_data$Patch2_Us == 100 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 100 & resultsModel_data$Patch2_Us == 100 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 100 & resultsModel_data$Patch2_Us == 100 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 100 & resultsModel_data$Patch2_Us == 100 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)


plot(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Patch2_Us == 75 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Patch2_Us == 75 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.33 & resultsModel_data$Total_prop_Ls < 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), xlim=c(1,5), col="grey", xlab="Beta", ylab="Lambda", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Patch2_Us == 75 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Patch2_Us == 75 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.01)], pch=15, ylim=c(0,0.01), col="green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Patch2_Us == 75 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Patch2_Us == 75 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Var >= 0.1)], pch=15, ylim=c(0,0.01), col="dark green", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Patch2_Us == 75 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Patch2_Us == 75 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls > 0.66 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="red", cex=0.9)
points(resultsModel_data$Beta[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Patch2_Us == 75 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], resultsModel_data$Lambda[which(resultsModel_data$Patch1_Us == 25 & resultsModel_data$Patch2_Us == 75 & resultsModel_data$Distance == 1.5 & resultsModel_data$Alpha == 0.01 & resultsModel_data$Total_prop_Ls < 0.33 & resultsModel_data$Var < 0.01)], pch=15, ylim=c(0,0.01), col="blue", cex=0.9)















## FIGURE S1 : sigmoidal acquisition curves for different values of beta

beta=1
x = seq(0,1,0.01)
y = 1 / (1 + exp(-log(x/(1-x))*beta))
plot(x, y, type="l", xlab="Prevalence of the behavioural solution (prevS1 / prevS2)", ylab="Rate of adoption of the behavioural solution (LS1 / LS2)", col="grey")
beta=1.2
y = 1 / (1 + exp(-log(x/(1-x))*beta))
points(x, y, type="l", col="yellow2")
beta=1.5
y = 1 / (1 + exp(-log(x/(1-x))*beta))
points(x, y, type="l", col="gold1")
beta=2
y = 1 / (1 + exp(-log(x/(1-x))*beta))
points(x, y, type="l", col="gold2")
beta=5
y = 1 / (1 + exp(-log(x/(1-x))*beta))
points(x, y, type="l", col="gold3")
beta=10
y = 1 / (1 + exp(-log(x/(1-x))*beta))
points(x, y, type="l", col="gold4")
legend("topleft", inset=.04, box.col="white", title="", c("β = 10","β = 5", "β = 2", "β = 1.5", "β = 1.2", "β = 1"), col=c("gold4", "gold3", "gold2", "gold1", "yellow2", "grey"), lty=c(1,1))





## Plot the network (not spatial)
adj_mat <- (move_per_capita + t(move_per_capita)) / 2
adj_mat[which(adj_mat < 0.01)] <- 0
diag(adj_mat) <- rep(0,65)
feeders_graph <- graph_from_adjacency_matrix(adj_mat, mode="undirected", weighted=T)
pieColors <- list()
for(i in 1:65){ pieColors[[i]] = c("light grey", "red", "blue") }
plot(feeders_graph, vertex.size = loggers[,5]/5, vertex.label=NA, vertex.shape="pie", vertex.pie=model_res_list, vertex.pie.color= pieColors)




## Plot the dynamic over time for the feeders from Lucy's paper

# T3
nonsolvers = as.matrix(resultsModel)[1:20,11]
left = as.matrix(resultsModel)[1:20,65+11]
right = as.matrix(resultsModel)[1:20,130+11]
prop = (left + right) / (nonsolvers + left + right)
plot(1:20, prop, col="red", pch=3, xlab="Time (1–20 days)", ylab="Proportion of solvers", ylim=c(0,1))

# T4
nonsolvers = as.matrix(resultsModel)[1:20,59]
left = as.matrix(resultsModel)[1:20,65+59]
right = as.matrix(resultsModel)[1:20,130+59]
prop = (left + right) / (nonsolvers + left + right)
points(1:20, prop, col="red", pch=4)

# T5
nonsolvers = as.matrix(resultsModel)[1:20,51]
left = as.matrix(resultsModel)[1:20,65+51]
right = as.matrix(resultsModel)[1:20,130+51]
prop = (left + right) / (nonsolvers + left + right)
points(1:20, prop, col="red", pch=5)

# T1
nonsolvers = as.matrix(resultsModel)[1:20,3]
left = as.matrix(resultsModel)[1:20,65+3]
right = as.matrix(resultsModel)[1:20,130+3]
prop = (left + right) / (nonsolvers + left + right)
points(1:20, prop, col="blue", pch=2)

# T2
nonsolvers = as.matrix(resultsModel)[1:20,7]
left = as.matrix(resultsModel)[1:20,65+7]
right = as.matrix(resultsModel)[1:20,130+7]
prop = (left + right) / (nonsolvers + left + right)
points(1:20, prop, col="blue", pch=1)

# C1
nonsolvers = as.matrix(resultsModel)[1:20,36]
left = as.matrix(resultsModel)[1:20,65+36]
right = as.matrix(resultsModel)[1:20,130+36]
prop = (left + right) / (nonsolvers + left + right)
points(1:20, prop, col="green3", pch=6)

# C2
nonsolvers = as.matrix(resultsModel)[1:20,34]
left = as.matrix(resultsModel)[1:20,65+34]
right = as.matrix(resultsModel)[1:20,130+34]
prop = (left + right) / (nonsolvers + left + right)
points(1:20, prop, col="green3", pch=7)

# C3
nonsolvers = as.matrix(resultsModel)[1:20,45]
left = as.matrix(resultsModel)[1:20,65+45]
right = as.matrix(resultsModel)[1:20,130+45]
prop = (left + right) / (nonsolvers + left + right)
points(1:20, prop, col="green3", pch=8)



## Plot the observed map after 20 days from Lucy's paper

obs_props <- cbind(rep(0,65), rep(0,65), rep(0,65))

# Red – left
# T3
obs_props[10,1] <- 30
obs_props[10,2] <- 65
obs_props[10,3] <- 5
obs_props[11,1] <- 30
obs_props[11,2] <- 65
obs_props[11,3] <- 5
obs_props[12,1] <- 30
obs_props[12,2] <- 65
obs_props[12,3] <- 5
# T4
obs_props[58,1] <- 28
obs_props[58,2] <- 70
obs_props[58,3] <- 2
obs_props[59,1] <- 28
obs_props[59,2] <- 70
obs_props[59,3] <- 2
obs_props[60,1] <- 28
obs_props[60,2] <- 70
obs_props[60,3] <- 2
# T5
obs_props[52,1] <- 26
obs_props[52,2] <- 72
obs_props[52,3] <- 2
obs_props[51,1] <- 26
obs_props[51,2] <- 72
obs_props[51,3] <- 2
obs_props[50,1] <- 26
obs_props[50,2] <- 72
obs_props[50,3] <- 2

# Blue – right
# T1
obs_props[4,1] <- 15
obs_props[4,2] <- 9
obs_props[4,3] <- 76
obs_props[3,1] <- 15
obs_props[3,2] <- 9
obs_props[3,3] <- 76
obs_props[2,1] <- 15
obs_props[2,2] <- 9
obs_props[2,3] <- 76
# T2
obs_props[22,1] <- 32
obs_props[22,2] <- 1
obs_props[22,3] <- 67
obs_props[7,1] <- 32
obs_props[7,2] <- 1
obs_props[7,3] <- 67
obs_props[8,1] <- 32
obs_props[8,2] <- 1
obs_props[8,3] <- 67

# controls
# C1
obs_props[37,1] <- 90
obs_props[37,2] <- 6
obs_props[37,3] <- 4
obs_props[36,1] <- 90
obs_props[36,2] <- 6
obs_props[36,3] <- 4
obs_props[35,1] <- 90
obs_props[35,2] <- 6
obs_props[35,3] <- 4
# C2
obs_props[32,1] <- 45
obs_props[32,2] <- 25
obs_props[32,3] <- 30
obs_props[34,1] <- 45
obs_props[34,2] <- 25
obs_props[34,3] <- 30
obs_props[43,1] <- 45
obs_props[43,2] <- 25
obs_props[43,3] <- 30
# C3
obs_props[44,1] <- 70
obs_props[44,2] <- 24
obs_props[44,3] <- 6
obs_props[45,1] <- 70
obs_props[45,2] <- 24
obs_props[45,3] <- 6
obs_props[46,1] <- 70
obs_props[46,2] <- 24
obs_props[46,3] <- 6

obs_props_list <- as.list(as.data.frame(t(obs_props)))

plot(poly.owin)
for(i in 1:65){
	if(sum(obs_props_list[[i]]) > 0){
			add.pie(z=obs_props_list[[i]], x=loggers_coords[i,"x"], y=loggers_coords[i,"y"], labels="", radius=loggers[i,5]*2, col=c("light grey", "red", "blue"))
	}
}