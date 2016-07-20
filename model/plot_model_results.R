
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

## Plot the flow of information in wytham woods (i.e. results of the model)

setwd("/Users/mariussomveille/Desktop/Oxford/Project_Ben_Robin/Wytham-tits-information-flow")

load("bird_movement/movements_data.RData")
resultsModel <- read.csv("model/resultsModel.csv")
loggers_coords <- read.csv("Wytham_loggers_coordinates.csv")
model_res <- cbind(as.matrix(resultsModel)[40,1:65], as.matrix(resultsModel)[40,66:130], as.matrix(resultsModel)[40,131:195])
model_res_list <- as.list(as.data.frame(t(model_res)))

wyt <-readOGR("/Users/mariussomveille/Desktop/Oxford/Project_Ben_Robin/wytham/EllaCole", "perimeter poly with clearings_region")
poly.sp<-SpatialPolygons(list(wyt@polygons[[1]]))
poly.owin<-as(poly.sp,"owin")

plot(poly.owin)
for(i in 1:65){
	add.pie(z=model_res_list[[i]], x=loggers_coords[i,"x"], y=loggers_coords[i,"y"], labels="", radius=loggers[i,5], col=c("light grey", "red", "blue"))
}



## Plot the network (not spatial)
adj_mat <- (move_per_capita + t(move_per_capita)) / 2
adj_mat[which(adj_mat < 0.01)] <- 0
diag(adj_mat) <- rep(0,65)
feeders_graph <- graph_from_adjacency_matrix(adj_mat, mode="undirected", weighted=T)
pieColors <- list()
for(i in 1:65){ pieColors[[i]] = c("light grey", "red", "blue") }
plot(feeders_graph, vertex.size = loggers[,5]/5, vertex.label=NA, vertex.shape="pie", vertex.pie=model_res_list, vertex.pie.color= pieColors)


