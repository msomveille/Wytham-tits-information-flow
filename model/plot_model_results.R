
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
	add.pie(z=model_res_list[[i]], x=loggers_coords[i,"x"], y=loggers_coords[i,"y"], labels="", radius=loggers[i,5]*2, col=c("light grey", "red", "blue"))
}



## Plot the network (not spatial)
adj_mat <- (move_per_capita + t(move_per_capita)) / 2
adj_mat[which(adj_mat < 0.01)] <- 0
diag(adj_mat) <- rep(0,65)
feeders_graph <- graph_from_adjacency_matrix(adj_mat, mode="undirected", weighted=T)
pieColors <- list()
for(i in 1:65){ pieColors[[i]] = c("light grey", "red", "blue") }
plot(feeders_graph, vertex.size = loggers[,5]/5, vertex.label=NA, vertex.shape="pie", vertex.pie=model_res_list, vertex.pie.color= pieColors)




## Plot the dynamic over time

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



## Plot the observed map after 20 days from Aplin et al (2015)

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