library(parallel)

wythammodel <- function(){

setwd("/data/zool-avian-social-ecology/zool2068/wytham")
	
library(sp)
library(rgdal)
library(spatstat)
library(maptools)
library(igraph)
library(tnet)
library(MASS)
library(mapplots)

#Load data
load("data/Group_by_individual_all_winter_2013.RData")
load("data/Group_by_individual_data_all_winter_2013.RData")
load("data/movements_data.RData")
loggers_coords <- read.csv("data/Wytham_loggers_coordinates.csv")
#wyt <-readOGR("data", "perimeter poly with clearings_region")
#poly.sp<-SpatialPolygons(list(wyt@polygons[[1]]))
#poly.owin<-as(poly.sp,"owin")

#Change site label to match loggers coordinates
group_data[,1] <- as.character(group_data[,1])
group_data$Location[which(group_data$Location == "1A")] <- "1a"
group_data$Location[which(group_data$Location == "1B")] <- "1b"
group_data$Location[which(group_data$Location == "1C")] <- "1c"
group_data$Location[which(group_data$Location == "1D")] <- "1d"
group_data$Location[which(group_data$Location == "1")] <- "1e"
group_data$Location[which(group_data$Location == "1F")] <- "1f"
group_data$Location[which(group_data$Location == "1G")] <- "1g"
group_data$Location[which(group_data$Location == "1H")] <- "1h"
group_data$Location[which(group_data$Location == "2A")] <- "2a"
group_data$Location[which(group_data$Location == "2B")] <- "2b"
group_data$Location[which(group_data$Location == "2C")] <- "2c"
group_data$Location[which(group_data$Location == "2D")] <- "2d"
group_data$Location[which(group_data$Location == "2")] <- "2e"
group_data$Location[which(group_data$Location == "2F")] <- "2f"
group_data$Location[which(group_data$Location == "2G")] <- "2g"
group_data$Location[which(group_data$Location == "2H")] <- "2h"
group_data$Location[which(group_data$Location == "2I")] <- "2i"
group_data$Location[which(group_data$Location == "3A")] <- "3a"
group_data$Location[which(group_data$Location == "3B")] <- "3b"
group_data$Location[which(group_data$Location == "3C")] <- "3c"
group_data$Location[which(group_data$Location == "3D")] <- "3d"
group_data$Location[which(group_data$Location == "3")] <- "3e"
group_data$Location[which(group_data$Location == "3F")] <- "3f"
group_data$Location[which(group_data$Location == "3G")] <- "3g"
group_data$Location[which(group_data$Location == "3H")] <- "3h"
group_data$Location[which(group_data$Location == "4A")] <- "4a"
group_data$Location[which(group_data$Location == "4B")] <- "4b"
group_data$Location[which(group_data$Location == "4C")] <- "4c"
group_data$Location[which(group_data$Location == "4D")] <- "4d"
group_data$Location[which(group_data$Location == "4")] <- "4e"
group_data$Location[which(group_data$Location == "4F")] <- "4f"
group_data$Location[which(group_data$Location == "4G")] <- "4g"
group_data$Location[which(group_data$Location == "4H")] <- "4h"
group_data$Location[which(group_data$Location == "4I")] <- "4i"
group_data$Location[which(group_data$Location == "5A")] <- "5a"
group_data$Location[which(group_data$Location == "5B")] <- "5b"
group_data$Location[which(group_data$Location == "5C")] <- "5c"
group_data$Location[which(group_data$Location == "5D")] <- "5d"
group_data$Location[which(group_data$Location == "5")] <- "5e"
group_data$Location[which(group_data$Location == "5F")] <- "5f"
group_data$Location[which(group_data$Location == "5G")] <- "5g"
group_data$Location[which(group_data$Location == "5H")] <- "5h"
group_data$Location[which(group_data$Location == "5I")] <- "5i"
group_data$Location[which(group_data$Location == "6A")] <- "6a"
group_data$Location[which(group_data$Location == "6B")] <- "6b"
group_data$Location[which(group_data$Location == "6C")] <- "6c"
group_data$Location[which(group_data$Location == "6D")] <- "6d"
group_data$Location[which(group_data$Location == "6")] <- "6e"
group_data$Location[which(group_data$Location == "6F")] <- "6f"
group_data$Location[which(group_data$Location == "6G")] <- "6g"
group_data$Location[which(group_data$Location == "6H")] <- "6h"
group_data$Location[which(group_data$Location == "6I")] <- "6i"
group_data$Location[which(group_data$Location == "7A")] <- "7a"
group_data$Location[which(group_data$Location == "7B")] <- "7b"
group_data$Location[which(group_data$Location == "7C")] <- "7c"
group_data$Location[which(group_data$Location == "7D")] <- "7d"
group_data$Location[which(group_data$Location == "7")] <- "7e"
group_data$Location[which(group_data$Location == "7F")] <- "7f"
group_data$Location[which(group_data$Location == "7G")] <- "7g"
group_data$Location[which(group_data$Location == "7H")] <- "7h"
group_data$Location[which(group_data$Location == "8B")] <- "8b"
group_data$Location[which(group_data$Location == "8C")] <- "8c"
group_data$Location[which(group_data$Location == "8D")] <- "8d"
group_data$Location[which(group_data$Location == "8")] <- "8e"
group_data$Location[which(group_data$Location == "8F")] <- "8f"
group_by_individual = group_by_individual[do.call(order, group_data),] # A 1/0 table with feeding events in row (68057) and individuals in column (729)
group_data = group_data[do.call(order, group_data),]  # A table giving details for each feeding events (location, date and duration)
feeders.id <- unique(group_data[,1])


#Get the duration of feeding events in seconds (and remove feeding events whose duration is 0)
feeding.duration.obs <- group_data[,4] - group_data[,3]
group_data <- group_data[-which(feeding.duration.obs == 0),]
group_by_individual <- group_by_individual[-which(feeding.duration.obs == 0),]
feeding.duration.obs <- feeding.duration.obs[-which(feeding.duration.obs == 0)]

#Get the total number of feeding events attended by each individual and the number of individuals per feeding event
Nb.events.per.ind <- as.vector(apply(group_by_individual, 2, sum))
Nb.inds.per.event <- as.vector(apply(group_by_individual, 1, sum))

#Get the number of feeding events per feeder
Nb.events.per.feeder <- vector()
for(i in 1:length(unique(group_data$Location))){
	Nb.events.per.feeder[i] <- length(which(group_data$Location == unique(group_data$Location)[i]))
}
Nb.events.per.feeder <- data.frame(unique(group_data$Location), as.numeric(Nb.events.per.feeder))

##Get the number of individuals per feeding events for each feeder 
Nb.inds.per.event.per.feeder <- list()
for(i in 1:length(unique(group_data$Location))){
	Nb.inds.per.event.per.feeder[[i]] <- Nb.inds.per.event[which(group_data$Location == unique(group_data$Location)[i])]
}

#Get the distance between each pair of feeder 
feeders.dists = as.matrix(dist(loggers_coords[,2:3], upper=T, diag=T))



##  The model  ##

#Modelling the frequency, size and duration of feeding events at each feeder over the simulation period
feeding.events <- list()
for(i in 1:length(Nb.events.per.feeder[,1])){
	feeding.events[[i]] <- unique(round(runif(Nb.events.per.feeder[i,2], 0, 1350*14)))
}
inds.per.event <- list()
for(i in 1:length(feeding.events)){
	ipc <- rep(0,length(feeding.events[[i]]))
	for(j in 1:length(feeding.events[[i]])){
		while(ipc[j] < 1){
			ipc[j] <- rpois(1, fitdistr(Nb.inds.per.event.per.feeder[[i]], "Poisson")$estimate)
		}
	}
	inds.per.event[[i]] <- ipc
}
events.feederlocation <- vector()
events.numberindividuals <- vector()
for(i in 1:max(unlist(feeding.events))){
	events.locations <- unlist(lapply(feeding.events, function(x) match(i, x)))
	
	if(length(which(events.locations > 0)) > 1){
		events.feeder <- sample(which(events.locations > 0))
	}else{
		events.feeder <- which(events.locations > 0)
	}
	
	if(length(events.feeder > 0)){
		events.nb.ind <- vector()
		for(j in 1:length(events.feeder)){
			events.nb.ind[j] <- inds.per.event[[events.feeder[j]]][events.locations[events.feeder[j]]]
		}
		events.feederlocation <- c(events.feederlocation, events.feeder)
		events.numberindividuals <- c(events.numberindividuals, events.nb.ind)
	}
}
events.table <- data.frame(events.feederlocation, events.numberindividuals)
colnames(events.table) <- c("Feeder", "No_individuals")


outputs <- vector()

for(run in 1:10){

#Parameters
alpha = sample(seq(0.001,2,0.001),1) #0.017   # Related to the movement of birds  
beta = sample(seq(0.001,2,0.001),1)   # Related to the social preferential attachment  



#Initiation: match every individual to a feeder
inds = 1:729  # individuals
feeders = 1:length(feeders.id) # feeders
inds.feeder = round(unlist(lapply(Nb.inds.per.event.per.feeder, sum)) / sum(unlist(lapply(Nb.inds.per.event.per.feeder, sum))) * length(inds))
N = sum(inds.feeder) # total number of individuals
EI = matrix(0, ncol=N, nrow=dim(events.table)[1])  # Table of 1/0 with feeding event in row (in simulation order) and individual in column
FI = matrix(0, ncol=length(feeders), nrow=N)  # Table of number of feeding events previously attended by each individual at each feeder
inds2 <- 1:N
for(i in 1:length(inds.feeder)){
	ind.sampled <- sample(inds2, inds.feeder[i])
	FI[ind.sampled, i] <- 1
	inds2 <- inds2[-match(ind.sampled, inds2)]
}


intersection.between.inds <- matrix(0, ncol=dim(EI)[2], nrow=dim(EI)[2]) 
overlap.between.inds <- matrix(0, ncol=dim(EI)[2], nrow=dim(EI)[2]) 


for(event in 1:dim(events.table)[1]){
	
	f = events.table[event,1]  # at which feeder does the event take place
	
	attending.inds <- vector()
	for(k in 1:events.table[event,2]){
		Pie <- vector()
		if(k==1){
			for(i in 1:N){
				Pie[i] = sum(exp( - alpha * sum(FI[i,]*(1+feeders.dists[f,])) / sum(FI[i,]) ))
			}
			Pie = Pie / sum(Pie)
		}else{
			for(i in 1:N){
				Pie[i] = sum(exp( - ( (alpha * sum(FI[i,]*(1+feeders.dists[f,])) / sum(FI[i,])) + (beta/(1+sum(overlap.between.inds[i,attending.inds]))) ) ))
			}
			Pie = Pie / sum(Pie)
		}
		if(k==1){
			attending.inds[k] <- sample(1:N, 1, prob=Pie)
		}else{
			attending.inds[k] <- sample((1:N)[-attending.inds], 1, prob=Pie[-attending.inds])
		}	
	}
	FI[attending.inds, f] <- FI[attending.inds, f] + 1
	
	EI[event,attending.inds] <- 1 #individual.status[attending.inds]
	
	#Weighted social network
	intersection.between.inds[attending.inds, attending.inds] <- intersection.between.inds[attending.inds, attending.inds] + 1
	for(i in 1:length(attending.inds)){
		for(j in 1:N){
			overlap.between.inds[attending.inds[i], j] <- intersection.between.inds[attending.inds[i], j] / (intersection.between.inds[attending.inds[i], attending.inds[i]] + intersection.between.inds[j, j] - intersection.between.inds[attending.inds[i], j])
			overlap.between.inds[j, attending.inds[i]] <- intersection.between.inds[j, attending.inds[i]] / (intersection.between.inds[j, j] + intersection.between.inds[attending.inds[i], attending.inds[i]] - intersection.between.inds[j, attending.inds[i]])
		}
	}
	diag(overlap.between.inds) <- 0	
}


# Summary statistics

g <- graph.adjacency(overlap.between.inds, weighted=T)
net <- get.data.frame(g)
summary.statistic.social1.sim <- clustering_w(net, measure="am")
summary.statistic.social2.sim <- mean(strength(g))
		
#Weighted spatial network
inds.per.feeder.sim <- list()
for(i in 1:length(feeders)){
	indss <- vector()
	if(length(which(events.table[1:event,1] == i)) > 0){
		for(j in 1:length(which(events.table[1:event,1] == i))){
			indss <- c(indss, which(EI[which(events.table[1:event,1] == i),][j,] != "0"))
		}
		inds.per.feeder.sim[[i]] <- as.vector(indss)
	}
}
inds.per.feeder.sim.unique <- lapply(inds.per.feeder.sim, unique)
feeder.size.sim <- unlist(lapply(inds.per.feeder.sim.unique, length))
intersection.between.feeders <- matrix(0, ncol=length(inds.per.feeder.sim.unique), nrow=length(inds.per.feeder.sim.unique)) 
for(i in 1:length(inds.per.feeder.sim.unique)){
	for(j in 1:length(inds.per.feeder.sim.unique)){
		intersection.between.feeders[i,j] <- length(which(unique(inds.per.feeder.sim[[i]]) %in% unique(inds.per.feeder.sim[[j]]) == T))
	}
}
union.between.feeders <- matrix(0, ncol=length(inds.per.feeder.sim.unique), nrow=length(inds.per.feeder.sim.unique)) 
for(i in 1:length(inds.per.feeder.sim.unique)){
	for(j in 1:length(inds.per.feeder.sim.unique)){
		union.between.feeders[i,j] <- feeder.size.sim[i] + feeder.size.sim[j] - intersection.between.feeders[i,j]
	}
}
overlap.between.feeders <- intersection.between.feeders / union.between.feeders
diag(overlap.between.feeders) <- 0
overlap.between.feeders[which(overlap.between.feeders == "NaN")] <- 0
summary.statistic.spatial.sim <- (sum(apply(feeders.dists * overlap.between.feeders, 1, sum)) / 2) / (sum(apply(overlap.between.feeders, 1, sum)) / 2)

outputs <- rbind(outputs, c(alpha, beta, summary.statistic.social1.sim, summary.statistic.social2.sim, summary.statistic.spatial.sim))

}

print(outputs)

}


cl <- makeCluster( 15 )
out <- clusterCall( 
  cl,
wythammodel
)
stopCluster(cl)

abc = sample(seq(1,10000000,1),1)
pathh <- paste("reswy", abc, ".csv", sep="")
write.csv(out, file=pathh)		