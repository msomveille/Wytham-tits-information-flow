#Load required libraries
library(sp)
library(rgdal)
library(spatstat)
library(maptools)
library(igraph)
library(tnet)
library(MASS)
library(mapplots)

setwd("~/Wytham-tits-information-flow")

#Load data
load("data/Group_by_individual_all_winter_2013.RData")
load("data/Group_by_individual_data_all_winter_2013.RData")
load("data/movements_data.RData")
loggers_coords <- read.csv("data/Wytham_loggers_coordinates.csv")
wyt <-readOGR("~/Wytham-tits-information-flow/data", "perimeter poly with clearings_region")
poly.sp<-SpatialPolygons(list(wyt@polygons[[1]]))
poly.owin<-as(poly.sp,"owin")

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


##  Analyse empirical data  ##

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

#Get the number of feeding events per day and per week-end
daily.number.of.events <- vector()
for(i in 1:length(unique(group_data$Date))){
	daily.number.of.events[i] <- length(which(group_data$Date == unique(group_data$Date)[i]))
}
weekend.number.of.events <- daily.number.of.events[seq(1, 28, 2)] + daily.number.of.events[seq(2, 28, 2)]
weekend.id <- c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14)

#Compute the summary statistics (clustering coeficient of the weighted social network, and weigthed average distance on the spatial network) for the empirical data
summary.statistic.social.obs <- vector()
summary.statistic.spatial.obs <- vector()
for(w in 1:14){
	dates <- unique(group_data$Date)[which(weekend.id == w)]
	group_data_weekend <- group_data[which(group_data$Date == dates[1] | group_data$Date == dates[2]),]
	group_by_individual_weekend <- group_by_individual[which(group_data$Date == dates[1] | group_data$Date == dates[2]),]	
	#weighted spatial network	
	number.of.events.per.ind <- as.vector(apply(group_by_individual_weekend, 2, sum))	
	intersection.between.inds <- matrix(0, ncol=length(number.of.events.per.ind), nrow=length(number.of.events.per.ind)) 
	for(i in 1:length(number.of.events.per.ind)){
		if(length(which(group_by_individual_weekend[,i] == 1)) > 1){
			intersection.between.inds[i,] <- as.vector(apply(group_by_individual_weekend[which(group_by_individual_weekend[,i] == 1), ], 2, sum))
		}else if(length(which(group_by_individual_weekend[,i] == 1)) == 1){
			intersection.between.inds[i,] <- as.vector(group_by_individual_weekend[which(group_by_individual_weekend[,i] == 1), ])
		}
	}	
	union.between.inds <- matrix(0, ncol=length(number.of.events.per.ind), nrow=length(number.of.events.per.ind)) 
	for(i in 1:length(number.of.events.per.ind)){
		for(j in 1:length(number.of.events.per.ind)){
			union.between.inds[i,j] <- number.of.events.per.ind[i] + number.of.events.per.ind[j] - intersection.between.inds[i,j]
		}
	}	
	overlap.between.inds <- intersection.between.inds / union.between.inds
	diag(overlap.between.inds) <- 0	
	overlap.between.inds[which(overlap.between.inds == "NaN")] <- 0
	g <- graph.adjacency(overlap.between.inds, weighted=T)
	net <- get.data.frame(g)
	summary.statistic.social.obs[w] <- clustering_w(net, measure="am")
	
	#weighted spatial network
	inds.per.feeder <- list()
	for(i in 1:length(feeders.id)){
		indss <- vector()
		if(length(group_by_individual_weekend[which(group_data_weekend[,1] == feeders.id[i]),1]) > 0){
			for(j in 1:length(group_by_individual_weekend[which(group_data_weekend[,1] == feeders.id[i]),1])){
				indss <- c(indss, which(group_by_individual_weekend[which(group_data_weekend[,1] == feeders.id[i]),][j,] == 1))
			}
			inds.per.feeder[[i]] <- as.vector(indss)
		}
	}	
	inds.per.feeder.unique <- lapply(inds.per.feeder, unique)
	feeder.size <- unlist(lapply(inds.per.feeder.unique, length))
	intersection.between.feeders <- matrix(0, ncol=length(inds.per.feeder.unique), nrow=length(inds.per.feeder.unique)) 
	for(i in 1:length(inds.per.feeder.unique)){
		for(j in 1:length(inds.per.feeder.unique)){
			intersection.between.feeders[i,j] <- length(which(match(inds.per.feeder.unique[[i]], inds.per.feeder.unique[[j]], nomatch=0) != 0))
		}
	}
	union.between.feeders <- matrix(0, ncol=length(inds.per.feeder), nrow=length(inds.per.feeder)) 
	for(i in 1:length(inds.per.feeder)){
		for(j in 1:length(inds.per.feeder)){
			union.between.feeders[i,j] <- feeder.size[i] + feeder.size[j] - intersection.between.feeders[i,j]
		}
	}
	overlap.between.feeders <- intersection.between.feeders / union.between.feeders
	diag(overlap.between.feeders) <- 0
	overlap.between.feeders[which(overlap.between.feeders == "NaN")] <- 0
	summary.statistic.spatial.obs[w] <- (sum(apply(D * overlap.between.feeders, 1, sum)) / 2) / (sum(apply(overlap.between.feeders, 1, sum)) / 2)
}



##  The model  ##

#Modelling the frequency, size and duration of feeding events at each feeder over the simulation period (1 week-end = 1350 minutes, and there are 14 week-ends)
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
events.table[,3] <- rep(0, length(events.table[,1]))
colnames(events.table)[3] <- "Duration"
for(i in 1:length(events.table$Duration)){
	events.table[i,3] <- sample(feeding.duration.obs[which(Nb.inds.per.event==events.table[i,2])], 1)
}


#Parameters
alpha = 0.017  # Related to the movement of birds  
beta = 0.0015   # Related to the infection rate    


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

#All individuals set to susceptible
individual.status <- rep("S", N)

#Release some Ls and some Rs
individual.status[sample(which(FI[,3] == 1), 2)] <- "R"
individual.status[sample(which(FI[,7] == 1), 2)] <- "R"
individual.status[sample(which(FI[,11] == 1), 2)] <- "L"
individual.status[sample(which(FI[,50] == 1), 2)] <- "L"
individual.status[sample(which(FI[,59] == 1), 2)] <- "L"


# For each feeding event e, compute the probability of each individual i being sampled to attend
for(event in 1:dim(events.table)[1]){
	
	f = events.table[event,1]  # at which feeder does the event take place
	
	Pie <- vector()
	for(i in 1:N){
		Pie[i] = sum(exp( - alpha * sum(FI[i,]*(1+feeders.dists[f,])) / sum(FI[i,]) ))
	}
	Pie = Pie / sum(Pie)
	attending.inds <- sample(1:N, events.table[event,2], prob=Pie)	
	FI[attending.inds, f] <- FI[attending.inds, f] + 1
	
	#Spread of information
	Pr.nothing.happens = exp(- beta * ((sum(individual.status[attending.inds] == "L") + sum(individual.status[attending.inds] == "R")) * events.table[event,3]) / (sum(individual.status[attending.inds] == "L") + sum(individual.status[attending.inds] == "R") + sum(individual.status[attending.inds] == "S")))
	if(Pr.nothing.happens < 1){
		Pr.becomes.L = (sum(individual.status[attending.inds] == "L") / (sum(individual.status[attending.inds] == "L") + sum(individual.status[attending.inds] == "R"))) * (1-Pr.nothing.happens)
		Pr.becomes.R = (sum(individual.status[attending.inds] == "R") / (sum(individual.status[attending.inds] == "L") + sum(individual.status[attending.inds] == "R"))) * (1-Pr.nothing.happens)
	}else{
		Pr.becomes.L = 0
		Pr.becomes.R = 0
	}
	for(a in length(attending.inds)){
		individual.status[attending.inds[a]] <- sample(c(individual.status[attending.inds[a]], "L", "R"), 1, prob=c(Pr.nothing.happens, Pr.becomes.L, Pr.becomes.R))
	}
	
	EI[event,attending.inds] <- individual.status[attending.inds]
}




##  Analyse model outputs  ##

contact = 1
summary.statistic.social.sim <- vector()
summary.statistic.spatial.sim <- vector()
SLR.results <- list()
for(w in 1:14){
	group_by_individual_weekend <- EI[contact:(contact+weekend.number.of.events[w]),]
		
	#Weighted social network	
	ind.number.of.events <- as.vector(apply(group_by_individual_weekend, 2, function(x) length(which(x != "0"))))	
	intersection.between.inds <- matrix(0, ncol=dim(EI)[2], nrow=dim(EI)[2]) 
	for(i in 1:dim(EI)[2]){
		if(length(which(group_by_individual_weekend[,i] != "0")) > 1){
			intersection.between.inds[i,] <- as.vector(apply(group_by_individual_weekend[which(group_by_individual_weekend[,i] != "0"), ], 2, function(x) length(which(x != "0"))))
		}else if(length(which(group_by_individual_weekend[,i] != "0")) == 1){
			int <- group_by_individual_weekend[which(group_by_individual_weekend[,i] != "0"), ]
			int[which(int != "0")] <- "1"
			intersection.between.inds[i,] <- as.numeric(int)
		}
	}
	union.between.inds <- matrix(0, ncol=dim(EI)[2], nrow=dim(EI)[2]) 
	for(i in 1:dim(EI)[2]){
		for(j in 1:dim(EI)[2]){
			union.between.inds[i,j] <- ind.number.of.events[i] + ind.number.of.events[j] - intersection.between.inds[i,j]
		}
	}	
	overlap.between.inds <- intersection.between.inds / union.between.inds
	diag(overlap.between.inds) <- 0	
	overlap.between.inds[which(overlap.between.inds == "NaN")] <- 0
	g <- graph.adjacency(overlap.between.inds, weighted=T)
	net <- get.data.frame(g)
	summary.statistic.social.sim[w] <- clustering_w(net, measure="am")

	#Weighted spatial network
	event.location.weekend <- events.table[contact:(contact+weekend.number.of.events[w]),1]
	inds.per.feeder.sim <- list()
	for(i in 1:length(feeders)){
		indss <- vector()
		if(length(which(event.location.weekend == i)) > 0){
			for(j in 1:length(which(event.location.weekend == i))){
				indss <- c(indss, which(group_by_individual_weekend[which(event.location.weekend == i),][j,] != "0"))
			}
			inds.per.feeder.sim[[i]] <- as.vector(indss)
		}
	}	
	inds.per.feeder.sim.unique <- lapply(inds.per.feeder.sim, unique)
	feeder.size.sim <- unlist(lapply(inds.per.feeder.sim.unique, length))
	intersection.between.feeders <- matrix(0, ncol=length(inds.per.feeder.sim.unique), nrow=length(inds.per.feeder.sim.unique)) 
	for(i in 1:length(inds.per.feeder.sim.unique)){
		for(j in 1:length(inds.per.feeder.sim.unique)){
			intersection.between.feeders[i,j] <- length(which(match(inds.per.feeder.sim.unique[[i]], inds.per.feeder.sim.unique[[j]], nomatch=0) != 0))
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
	summary.statistic.spatial.sim[w] <- (sum(apply(D * overlap.between.feeders, 1, sum)) / 2) / (sum(apply(overlap.between.feeders, 1, sum)) / 2)
	
	#Information transmission
	SLR <- matrix(0, ncol=3, nrow=length(feeders))
	colnames(SLR) <- c("S", "L", "R")
	for(i in 1:length(feeders)){
		SLR[i,1] = length(which(group_by_individual_weekend[which(event.location.weekend == i),] == "S"))
		SLR[i,2] = length(which(group_by_individual_weekend[which(event.location.weekend == i),] == "L"))
		SLR[i,3] = length(which(group_by_individual_weekend[which(event.location.weekend == i),] == "R"))
	}
	SLR.results[[w]] <- SLR
	
	contact = contact + weekend.number.of.events[w]		
}


#Plot the comparison of empirical and simulated summary statistics
X = 1:14
par(mfrow=c(2,2), mar=c(3,3,2,1), mgp=c(1.8,0.5,0))
plot(1:14, summary.statistic.spatial.obs, pch=20, ylim= c(0,1000), xlab="Week-ends", ylab="D (spatial network)", main="Empirical data")
abline(lm(summary.statistic.spatial.obs ~ X), col="red")
plot(1:13, summary.statistic.spatial.sim, pch=20, ylim= c(0,1000), xlab="Week-ends", ylab="D (spatial network)", main="Model outputs")
abline(lm(summary.statistic.spatial.sim ~ X[1:13]), col="red")
plot(1:14, summary.statistic.social.obs, pch=20, ylim= c(0,1), xlab="Week-ends", ylab="C (social network)")
abline(lm(summary.statistic.social.obs ~ X), col="red")
plot(1:13, summary.statistic.social.sim, pch=20, ylim= c(0,1), xlab="Week-ends", ylab="C (social network)")
abline(lm(summary.statistic.social.sim ~ X[1:13]), col="red")

#Plot the information spread
par(mfrow=c(3,4), mar=c(0.5,0.5,0.5,0.5), mgp=c(1.5,0.5,0))
for(w in 1:12){
	SLR.results.list <- as.list(as.data.frame(t(SLR.results[[w]])))
	plot(poly.owin, main=paste("Week-end", w))
	for(i in 1:65){
		add.pie(z=SLR.results.list[[i]], x=loggers_coords[i,"x"], y=loggers_coords[i,"y"], labels="", radius=loggers[i,5]*2, col=c("light grey", "red", "blue"))
	}
}



