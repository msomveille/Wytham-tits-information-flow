#Load required libraries
library(sp)
library(rgdal)
library(spatstat)
library(maptools)
library(igraph)
library(tnet)
library(MASS)
library(mapplots)
library(spdep)
library(boot)

setwd("~/Wytham-tits-information-flow") # /Users/mariussomveille/Desktop/Oxford/Project_Ben_Robin

#Load data
load("data/Group_by_individual_all_winter_2013.RData")
load("data/Group_by_individual_data_all_winter_2013.RData")
load("data/movements_data.RData")
load("data/forest_distances.RData")
loggers_coords <- read.csv("data/Wytham_loggers_coordinates.csv")
wyt <-readOGR("data", "perimeter poly with clearings_region")
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

Nb.unique.ind.per.feeder <- vector()
for(i in 1:length(feeders.id)){
	Nb.unique.ind.per.feeder[i] <- length(which(apply(group_by_individual[which(group_data[,1] == feeders.id[i]),], 2, sum) > 0))
}
relationship.NBinds.NBevents <- summary(lm(Nb.events.per.feeder[,2] ~ Nb.unique.ind.per.feeder))$coefficients[,1]

#Get the distance between each pair of feeder 
feeders.dists = as.matrix(dist(loggers_coords[,2:3], upper=T, diag=T))

group_by_individual2 <- group_by_individual[-which(group_data[,1]== feeders.id[61] | group_data[,1]== feeders.id[62] | group_data[,1]== feeders.id[63] | group_data[,1]== feeders.id[64] | group_data[,1]== feeders.id[65]),]
group_data2 <- group_data[-which(group_data[,1]== feeders.id[61] | group_data[,1]== feeders.id[62] | group_data[,1]== feeders.id[63] | group_data[,1]== feeders.id[64] | group_data[,1]== feeders.id[65]),]

forest.distances <- forest.distances[1:60,1:60]
feeders.id <- feeders.id[1:60]



#####  Training the model  #####  

group_by_individual_timeorder <- group_by_individual2[order(group_data2[,5]),]
group_data_timeorder <- group_data2[order(group_data2[,5]),-which(apply(group_by_individual_timeorder, 2, sum) < 10)]
group_by_individual_timeorder <- group_by_individual_timeorder[,-which(apply(group_by_individual_timeorder, 2, sum) < 10)]
group_data_timeorder <- group_data_timeorder[-which(apply(group_by_individual_timeorder, 1, sum) == 0),]
group_by_individual_timeorder <- group_by_individual_timeorder[-which(apply(group_by_individual_timeorder, 1, sum) == 0),]


#group_by_individual_timeorder_year1 <- group_by_individual_timeorder[which(is.element(group_data_timeorder$Date, levels(group_data_timeorder$Date)[1:14]) == T),]
#group_data_timeorder_year1 <- group_data_timeorder[which(is.element(group_data_timeorder$Date, levels(group_data_timeorder$Date)[1:14]) == T),]
#group_by_individual_timeorder_year1 <- group_by_individual_timeorder_year1[,-which(apply(group_by_individual_timeorder_year1, 2, sum) < 10)]



#Do groups of attending individuals spend the whole day together (more or less)? PANMIXIA?

days <- levels(group_data_timeorder[,2])
day.events <- which(group_data_timeorder[,2] == days[2])
inds.pres.day <- apply(group_by_individual_timeorder[day.events,], 2, function(x) which(x==1))
inds.pres.day.locs <- lapply(inds.pres.day, function(x) group_data_timeorder[day.events,1][x])
inds.pres.day.locs.number <- lapply(inds.pres.day.locs, function(x) length(unique(x)))
ind.residents <- which(inds.pres.day.locs.number == 1)
ind.daily.friends <- list()
for(i in 1:length(ind.residents)){
	if(length(inds.pres.day[[ind.residents[i]]]) > 1){
		ind.daily.friends[[i]] <- as.numeric(unlist(apply(group_by_individual_timeorder[day.events, ind.residents][inds.pres.day[[ind.residents[i]]],], 1, function(x) which(x == 1))))
		ind.daily.friends[[i]] <- ind.daily.friends[[i]][-which(ind.daily.friends[[i]] == i)]
	}else if(length(inds.pres.day[[i]]) == 1){
		ind.daily.friends[[i]] <- as.numeric(which(group_by_individual_timeorder[day.events, ind.residents][inds.pres.day[[i]],] == 1))
		ind.daily.friends[[i]] <- ind.daily.friends[[i]][-which(ind.daily.friends[[i]] == i)]
	}else{
		ind.daily.friends[[i]] <- NA
	}	
}
ind.daily.friends.unique <- lapply(ind.daily.friends, unique)
ind.residents.locs <- unlist(lapply(inds.pres.day.locs[ind.residents], unique))
fraction.localinds.interacting <- vector()
for(i in 1:length(ind.daily.friends.unique)){
	fraction.localinds.interacting[i] <- length(which(match(ind.daily.friends.unique[[i]], which(ind.residents.locs == ind.residents.locs[[i]]), nomatch=0) > 0)) / length(ind.daily.friends.unique[[i]])
}


#What is the probability of foraging based on if foraged yesterday

days <- levels(group_data_timeorder[,2])
foraging.inds <- list()
daily.fraction.of.ind.foraging <- vector()
for(i in 1:length(days)){
	day.events <- which(group_data_timeorder[,2] == days[i])
	inds.pres.day <- apply(group_by_individual_timeorder[day.events,], 2, function(x) which(x==1))
	foraging.inds[[i]] <- as.numeric(which(lapply(inds.pres.day, length) > 0))
	daily.fraction.of.ind.foraging[i] <- length(which(lapply(inds.pres.day, length) > 0)) / dim(group_by_individual_timeorder)[2]
}
daily.proba.attending <- mean(daily.fraction.of.ind.foraging)
proba.attending.1 <- vector()
proba.attending.2 <- vector()
for(i in 1:length(seq(2, 28, 2))){
	proba.attending.1[i] <- length(which(match(foraging.inds[[seq(2, 28, 2)[i]-1]], foraging.inds[[seq(2, 28, 2)[i]]], nomatch=0)>0)) / length(foraging.inds[[seq(2, 28, 2)[i]-1]]) # If the ind feeded yesterday
	proba.attending.2[i] <- length(which(match((1:dim(group_by_individual_timeorder)[2])[-foraging.inds[[seq(2, 28, 2)[i]-1]]], foraging.inds[[seq(2, 28, 2)[i]]], nomatch=0)>0)) / length((1:728)[-foraging.inds[[seq(2, 28, 2)[i]-1]]]) # If the ind did not feed yesterday
}
p1 <- median(proba.attending.1)
p2 <- median(proba.attending.2)
ind.nb <- vector()
inds <- rep(0,dim(group_by_individual_timeorder)[2])
inds[foraging.inds[[1]]] <- 1
for(i in 1:28){
	inds.probas <- rep(p2, dim(group_by_individual_timeorder)[2]) #rep(sample(proba.attending.2, 1), dim(group_by_individual_timeorder)[2])
	inds.probas[which(inds == 1)] <- p1  #sample(proba.attending.1, 1)
	inds <- sapply(inds.probas, function(x) sample(c(1,0), 1, prob=c(x, 1-x)))
	ind.nb <- c(ind.nb, sum(inds))
}
#plot(1:28, daily.fraction.of.ind.foraging*dim(group_by_individual_timeorder)[2], pch=20, col="red", ylim=c(0,dim(group_by_individual_timeorder)[2]))
#points(1:28, ind.nb[1:28], pch=20)


#For each individual, what is the probability of spending the day near a feeder given where it has spent yesterday?

#Matrix of movements between feeders -- probability to move between pairs of feeder within a day.

Fd_eq_1 <- vector()
Fd_sup_eq_1 <- vector()
Fd_sup_1_pairs <- list()
for(i in 1:length(seq(1, 28, 1))){
	day.events <- which(group_data_timeorder[,2] == days[seq(1, 28, 1)[i]])
	inds.attended.events.today <- apply(group_by_individual_timeorder[day.events,], 2, function(x) which(x==1))
	inds.attended.events.today.locs <- lapply(inds.attended.events.today, function(x) group_data_timeorder[day.events,1][x])
	inds.attended.events.today.locs.numbers <- lapply(inds.attended.events.today.locs, function(x) length(unique(x)))
	pairs <- vector()
	for(feeder in 1:length(feeders.id)){
		Fd_eq_1 <- c(Fd_eq_1, length(which(unlist(lapply(inds.attended.events.today.locs[which(inds.attended.events.today.locs.numbers == 1)], unique)) == feeders.id[feeder])))
		Fd_sup_eq_1 <- c(Fd_sup_eq_1, length(which(lapply(inds.attended.events.today.locs[which(inds.attended.events.today.locs.numbers >= 1)], function(x) is.element(feeders.id[feeder],x)) == T)))
		for(feeder2 in 1:length(feeders.id)){
			pairs <- c(pairs, length(which(lapply(inds.attended.events.today.locs[which(inds.attended.events.today.locs.numbers > 1)], function(x) is.element(feeders.id[feeder],x) && is.element(feeders.id[feeder2],x)) == T)))
		}
	}
	Fd_sup_1_pairs[[i]] <- matrix(pairs, nrow=length(feeders.id), ncol=length(feeders.id), byrow=T)
}
Fd_sup_1_pairs2 <- Reduce("+", Fd_sup_1_pairs)

diag(Fd_sup_1_pairs2) -> Fd_sup_1
diag(Fd_sup_1_pairs2) <- 0

Pnotmoving <- apply(matrix(Fd_eq_1, nrow=28, ncol=length(feeders.id), byrow=T),2,sum) / apply(matrix(Fd_sup_eq_1, nrow=28, ncol=length(feeders.id), byrow=T),2,sum)
Pmoving <- 1 - Pnotmoving

move_per_capita <- move_per_capita[1:60,1:60]
movement.probas <- move_per_capita
for(i in 1:dim(move_per_capita)[1]){
	movement.probas[i,] <- (move_per_capita[i,] / sum(move_per_capita[i,], na.rm=T)) * Pmoving[i]
}
diag(movement.probas) <- Pnotmoving













movement.probas <- apply(Fd_sup_1_pairs2, 1, function(x) x/apply(matrix(Fd_sup_eq_1, nrow=28, ncol=length(feeders.id), byrow=T),2,sum))





diag(movement.probas) -> Fd_sup_1
diag(movement.probas) <- 0 
#for(i in 1:dim(movement.probas)[1]){
#	movement.probas[i,] <- movement.probas[i,] * Fd_sup_1[i]/apply(movement.probas, 1, sum)[i]
#}
#diag(movement.probas) <- 0
#diag(movement.probas) <- apply(matrix(Fd_eq_1, nrow=28, ncol=length(feeders.id), byrow=T),2,sum) / apply(matrix(Fd_sup_eq_1, nrow=28, ncol=length(feeders.id), byrow=T),2,sum)

Pnotmoving <- apply(matrix(Fd_eq_1, nrow=28, ncol=length(feeders.id), byrow=T),2,sum) / apply(matrix(Fd_sup_eq_1, nrow=28, ncol=length(feeders.id), byrow=T),2,sum)
Pnotmoving <- mean(Pnotmoving)

data <- data.frame(Pmvt = as.vector(movement.probas), ForestDist=as.vector(forest.distances))
data <- data[-which(is.na(data[,2])==T),]
mod <- nls(Pmvt ~ exp(a + b * ForestDist), data=data, start=list(a=0,b=0))
movement.function <- function(x){
	return(-0.051898 - 0.008044*x)
} 
mvt.pred <- apply(forest.distances, c(1,2), movement.function)

#Plot Fig 2
diag(mvt.pred) <- 0
dataPred <- data.frame(Pmvt = as.vector(mvt.pred), ForestDist=as.vector(forest.distances))
dataPred <- dataPred[-which(is.na(dataPred[,2])==T),]
dataPred <- dataPred[order(dataPred[,2]),]
#plot(log(forest.distances, base=10), log(movement.probas, base=10), pch=20, ylab="Movement probability (log10)", xlab="Forest distance (log10)")
#plot(data$ForestDist, data$Pmvt, pch=20, ylab="Movement probability", xlab="Forest distance")
plot(data$ForestDist, data$Pmvt, pch=20, ylab="Movement probability", xlab="Forest distance")
lines(dataPred$ForestDist, dataPred$Pmvt, col="red", lwd=2)

for(i in 1:dim(mvt.pred)[1]){
	mvt.pred[i,] <- mvt.pred[i,] * (1-Pnotmoving) / apply(mvt.pred, 1, sum)[i]
}
diag(mvt.pred) <- Pnotmoving








##### RUN THE MODEL ####


conformity.function <- function(x){
	return(1 / (1+exp(-alpha*logit(x))))
} 

transmission <- function(ind, party){
	number.of.left <- length(which(party=="L"))
	number.of.right <- length(which(party=="R"))
	if(number.of.left + number.of.right > 0){
		if(ind == "S"){
			proba.nothing <- exp(-(beta*((number.of.left + number.of.right)/length(party)) + (2*gamma)) * T )
			proba.become.left <- (1-proba.nothing) * conformity.function((beta*number.of.left + gamma) / (beta*(number.of.left + number.of.right) + (2*gamma)))
			proba.become.right <- (1-proba.nothing) * conformity.function((beta*number.of.right + gamma) / (beta*(number.of.left + number.of.right) + (2*gamma)))
			return(sample(c("S", "L", "R"), 1, prob=c(proba.nothing, proba.become.left, proba.become.right)))
		}else if(ind == "L"){
			proba.nothing <- exp(-(beta*((number.of.left + number.of.right)/length(party)) + (2*gamma)) * T )
			proba.become.right <- (1-proba.nothing) * conformity.function((beta*number.of.right + gamma) / (beta*(number.of.left + number.of.right) + (2*gamma)))
			return(sample(c("L", "R"), 1, prob=c(1-proba.become.right, proba.become.right)))
			#proba.nothing <- exp(-(beta*(number.of.right/length(party))) * T )
			#proba.become.right <- 1 - proba.nothing
			#return(sample(c("L", "R"), 1, prob=c(proba.nothing+proba.become.left, proba.become.right)))
		}else{
			proba.nothing <- exp(-(beta*((number.of.left + number.of.right)/length(party)) + (2*gamma)) * T )
			proba.become.left <- (1-proba.nothing) * conformity.function((beta*number.of.left + gamma) / (beta*(number.of.left + number.of.right) + (2*gamma)))
			return(sample(c("L", "R"), 1, prob=c(proba.become.left, 1-proba.become.left)))
			#proba.nothing <- exp(-(beta*(number.of.left/length(party))) * T )
			#proba.become.left <- 1 - proba.nothing
			#return(sample(c("L", "R"), 1, prob=c(proba.become.left, proba.nothing+ proba.become.right)))
		}
	}else{
		return(ind)
	}
}

beta=0.5; alpha=1; gamma=0.01
zeparty <- c(rep("S", 10), rep("L", 9), rep("R", 1))
zeind <- "S"
tres <- vector()
for(i in 1:100){
	tres <- c(tres, transmission(zeind, zeparty))
}
length(which(tres == "S"))
length(which(tres == "L"))
length(which(tres == "R"))



#Parameters
beta=0.3 # social learning rate
gamma=0.001 # asocial learning rate
alpha = 1.3 # conformity parameter (1 --> no conformity, >1 --> increaing conformity)
T=1

proportion.of.lefts <- list()
moran.results <- list()
SLR.results <- list()
proportion.of.solvers <- list()

for(run in 1:100){

#Run a simulation

#Prepare objects (start with yesterday being a randomly sampled day from data)
first.yesterday = sample(1:28, 1)
number.of.days <- 4*5 + 3*2
day.events <- which(group_data_timeorder[,2] == days[first.yesterday])
inds.pres.day <- apply(group_by_individual_timeorder[day.events,], 2, function(x) which(x==1))
daily_presabs <- matrix(0, nrow=number.of.days+1, ncol=ncol(group_by_individual_timeorder))
daily_presabs[1,which(lapply(inds.pres.day, length) > 0)] <- 1
patch.size <- unlist(lapply(Nb.inds.per.event.per.feeder[1:60], sum))
previous.loc <- sample(sample(1:length(feeders.id), ncol(group_by_individual_timeorder), replace=T, prob= patch.size))
daily_locs <- matrix(0, nrow=number.of.days+1, ncol=ncol(group_by_individual_timeorder))
daily_locs[1,] <- previous.loc
daily_status <- matrix(0, nrow=number.of.days+1, ncol=ncol(group_by_individual_timeorder))
daily_status[1,] <- rep("S", ncol(group_by_individual_timeorder))
daily_status[1,sample(which(daily_locs[1,] == 11), 2)] <- "L"
daily_status[1,sample(which(daily_locs[1,] == 50), 2)] <- "L"
daily_status[1,sample(which(daily_locs[1,] == 59), 2)] <- "L"
daily_status[1,sample(which(daily_locs[1,] == 3), 2)] <- "R"
daily_status[1,sample(which(daily_locs[1,] == 7), 2)] <- "R"


#Run
for(day in 2:(number.of.days+1)){
	
	# Determine new location for each individual (where it will hang out today)
	daily_locs[day,] <- sapply(daily_locs[day-1,], function(x) sample(1:length(feeders.id), 1, prob= movement.probas[x,]))

	# Keep only the individuals that will forage today
	inds.probas <- rep(p2, ncol(group_by_individual_timeorder))
	inds.probas[which(daily_presabs[day-1,] == 1)] <- p1
	inds.foraging.today <- sapply(inds.probas, function(x) sample(c(1,0), 1, prob=c(x, 1-x)))
	daily_presabs[day,] <- inds.foraging.today
	
	# Transmission of information
	daily_status[day,] <- daily_status[day-1,] 
	for(feeder in 1:length(feeders.id)){
		attending.individuals <- which(daily_locs[day,] == feeder & inds.foraging.today == 1)		
		if(length(attending.individuals > 1)){
	 		daily_status[day, attending.individuals] <- sapply(daily_status[day, attending.individuals], function(x) transmission(x,daily_status[day, attending.individuals][-which(daily_status[day, attending.individuals]==x)[1]]))
		}
	}	
}
daily_presabs <- daily_presabs[-1,]
daily_locs <- daily_locs[-1,]	
daily_status <- daily_status[-1,]
	
	
#prop.solvers <- apply(daily_status, 1, function(x) length(which(x != "S")) / dim(daily_status)[2])
#plot(1:26, prop.solvers, pch=20, ylim=c(0,1))	

SLR.res <- list()	
for(i in 1:length(feeders.id)){
	statuses <- vector()
	for(j in 15:25){
		statuses <- c(statuses, daily_status[j,which(daily_locs[j,] == i & daily_presabs[j,] == 1)])
	}
	SLR.res[[i]] <- statuses
}
SLR.res <- lapply(SLR.res, function(x) c(length(which(x=="S")), length(which(x=="L")), length(which(x=="R"))))
SLR.results[[run]] <- do.call(rbind, SLR.res)

#Plot graphs of results

prop.solvers <- list()	
for(day in 1:number.of.days){
	statuses <- list()
	for(i in 1:length(feeders.id)){
		statuses[[i]] <- daily_status[day,which(daily_locs[day,] == i & daily_presabs[day,] == 1)]
	}
	prop.solvers[[day]] <- statuses
}

T1 <- vector()
T2 <- vector()
T3 <- vector()
T4 <- vector()
T5 <- vector()
C1 <- vector()
C2 <- vector()
C3 <- vector()
#T1_solutions <- vector()
#T2_solutions <- vector()
#T3_solutions <- vector()
#T4_solutions <- vector()
#T5_solutions <- vector()
for(day in 1:number.of.days){
	st = unlist(prop.solvers[[day]][which(feeders.id == "1b" | feeders.id == "1c" | feeders.id == "1d")])
	T1[day] = length(which(st != "S")) / length(st)
	#T1_solutions[day] = length(which(st == "R")) / length(which(st != "S"))
	st = unlist(prop.solvers[[day]][which(feeders.id == "1e" | feeders.id == "1g" | feeders.id == "1h")])
	T2[day] = length(which(st != "S")) / length(st)
	#T2_solutions[day] = length(which(st == "R")) / length(which(st != "S"))
	st = unlist(prop.solvers[[day]][which(feeders.id == "2b" | feeders.id == "2c" | feeders.id == "2d")])
	T3[day] = length(which(st != "S")) / length(st)
	#T3_solutions[day] = length(which(st == "L")) / length(which(st != "S"))
	st = unlist(prop.solvers[[day]][which(feeders.id == "7f" | feeders.id == "7g" | feeders.id == "7h")])
	T4[day] = length(which(st != "S")) / length(st)
	#T4_solutions[day] = length(which(st == "L")) / length(which(st != "S"))
	st = unlist(prop.solvers[[day]][which(feeders.id == "6g" | feeders.id == "6h" | feeders.id == "6i")])
	T5[day] = length(which(st != "S")) / length(st)
	#T5_solutions[day] = length(which(st == "L")) / length(which(st != "S"))
	st = unlist(prop.solvers[[day]][which(feeders.id == "5a" | feeders.id == "5b" | feeders.id == "5c")])
	C1[day] = length(which(st != "S")) / length(st)
	st = unlist(prop.solvers[[day]][which(feeders.id == "4g" | feeders.id == "4i" | feeders.id == "5i")])
	C2[day] = length(which(st != "S")) / length(st)
	st = unlist(prop.solvers[[day]][which(feeders.id == "6a" | feeders.id == "6b" | feeders.id == "6c")])
	C3[day] = length(which(st != "S")) / length(st)
}

proportion.of.solvers[[run]] <- rbind(T1,T2,T3,T4,T5,C1,C2,C3)

# Quantify if there are local traditions across the wood
prop_L <- unlist(lapply(SLR.res, function(x) x[2]/(x[2]+x[3])))
if(length(which(prop_L == "NaN"))>0){
	dnn <- dnearneigh(as.matrix(loggers_coords[1:60,][-which(prop_L == "NaN"),2:3]), 0, 500)
	proportion.of.lefts[[run]] <- prop_L[-which(prop_L == "NaN")]
}else{
	dnn <- dnearneigh(as.matrix(loggers_coords[1:60,2:3]), 0, 500)
	proportion.of.lefts[[run]] <- prop_L
}
fd <- mat2listw(nb2mat(dnn, zero.policy=TRUE))
mtest <- moran.test(proportion.of.lefts[[run]], fd, zero.policy=TRUE)
moran.results[[run]] <- c(mtest$statistic, mtest$p.value)

}




par(mfrow=c(3,4), mar=c(2.5,2.5,0.5,0.1), mgp=c(1.5,0.5,0))

# Fig 1b
y = do.call(cbind, proportion.of.solvers)
x = rep(1:number.of.days, length(proportion.of.solvers))
plot(NULL, ylim=c(0,1), xlim=c(1,number.of.days), xlab="Days", ylab="Proportion of solvers")
for(run in 2:length(proportion.of.lefts)){
	lines(ksmooth(x, y[1,], "normal", bandwidth = 2), col="blue")
	lines(ksmooth(x, y[2,], "normal", bandwidth = 2), col="blue")
	lines(ksmooth(x, y[3,], "normal", bandwidth = 2), col="red")
	lines(ksmooth(x, y[4,], "normal", bandwidth = 2), col="red")
	lines(ksmooth(x, y[5,], "normal", bandwidth = 2), col="red")
	lines(ksmooth(x, y[6,], "normal", bandwidth = 2), col="green")
	lines(ksmooth(x, y[7,], "normal", bandwidth = 2), col="green")
	lines(ksmooth(x, y[8,], "normal", bandwidth = 2), col="green")
}



# Fig cumulative distribution
cumvalues <- list()
cumdist <- ecdf(proportion.of.lefts[[1]])
cumvalues[[1]] <- cumdist(seq(0,1,0.1))
plot(seq(0,1,0.1), cumvalues[[1]], ylim=c(0,1), col="grey", type="l", xlab="Fraction of solvers that are Ls", ylab="Cumulative proportion")
for(run in 2:length(proportion.of.lefts)){
	cumdist <- ecdf(proportion.of.lefts[[run]])
	cumvalues[[run]] <- cumdist(seq(0,1,0.1))
	points(seq(0,1,0.1), cumvalues[[run]], col="grey", type="l")
}
points(seq(0,1,0.1), seq(0,1,0.1), type="l")
cumvals <- do.call(cbind, cumvalues)
x = rep(seq(0,1,0.1), length(cumvalues))
lines(ksmooth(x, cumvals, "normal", bandwidth = 0.1), col="red")
cumvals2 <- do.call(rbind, cumvalues)
mtext(round(mean(cumvals2[,10]) - mean(cumvals2[,2]), 3), side=3, line=-1.5, cex=1, at=0.2, col="red")


# Histogram of Moran test p values
#hist(unlist(lapply(moran.results, function(x) x[2])), xlim=c(0,1))
mean(unlist(lapply(moran.results, function(x) x[2])))
max(unlist(lapply(moran.results, function(x) x[2])))
mtext(round(mean(unlist(lapply(moran.results, function(x) x[2]))), 4), side=3, line=-1.5, cex=1)

#Plot map of results
for(run in 1:10){
SLR.results.sum <- unlist(apply(SLR.results[[run]], 1, sum))
plot(poly.owin, main="")
for(i in 1:60){
	if(SLR.results.sum[i] > 0){
		add.pie(z= 
		SLR.results[[run]][i,], x=loggers_coords[i,"x"], y=loggers_coords[i,"y"], labels="", radius=SLR.results.sum[i]/1.5, col=c("light grey", "red", "blue"))
	}
}
}


























## Figure 1 ##

#par(mfrow=c(1,2), mar=c(0.1,0.1,1.5,0.1), mgp=c(0,0,0))
plot(poly.owin, main="", col="light grey")
points(loggers_coords[,2], loggers_coords[,3], col="red", pch=8)



















proba.moving <- vector()
for(i in 1:length(seq(1, 28, 1))){
	day.events <- which(group_data_timeorder[,2] == days[seq(1, 28, 1)[i]])
	inds.pres.day <- apply(group_by_individual_timeorder[day.events,], 2, function(x) which(x==1))
	inds.pres.day.locs <- lapply(inds.pres.day, function(x) group_data_timeorder[day.events,1][x])
	inds.pres.day.locs.number <- lapply(inds.pres.day.locs, function(x) length(unique(x)))
	ind.residents.and.dispersers <- which(inds.pres.day.locs.number > 0)
	inds.pres.day.locs.dist <- as.numeric(unlist(lapply(inds.pres.day.locs[ind.residents.and.dispersers], function(x) forest.distances[ which(feeders.id == x[1]), which(feeders.id == x[length(x)]) ] )))
	inds.pres.day.locs.dist[which(is.na(inds.pres.day.locs.dist) == T)] = 0
	proba.moving[i] <- length(which(inds.pres.day.locs.dist > 0)) / length(inds.pres.day.locs.dist)
}
proba.moving <- mean(proba.moving)

movement.probas2 <- move_per_capita
for(i in 1:dim(move_per_capita)[1]){
	movement.probas2[i,] <- (move_per_capita[i,] / sum(move_per_capita[i,], na.rm=T)) * proba.moving
}
diag(movement.probas2) <- 1-proba.moving







plot(NULL, ylim=c(0,1), xlim=c(1,number.of.days), xlab="Days", ylab="Proportion of solvers")
for(run in 2:length(proportion.of.lefts)){
	points(1:number.of.days, proportion.of.solvers[[run]][1,], pch=20, ylim=c(0,1), col="blue", type="l")
	points(1:number.of.days, proportion.of.solvers[[run]][2,], pch=20, col="blue", type="l")	
	points(1:number.of.days, proportion.of.solvers[[run]][3,], pch=20, col="red", type="l")
	points(1:number.of.days, proportion.of.solvers[[run]][4,], pch=20, col="red", type="l")
	points(1:number.of.days, proportion.of.solvers[[run]][5,], pch=20, col="red", type="l")	
	points(1:number.of.days, proportion.of.solvers[[run]][6,], pch=20, col="green", type="l")		
	points(1:number.of.days, proportion.of.solvers[[run]][7,], pch=20, col="green", type="l")	
	points(1:number.of.days, proportion.of.solvers[[run]][8,], pch=20, col="green", type="l")		
}




# Fig 2a	
plot(1:number.of.days, T1_solutions, pch=20, ylim=c(0,1), col="blue", type="o")
points(1:number.of.days, T2_solutions, pch=20, col="blue", type="o")	
points(1:number.of.days, T3_solutions, pch=20, col="red", type="o")
points(1:number.of.days, T4_solutions, pch=20, col="red", type="o")
points(1:number.of.days, T5_solutions, pch=20, col="red", type="o")	
	


# Fig 2b	
#Individuals' first learnt option
first.option <- apply(daily_status, 2, function(x) which(x != "S")[1])
first.option.loc <- vector()
first.option.status <- vector()
for(i in 1:dim(daily_status)[2]){
	first.option.loc[i] <- daily_locs[,i][first.option[i]]
	first.option.status[i] <- daily_status[,i][first.option[i]]
}

T2_firstoption <- 

T2_firstoption <- which(is.element(first.option.loc, which(feeders.id == "1e" | feeders.id == "1g" | feeders.id == "1h"))==T)

#Frequency of option A (right)
T1_freqA <- T1_solutions
T2_freqA <- T2_solutions
T3_freqA <- 1-T3_solutions
T4_freqA <- 1-T4_solutions
T5_freqA <- 1-T5_solutions
	
	
	
	FI <- matrix(0, nrow=length(feeders.id), ncol=ncol(group_by_individual_timeorder))
for(i in 1:length(previous.loc)){
	FI[previous.loc[i],i] <- 1
}
	
			if(feeder == release.lefts && length(attending.individuals)>=2){
			individual.status[attending.individuals[sample(1:length(attending.individuals), 2, replace=F)]] <- "L"
		}
		if(ev == release.rights && length(attending.individuals)>=2){
			individual.status[attending.individuals[sample(1:length(attending.individuals), 2, replace=F)]] <- "R"
		}
		
	
	
	
	
	
	
	
	
	
	
	#Update previous times
	previous.time.sim = previous.time.sim + (event.infos[event,2] - event.infos[event-1,2])
	
	#Compute previous distances to the location of the current event
	previous.dist.sim <- feeders.dists[which(feeders.id==event.infos[event,1]), apply(FI, 1, function(x) which(x==1))]
	
	#Get the social clusters from previous co-occurences
	social.clusters <- unique(apply(rbind(previous.dist.sim, previous.time.sim), 2, function(x) which(previous.dist.sim == x[1] & previous.time.sim == x[2])))
	social.clusters.distime <- lapply(social.clusters, function(x) rbind(previous.dist.sim[x[1]], previous.time.sim[x[1]]))
	social.clusters.distime <- do.call("cbind", social.clusters.distime)

	#Compute P(presence | d,t) for each social cluster
	quartile1 <- which(social.clusters.distime[2,] > quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[1] & social.clusters.distime[2,] <= quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[2])
	quartile2 <- which(social.clusters.distime[2,] > quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[2] & social.clusters.distime[2,] <= quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[3])
	quartile3 <- which(social.clusters.distime[2,] > quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[3] & social.clusters.distime[2,] <= quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[4])
	quartile4 <- which(social.clusters.distime[2,] > quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[4] & social.clusters.distime[2,] <= quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[5])
	social.clusters.probas <- rep(0,length(social.clusters.distime[1,]))
	social.clusters.probas[quartile1] <- sapply(social.clusters.distime[1,quartile1], function(x) predict(glm1, newdata=data.frame(Dist=x), type="response"))
	social.clusters.probas[quartile2] <- sapply(social.clusters.distime[1,quartile2], function(x) predict(glm2, newdata=data.frame(Dist=x), type="response"))
	social.clusters.probas[quartile3] <- sapply(social.clusters.distime[1,quartile3], function(x) predict(glm3, newdata=data.frame(Dist=x), type="response"))
	social.clusters.probas[quartile4] <- sapply(social.clusters.distime[1,quartile4], function(x) predict(glm4, newdata=data.frame(Dist=x), type="response"))
	
	#Sampling clusters
	selected.cluster <- sample(1:length(social.clusters.probas), 1, prob= social.clusters.probas)
	social.clusters.probas[selected.cluster] <- 1
	social.clusters.sampled <- which(sapply(social.clusters.probas, function(x) sample(c(1,0), 1, prob=c(x,1-x))) == 1)
	
	#Sampling individuals within the selected clusters
	ind.attending <- vector()
	for(i in 1:length(social.clusters.sampled)){
		soc.clust <- social.clusters[[social.clusters.sampled[i]]]
		if(length(soc.clust) > 2){
			first.ind <- sample(soc.clust, 1)
			soc.clust <- soc.clust[-which(soc.clust == first.ind)]
			n = length(soc.clust)
			probas <- dbinom(0:n, n, proba.binom)
			number.ind.sampled <- which(probas == sample(probas, 1, prob=probas))-1
			ind.attending <- c(ind.attending, first.ind, sample(soc.clust, number.ind.sampled))
		}else if(length(soc.clust) == 2){
			first.ind <- sample(soc.clust, 1)
			soc.clust <- soc.clust[-which(soc.clust == first.ind)]
			n = length(soc.clust)
			probas <- dbinom(0:n, n, proba.binom)
			number.ind.sampled <- which(probas == sample(probas, 1, prob=probas))-1
			if(number.ind.sampled==1){
				ind.attending <- c(ind.attending, first.ind, soc.clust)
			}else{
				ind.attending <- c(ind.attending, first.ind)
			}
		}else{
			ind.attending <- c(ind.attending, soc.clust)
		}
	}
	group_by_individual_simulated[event, unlist(ind.attending)] <- 1
	
	#Update FI
	FI[which(group_by_individual_simulated[event,] == 1),] <- 0
	FI[which(group_by_individual_simulated[event,] == 1),which(feeders.id==event.infos[event,1])] <- 1
	
	#Set previous times of sampled individuals to 0
	previous.time.sim[which(group_by_individual_simulated[event,] == 1)] <- 0
	
}


























previous.location <- lapply(apply(group_by_individual_timeorder[1:(day.events[1]-1),], 2, function(x) which(x==1)), function(x) x[length(x)])
previous.location[which(lapply(previous.location, length) == 0)] = NA
FI_data <- 
current.presabs.training <- group_by_individual_timeorder[day.events[1],]	
previous.dist.training <- sapply(group_data_timeorder[unlist(previous.location),1], function(x) forest.distances[which(feeders.id == x), which(feeders.id == group_data_timeorder[day.events[1],1])])



previous.dist.training <- list()
previous.time.training <- list()
current.presabs.training <- list()
cluster.size <- vector()
presence.number <- vector()
eee = 0
for(e in sample(10000:67000,500)){
	previous.location <- lapply(apply(group_by_individual_timeorder[1:(e-1),], 2, function(x) which(x==1)), function(x) x[length(x)])
	previous.location[which(lapply(previous.location, length) == 0)] = NA
	current.presabs.training[[e]] <- group_by_individual_timeorder[e,]	
	previous.dist.training[[e]] <- sapply(group_data_timeorder[unlist(previous.location),1], function(x) feeders.dists[which(feeders.id == x), which(feeders.id == group_data_timeorder[e,1])])
	previous.dist.training[[e]][which(lapply(previous.dist.training[[e]],length) == 0)] = NA
	previous.dist.training[[e]] <- unlist(previous.dist.training[[e]])
	previous.time.training[[e]] <- sapply(unlist(previous.location), function(x) group_data_timeorder[e,5] - group_data_timeorder[x,5])
	previous.time.training[[e]][which(lapply(previous.time.training[[e]],length) == 0)] = NA
	
	#Get the social clusters from previous co-occurences
	social.clusters <- unique(apply(rbind(previous.dist.training[[e]], previous.time.training[[e]]), 2, function(x) which(previous.dist.training[[e]] == x[1] & previous.time.training[[e]] == x[2])))
	#social.clusters.distime <- lapply(social.clusters, function(x) rbind(previous.dist.training[[e]][x[1]], previous.time.training[[e]][x[1]]))
	#social.clusters.distime <- do.call("cbind", social.clusters.distime)
	
	social.clusters.sel <- vector()
	for(i in 1:length(which(current.presabs.training[[e]] == 1))){
		if(is.na(previous.location[which(current.presabs.training[[e]] == 1)[i]]) == F){
			social.clusters.sel[i] <- which(lapply(social.clusters, function(x) is.element(which(current.presabs.training[[e]] == 1)[i], x)) == T)
		}
	}
	social.clusters.sel.unique = unique(social.clusters.sel)
	cluster.size <- c(cluster.size, unlist(lapply(social.clusters[social.clusters.sel.unique], length)))
	presence.number <- c(presence.number, sapply(social.clusters.sel.unique, function(x) length(which(social.clusters.sel==x))))
	
	eee = eee + 1
	print(eee)
}
data.for.model <- as.data.frame(cbind(unlist(current.presabs.training), unlist(previous.dist.training), unlist(previous.time.training)))
colnames(data.for.model) <- c("PresAbs", "Dist", "Time")

#Estimation of proba.binom
proba.binom <- sum(presence.number-1) / sum(cluster.size-1)


#Generalized Linear Models for several fixed t
glm1 <- glm(PresAbs ~ Dist, family=binomial(), data= data.for.model[which((data.for.model[,3] > quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[1]) & (data.for.model[,3] <= quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[2])),])
glm2 <- glm(PresAbs ~ Dist, family=binomial(), data= data.for.model[which((data.for.model[,3] > quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[2]) & (data.for.model[,3] <= quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[3])),])
glm3 <- glm(PresAbs ~ Dist, family=binomial(), data= data.for.model[which((data.for.model[,3] > quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[3]) & (data.for.model[,3] <= quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[4])),])
glm4 <- glm(PresAbs ~ Dist, family=binomial(), data= data.for.model[which((data.for.model[,3] > quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[4]) & (data.for.model[,3] <= quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[5])),])






	
#Run a simulation


#Prepare objects
group_by_individual_simulated <- matrix(0, ncol=ncol(group_by_individual_timeorder), nrow=nrow(group_by_individual_timeorder))
previous.event.attended <- lapply(apply(group_by_individual_timeorder, 2, function(x) which(x==1)), function(x) x[length(x)])
FI <- matrix(0, nrow=length(previous.event.attended), ncol=length(feeders.id))
for(i in 1:length(previous.event.attended)){
	FI[i,which(feeders.id == group_data_timeorder[unlist(previous.event.attended),1][i])] <- 1
}
previous.time.sim <- sapply(unlist(previous.event.attended), function(x) group_data_timeorder[dim(group_data_timeorder)[1],5] - group_data_timeorder[x,5])
social.net <- matrix(0, nrow=length(previous.event.attended), ncol=length(previous.event.attended))
for(i in 1:length(previous.event.attended)){
	social.net[i,which(unlist(previous.event.attended) == unlist(previous.event.attended)[i])] <- 1
}
diag(social.net) <- 0
event.infos <- group_data_timeorder[,c(1,5)]
event.infos[,2] <- event.infos[,2] - event.infos[1,2]

#Run
for(event in 2:dim(event.infos)[1]){
	
	#Update previous times
	previous.time.sim = previous.time.sim + (event.infos[event,2] - event.infos[event-1,2])
	
	#Compute previous distances to the location of the current event
	previous.dist.sim <- feeders.dists[which(feeders.id==event.infos[event,1]), apply(FI, 1, function(x) which(x==1))]
	
	#Get the social clusters from previous co-occurences
	social.clusters <- unique(apply(rbind(previous.dist.sim, previous.time.sim), 2, function(x) which(previous.dist.sim == x[1] & previous.time.sim == x[2])))
	social.clusters.distime <- lapply(social.clusters, function(x) rbind(previous.dist.sim[x[1]], previous.time.sim[x[1]]))
	social.clusters.distime <- do.call("cbind", social.clusters.distime)

	#Compute P(presence | d,t) for each social cluster
	quartile1 <- which(social.clusters.distime[2,] > quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[1] & social.clusters.distime[2,] <= quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[2])
	quartile2 <- which(social.clusters.distime[2,] > quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[2] & social.clusters.distime[2,] <= quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[3])
	quartile3 <- which(social.clusters.distime[2,] > quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[3] & social.clusters.distime[2,] <= quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[4])
	quartile4 <- which(social.clusters.distime[2,] > quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[4] & social.clusters.distime[2,] <= quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[5])
	social.clusters.probas <- rep(0,length(social.clusters.distime[1,]))
	social.clusters.probas[quartile1] <- sapply(social.clusters.distime[1,quartile1], function(x) predict(glm1, newdata=data.frame(Dist=x), type="response"))
	social.clusters.probas[quartile2] <- sapply(social.clusters.distime[1,quartile2], function(x) predict(glm2, newdata=data.frame(Dist=x), type="response"))
	social.clusters.probas[quartile3] <- sapply(social.clusters.distime[1,quartile3], function(x) predict(glm3, newdata=data.frame(Dist=x), type="response"))
	social.clusters.probas[quartile4] <- sapply(social.clusters.distime[1,quartile4], function(x) predict(glm4, newdata=data.frame(Dist=x), type="response"))
	
	#Sampling clusters
	selected.cluster <- sample(1:length(social.clusters.probas), 1, prob= social.clusters.probas)
	social.clusters.probas[selected.cluster] <- 1
	social.clusters.sampled <- which(sapply(social.clusters.probas, function(x) sample(c(1,0), 1, prob=c(x,1-x))) == 1)
	
	#Sampling individuals within the selected clusters
	ind.attending <- vector()
	for(i in 1:length(social.clusters.sampled)){
		soc.clust <- social.clusters[[social.clusters.sampled[i]]]
		if(length(soc.clust) > 2){
			first.ind <- sample(soc.clust, 1)
			soc.clust <- soc.clust[-which(soc.clust == first.ind)]
			n = length(soc.clust)
			probas <- dbinom(0:n, n, proba.binom)
			number.ind.sampled <- which(probas == sample(probas, 1, prob=probas))-1
			ind.attending <- c(ind.attending, first.ind, sample(soc.clust, number.ind.sampled))
		}else if(length(soc.clust) == 2){
			first.ind <- sample(soc.clust, 1)
			soc.clust <- soc.clust[-which(soc.clust == first.ind)]
			n = length(soc.clust)
			probas <- dbinom(0:n, n, proba.binom)
			number.ind.sampled <- which(probas == sample(probas, 1, prob=probas))-1
			if(number.ind.sampled==1){
				ind.attending <- c(ind.attending, first.ind, soc.clust)
			}else{
				ind.attending <- c(ind.attending, first.ind)
			}
		}else{
			ind.attending <- c(ind.attending, soc.clust)
		}
	}
	group_by_individual_simulated[event, unlist(ind.attending)] <- 1
	
	#Update FI
	FI[which(group_by_individual_simulated[event,] == 1),] <- 0
	FI[which(group_by_individual_simulated[event,] == 1),which(feeders.id==event.infos[event,1])] <- 1
	
	#Set previous times of sampled individuals to 0
	previous.time.sim[which(group_by_individual_simulated[event,] == 1)] <- 0
	
}


group_by_individual_simulated = group_by_individual_simulated[1:event-1,]
event.locs <- group_data_timeorder[1:event-1,1]

###############################
#Spread of information

transmission <- function(ind, party){
	number.of.left <- length(which(party=="L"))
	number.of.right <- length(which(party=="R"))
	if(ind == "S"){
		proba.nothing <- exp(-(beta*(number.of.left + number.of.right) + 2*gamma) * T )
		proba.become.left <- (1-proba.nothing) * (beta*number.of.left + gamma) / (beta*(number.of.left + number.of.right) + gamma)
		proba.become.right <- (1-proba.nothing) * (beta*number.of.right + gamma) / (beta*(number.of.left + number.of.right) + gamma)
	}else if(ind == "L"){
		proba.nothing <- exp(-(beta*(number.of.right) + gamma) * T )
		proba.become.left <- 0
		proba.become.right <- 1 - proba.nothing
	}else{
		proba.nothing <- exp(-(beta*(number.of.left) + gamma) * T )
		proba.become.left <- 1 - proba.nothing
		proba.become.right <- 0
	}
	return(sample(c("S", "L", "R"), 1, prob=c(proba.nothing, proba.become.left, proba.become.right)))
}

#Parameters
beta=10
gamma=0.001
T=1

group_by_individual_simulated2 = group_by_individual_simulated
individual.status <- rep("S", length(previous.time.sim))
release.lefts <- c(which(group_data_timeorder[1:event-1,1] == feeders.id[11])[1], which(group_data_timeorder[1:event-1,1] == feeders.id[50])[1], which(group_data_timeorder[1:event-1,1] == feeders.id[59])[1])
release.rights <- c(which(group_data_timeorder[1:event-1,1] == feeders.id[3])[1], which(group_data_timeorder[1:event-1,1] == feeders.id[7])[1])
for(ev in 1:dim(group_by_individual_simulated)[1]){
	attending.individuals <- which(group_by_individual_simulated[ev,] == 1)
	if(ev == release.lefts && length(attending.individuals)>=2){
		individual.status[attending.individuals[sample(1:length(attending.individuals), 2, replace=F)]] <- "L"
	}
	if(ev == release.rights && length(attending.individuals)>=2){
		individual.status[attending.individuals[sample(1:length(attending.individuals), 2, replace=F)]] <- "R"
	}
	if(length(attending.individuals > 1)){
	 	new.status <- sapply(individual.status[attending.individuals], function(x) transmission(x,individual.status[attending.individuals][-1]))
		individual.status[attending.individuals] <- new.status
		group_by_individual_simulated2[ev,attending.individuals] <- new.status
	}
}

SLR.results <- list()
for(period in 1:3){
	SLR.res <- list()
	theperiod <- round(quantile(1:event, probs=seq(0,1,1/3))[c(period,period+1)])
	for(i in 1:length(feeders.id)){
		if(length(which(event.locs[theperiod[1]:theperiod[2]] == feeders.id[i])) > 1){
			SLR.res[[i]] <- apply(group_by_individual_simulated2[which(event.locs[theperiod[1]:theperiod[2]] == feeders.id[i]),], 1, function(x) c(length(which(x=="S")),length(which(x=="L")),length(which(x=="R"))))
			SLR.res[[i]] <- apply(SLR.res[[i]], 1, sum)
		}else if(length(which(event.locs[theperiod[1]:theperiod[2]] == feeders.id[i])) == 1){
			SLR.res[period][[i]] <- c(length(which(group_by_individual_simulated2[which(event.locs[theperiod[1]:theperiod[2]] == feeders.id[i]),]=="S")),length(which(group_by_individual_simulated2[which(event.locs[theperiod[1]:theperiod[2]] == feeders.id[i]),]=="L")),length(which(group_by_individual_simulated2[which(event.locs[theperiod[1]:theperiod[2]] == feeders.id[i]),]=="R")))
		}else{
			SLR.res[[i]] <- c(0,0,0)
		}
	}
	SLR.results[[period]] <- SLR.res
}	


	
#Plot the information spread

par(mfrow=c(1,3), mar=c(0.5,0.5,0.5,0.5), mgp=c(1.5,0.5,0))
for(period in 1:3){
	SLR.results.sum <- unlist(lapply(SLR.results[[period]], sum))
	plot(poly.owin, main="")
	for(i in 1:65){
		if(SLR.results.sum[i] > 0){
			add.pie(z= SLR.results[[period]][[i]], x=loggers_coords[i,"x"], y=loggers_coords[i,"y"], labels="", radius=SLR.results.sum[i], col=c("light grey", "red", "blue"))
		}
	}
}





















	
	
#####  OLD  #####




	###############################
	#Spread of information
	Pr.nothing.happens = (1-gamma) * exp(- beta * ((sum(individual.status[attending.inds] == "L") + sum(individual.status[attending.inds] == "R"))) / (sum(individual.status[attending.inds] == "L") + sum(individual.status[attending.inds] == "R") + sum(individual.status[attending.inds] == "S")))
	if(Pr.nothing.happens < (1-gamma)){
		Pr.becomes.L = ((sum(individual.status[attending.inds] == "L") / (sum(individual.status[attending.inds] == "L") + sum(individual.status[attending.inds] == "R"))) * (1-Pr.nothing.happens-gamma)) + (gamma/2)
		Pr.becomes.R = ((sum(individual.status[attending.inds] == "R") / (sum(individual.status[attending.inds] == "L") + sum(individual.status[attending.inds] == "R"))) * (1-Pr.nothing.happens-gamma)) + (gamma/2)
	}else{
		Pr.becomes.L = (gamma/2)
		Pr.becomes.R = (gamma/2)
	}
	for(a in length(attending.inds)){
		individual.status[attending.inds[a]] <- sample(c(individual.status[attending.inds[a]], "L", "R"), 1, prob=c(Pr.nothing.happens, Pr.becomes.L, Pr.becomes.R))
	}
	###############################
	


glm.predictions <- function(x){
	if(is.na(x[2])==F){
		if((x[2] > quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[1]) & (x[2] <= quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[2])){
			proba.pred = predict(glm1, newdata=data.frame(Dist=as.numeric(x[1])), type="response")
		}else if((x[2] > quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[2]) & (x[2] <= quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[3])){
			proba.pred = predict(glm2, newdata=data.frame(Dist=as.numeric(x[1])), type="response")
		}else if((x[2] > quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[3]) & (x[2] <= quantile(data.for.model[,3], probs=seq(0,1,0.25), na.rm=T)[4])){
			proba.pred = predict(glm3, newdata=data.frame(Dist=as.numeric(x[1])), type="response")
		}else{
			proba.pred = predict(glm4, newdata=data.frame(Dist=as.numeric(x[1])), type="response")
		}
	}else{
		proba.pred = NA
	}	
	return(proba.pred)
}


#Training the model (for social cohesion)
eee = 0
data_social <- list()
for(e in sample(10000:60000,300)){
	previous.location <- lapply(apply(group_by_individual_timeorder[1:(e-1),], 2, function(x) which(x==1)), function(x) x[length(x)])
	previous.location[which(lapply(previous.location,length) == 0)] = NA
	current.presabs <- group_by_individual_timeorder[e,]
	if(sum(current.presabs) > 2){
		previous.dist <- sapply(group_data_timeorder[unlist(previous.location),1], function(x) forest.distances[which(feeders.id == x), which(feeders.id == group_data_timeorder[e,1])])
		previous.dist[which(lapply(previous.dist,length) == 0)] = NA
		previous.time <- sapply(unlist(previous.location), function(x) group_data_timeorder[e,5] - group_data_timeorder[x,5])
		previous.time[which(lapply(previous.time,length) == 0)] = NA
		previous.dist.time <- rbind(previous.dist, previous.time)
		ind.selected <- sample(which(group_by_individual_timeorder[e,] == 1),1)
		currentPresabs <- current.presabs[-ind.selected]
		previousCoPresabs <- rep(0, length(group_by_individual_timeorder[e,]))
		previousCoPresabs[which(unlist(previous.location)==unlist(previous.location)[ind.selected])[-which(which(unlist(previous.location)==unlist(previous.location)[ind.selected]) == ind.selected)]] <- 1
		previousCoPresabs <- previousCoPresabs[-ind.selected]
		predicted_probas <- apply(previous.dist.time, 2, glm.predictions)[-ind.selected]
		data_social[[e]] <- cbind(predicted_probas, currentPresabs, previousCoPresabs)
	}
	eee = eee + 1
	print(eee)
}
data_social <- data_social[-which(lapply(data_social,length)==0)]
data_social.table <- do.call("rbind", data_social)

previous1_current1 <- length(which(data_social.table[,3]==1 & data_social.table[,2]==1))
previous1_current0 <- length(which(data_social.table[,3]==1 & data_social.table[,2]==0))
previous0_current1 <- length(which(data_social.table[,3]==0 & data_social.table[,2]==1))
previous0_current0 <- length(which(data_social.table[,3]==0 & data_social.table[,2]==0))
	
proba.ratio <- ((previous1_current1 / (previous1_current1 + previous1_current0)) / (previous0_current1 / (previous0_current1 + previous0_current0))) / (mean(data_social.table[which(data_social.table[,3]==1),1],na.rm=T) / mean(data_social.table[which(data_social.table[,3]==0),1], na.rm=T))


	
	
	
	
	



social.net <- matrix(0, nrow=length(previous.event.attended), ncol=length(previous.event.attended))
	for(i in 1:length(social.clusters)){
		social.net[i, social.clusters[[i]]] <- 1
	}
	diag(social.net) <- 0









proba.ratio <- vector()
for(i in 1:20){
	
	repeat{
		ev <- sample(test.range,1)
		ind.selected <- sample(which(group_by_individual_timeorder[ev,] == 1),1)
		
		lastPres <- unlist(sapply(1:length(group_by_individual_timeorder[ev,]), function(x) which(group_by_individual_timeorder[1:(ev-1),x]==1)[length(which(group_by_individual_timeorder[1:(ev-1),x]==1))]))
		PreviousPresAbs <- rep(0, length(group_by_individual_timeorder[ev,]))
		PreviousPresAbs[which(lastPres==lastPres[ind.selected])[-which(which(lastPres==lastPres[ind.selected]) == ind.selected)]] <- 1
		PreviousPresAbs <- PreviousPresAbs[-ind.selected]
		CurrentPresAbs <- group_by_individual_timeorder[ev,-ind.selected]
		if(sum(CurrentPresAbs) > 2 & sum(PreviousPresAbs) > 2){break}
	}
	
	previous1_current1 <- length(which(PreviousPresAbs==1 & CurrentPresAbs==1))
	previous1_current0 <- length(which(PreviousPresAbs==1 & CurrentPresAbs==0))
	previous0_current1 <- length(which(PreviousPresAbs==0 & CurrentPresAbs==1))
	previous0_current0 <- length(which(PreviousPresAbs==0 & CurrentPresAbs==0))

	current_probas <- apply(previous.dist.time.test[[ev-65999]], 2, glm.predictions)[-ind.selected]

	proba.ratio <- c(proba.ratio, ( (previous1_current1 / (previous1_current1 + previous1_current0)) / (previous0_current1 / (previous0_current1 + previous0_current0)) ) / ( mean(current_probas[which(PreviousPresAbs==1)]) / mean(current_probas[which(PreviousPresAbs==0)], na.rm=T) ))
	
}






previous.dist.time.test



lapply(previous.dist.time.test, function(x1) sapply(apply(x1,2,glm.predictions), function(x2) sample(c(1,0), 1, prob=c(x2, 1-x2))))

test.occurences <- lapply(previous.dist.time.test, function(x1) sapply(apply(x1,2,glm.predictions), function(x2) sample(c(1,0), 1, prob=c(x2, 1-x2))))

previous.dist.test

unlist(lapply(test.occurences, sum))
unlist(lapply(current.presabs.test, sum))







### Compute the summary statistics for the empirical data

mvts.dists.sum <- vector()
mvts.info <- list()
for(i in 1:dim(group_by_individual)[2]){
	i_e <- which(group_by_individual[,i]==1)
	group_data_ind <- group_data[i_e,][order(group_data[i_e,][,5]),]
	mvts.transitions <- rbind(group_data_ind[,1][-length(i_e)], group_data_ind[,1][-1])
	mvts.dists <- vector()
	for(j in 1:length(i_e)-1){
		mvts.dists[j] <- feeders.dists[which(feeders.id == mvts.transitions[1,j]), which(feeders.id == mvts.transitions[2,j])]
	}
	mvts.dists.sum[i] <- sum(mvts.dists)
	mvts.t <- rbind(group_data_ind[,5][-length(i_e)], group_data_ind[,5][-1])
	mvts.time <- mvts.t[2,] - mvts.t[1,]
	mvts.info[[i]] <- cbind(mvts.dists, mvts.time)
}

mvts.info.full <- do.call("rbind", mvts.info)
plot(mvts.info.full[,1], mvts.info.full[,2], pch=20)


## SS1: median distance travelled by an individual
mvts.dists.sum <- vector()
mvts <- list()
for(i in 1:dim(group_by_individual)[2]){
	#Foraging events attended by individual i
	i_e <- which(group_by_individual[,i]==1)
	#Detecting movement occurences for individual i
	mvt <- which( group_data[i_e,1][-length(i_e)] != group_data[i_e,1][-1] )
	if(length(mvt) > 0){
		mvts.transitions <- rbind(group_data[i_e,1][mvt], group_data[i_e,1][mvt+1])
		#Computing the total distance travelled by individual i
		mvts.dists <- vector()
		for(j in 1:length(mvt)){
			mvts.dists[j] <- feeders.dists[which(feeders.id == mvts.transitions[1,j]), which(feeders.id == mvts.transitions[2,j])]
		}
		mvts.dists.sum[i] <- sum(mvts.dists)
		mvts[[i]] <- c(mvts.dists, rep(0,length(i_e)-length(mvts.dists)))
	}else{
		mvts[[i]] <- rep(0,length(i_e))
		mvts.dists.sum[i] <- 0
	}
}
SS1.obs = mean(mvts.dists.sum)


## Social network 
intersection.ind <- matrix(0, ncol=dim(group_by_individual)[2], nrow=dim(group_by_individual)[2]) 
union.ind <- matrix(0, ncol=dim(group_by_individual)[2], nrow=dim(group_by_individual)[2]) 
#overlap.between.inds.obs <- matrix(0, ncol=dim(group_by_individual)[2], nrow=dim(group_by_individual)[2]) 
for(i in 1:dim(group_by_individual)[2]){
	intersection.ind[i,(i+1):dim(group_by_individual)[2]] <- apply(group_by_individual[,i] * group_by_individual[,(i+1):dim(group_by_individual)[2]], 2, sum)
	union.ind[i,(i+1):dim(group_by_individual)[2]] <- apply(group_by_individual[,i] + group_by_individual[,(i+1):dim(group_by_individual)[2]], 2, sum)
}
overlap.between.inds.obs <- intersection.ind / (union.ind - intersection.ind)
overlap.between.inds.obs <- overlap.between.inds.obs + t(overlap.between.inds.obs)
g <- graph.adjacency(overlap.between.inds.obs, weighted=T)
net <- get.data.frame(g)

## SS2: Clustering coefficient of the social network (tendency of individuals to cluster together into tightly knit groups)
SS2.obs <- clustering_w(net, measure="am")

## SS3: Mean weigthed vertex degree (sum of the friendship strenght [weights] of the direct friendships for each individual)
SS3.obs <- mean(strength(g))




##  The model  ##


#Parameters
a = 1
b = -0.025
c = 0.5

#alpha = -0.01  # 0.014 # 0.017   # Related to the movement of birds 
#beta = 1 # 0.5  # Related to the social preferential attachment 
#beta2 = 1   # Related to the infection rate    
#gamma = 0.01  # Spontaneous learning rate


#Initiation: match every individual to a feeder
inds = 1:729  # individuals
feeders = 1:length(feeders.id) # feeders
inds.feeder = round((Nb.unique.ind.per.feeder / sum(Nb.unique.ind.per.feeder)) * 729)
N = sum(inds.feeder) # total number of individuals
events.inds.table = matrix(0, ncol=N, nrow=dim(group_data)[1])  # Table of 1/0 with feeding event in row (in simulation order) and individual in column
FI.state.table = matrix(0, ncol=length(feeders), nrow=N)  # Table of number of feeding events previously attended by each individual at each feeder
social.network = matrix(0, ncol=N, nrow=N)
#Intiate the state matrix (FI)
inds2 <- 1:N
for(i in 1:length(inds.feeder)){
	ind.sampled <- sample(inds2, inds.feeder[i])
	FI.state.table[ind.sampled, i] <- 1
	inds2 <- inds2[-match(ind.sampled, inds2)]
}



#Run simulation

event.location <- vector()
for(event in 1:dim(group_data)[1]){
	
	#Select the location of the foraging event (based on the number of individuals at feeders)
	event.location[event] <- sample(1:length(feeders), 1, prob=(relationship.NBinds.NBevents[1] + (apply(FI.state.table, 2, sum) * relationship.NBinds.NBevents[2])) / sum(relationship.NBinds.NBevents[1] + (apply(FI.state.table, 2, sum) * relationship.NBinds.NBevents[2])))

	#Select a first individual, IND1 (which is currently at the event location)
	first.ind <- sample(which(apply(FI.state.table, 1, function(x) feeders.dists[event.location[event],which(x==1)]) == min(apply(FI.state.table, 1, function(x) feeders.dists[event.location[event],which(x==1)]))), 1)

	#Select individuals who attend the event with IND1.
	Pie = exp( - a + (b*apply(FI.state.table, 1, function(x) feeders.dists[event.location[event],which(x==1)])) + (a*c*social.network[first.ind,]) )   # Probability that individual i attends the event e
	inds.selected = sapply(Pie, function(x) sample(c(1,0), 1, prob=c(x,1-x)))
	inds.selected[first.ind] = 1
	
	#Update tables
	events.inds.table[event,] <- inds.selected
	FI.state.table[which(inds.selected==1),event.location[event]] <- 1
	FI.state.table[which(inds.selected==1),-event.location[event]] <- 0	
	social.network[which(inds.selected==1), which(inds.selected==1)] <- 1
	social.network[which(inds.selected==1), -which(inds.selected==1)] <- 0
	social.network[-which(inds.selected==1), which(inds.selected==1)] <- 0
}





### Compute the summary statistics for the simulated data

## SS1: median distance travelled by an individual
mvts.dists.sum <- vector()
for(i in 1:dim(events.inds.table)[2]){
	#Foraging events attended by individual i
	i_e <- which(events.inds.table[,i]==1)
	#Detecting movement occurences for individual i
	mvts <- which( event.location[i_e][-length(i_e)] != event.location[i_e][-1] )	
	if(length(mvts) > 0){
		mvts.transitions <- rbind(event.location[i_e][mvts], event.location[i_e][mvts+1])
		#Computing the total distance travelled by individual i
		mvts.dists <- vector()
		for(j in 1:length(mvts)){
			mvts.dists[j] <- feeders.dists[mvts.transitions[1,j], mvts.transitions[2,j]]
		}
		mvts.dists.sum[i] <- sum(mvts.dists)
	}else{
		mvts.dists.sum[i] <- 0
	}
}
SS1.sim = mean(mvts.dists.sum)

## Social network 
overlap.between.inds.obs <- matrix(0, ncol=dim(group_by_individual)[2], nrow=dim(group_by_individual)[2]) 
for(i in 1:dim(group_by_individual)[2]){
	for(j in 1:dim(group_by_individual)[2]){
		if(j>i){
			intersection.ind <- sum(group_by_individual[,i] * group_by_individual[,j])
			union.ind <- sum(group_by_individual[,i] + group_by_individual[,j])
			overlap.between.inds.obs[i,j] <- intersection.ind / (union.ind - intersection.ind)
		}
	}
}
overlap.between.inds.obs <- overlap.between.inds.obs + t(overlap.between.inds.obs)
g <- graph.adjacency(overlap.between.inds.obs, weighted=T)
net <- get.data.frame(g)

## SS2: Clustering coefficient of the social network (tendency of individuals to cluster together into tightly knit groups)
SS2.obs <- clustering_w(net, measure="am")

## SS3: Mean weigthed vertex degree (sum of the friendship strenght [weights] of the direct friendships for each individual)
SS3.obs <- mean(strength(g))










#Modelling the frequency of foraging events at each feeder over the simulation period (1 week-end = 1350 minutes, and there are 14 week-ends)
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
#events.table[,3] <- rep(0, length(events.table[,1]))
#colnames(events.table)[3] <- "Duration"
#for(i in 1:length(events.table$Duration)){
#	events.table[i,3] <- sample(feeding.duration.obs[which(Nb.inds.per.event==events.table[i,2])], 1)
#}


#Parameters
alpha = 0.01  # 0.014 # 0.017   # Related to the movement of birds 
beta = 1 # 0.5  # Related to the social preferential attachment 
#beta2 = 1   # Related to the infection rate    
#gamma = 0.01  # Spontaneous learning rate


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
#individual.status <- rep("S", N)

#Release some Ls and some Rs
#individual.status[sample(which(FI[,3] == 1), 2)] <- "R"
#individual.status[sample(which(FI[,7] == 1), 2)] <- "R"
#individual.status[sample(which(FI[,11] == 1), 2)] <- "L"
#individual.status[sample(which(FI[,50] == 1), 2)] <- "L"
#individual.status[sample(which(FI[,59] == 1), 2)] <- "L"


#Weighted social network
intersection.between.inds <- matrix(0, ncol=dim(EI)[2], nrow=dim(EI)[2]) 
for(i in 1:length(FI[1,])){
	intersection.between.inds[which(FI[,i]==1),which(FI[,i]==1)] = intersection.between.inds[which(FI[,i]==1),which(FI[,i]==1)] + 1
}
diag(intersection.between.inds) <- 0	
overlap.between.inds <- matrix(0, ncol=dim(EI)[2], nrow=dim(EI)[2]) 
for(i in 1:length(FI[,1])){
	overlap.between.inds[i,] <- intersection.between.inds[i,] / apply(FI, 1, sum)[i]
}

SS1.sim <- vector()
SS2.sim <- vector()
SS3.sim <- vector()
# For each feeding event e, compute the probability of each individual i being sampled to attend
for(event in 1:dim(events.table)[1]){
	
	f = events.table[event,1]  # at which feeder does the event take place
	
	attending.inds <- vector()
	for(k in 1:events.table[event,2]){
		Pie <- vector()
		if(k==1){
			for(i in 1:N){
				Pie[i] = sum(exp( - alpha * sum(FI[i,]*feeders.dists[f,]) / sum(FI[i,]) ))
			}
			Pie = Pie / sum(Pie)
		}else{
			for(i in 1:N){
				Pie[i] = sum(exp( - ( (alpha * sum(FI[i,]*feeders.dists[f,]) / sum(FI[i,])) + (beta/(1+sum(overlap.between.inds[i,attending.inds]))) ) ))
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
	
	#Updating the weighted social network
	intersection.between.inds[attending.inds, attending.inds] <- intersection.between.inds[attending.inds, attending.inds] + 1
	for(i in 1:length(attending.inds)){
		overlap.between.inds[attending.inds[i],] <- intersection.between.inds[attending.inds[i],] / apply(FI, 1, sum)[attending.inds[i]]
	}
	diag(overlap.between.inds) <- 0	
	#for(i in 1:length(attending.inds)){
	#	for(j in 1:N){
	#		overlap.between.inds[attending.inds[i], j] <- intersection.between.inds[attending.inds[i], j] / (intersection.between.inds[attending.inds[i], attending.inds[i]] + intersection.between.inds[j, j] - intersection.between.inds[attending.inds[i], j])
	#		overlap.between.inds[j, attending.inds[i]] <- intersection.between.inds[j, attending.inds[i]] / (intersection.between.inds[j, j] + intersection.between.inds[attending.inds[i], attending.inds[i]] - intersection.between.inds[j, attending.inds[i]])
	#	}
	#}
	#diag(overlap.between.inds) <- 0	
		
	if(is.element(event, c(seq(round(64678/5),64678,round(64678/5)), 64678))==T){
		
		## SS1: median distance travelled by an individual
		mvts.dists.sum <- vector()
		for(i in 1:dim(EI)[2]){
			#Foraging events attended by individual i
			i_e <- which(EI[,i]==1)
			#Detecting movement occurences for individual i		
			mvts <- which( events.table$Feeder[i_e][-length(i_e)] != events.table$Feeder[i_e][-1] )
			if(length(mvts) > 0){
				mvts.transitions <- rbind(events.table$Feeder[i_e][mvts], events.table$Feeder[i_e][mvts+1])
				#Computing the total distance travelled by individual i
				mvts.dists <- vector()
				for(j in 1:length(mvts)){
					mvts.dists[j] <- feeders.dists[mvts.transitions[1,j],mvts.transitions[2,j]]
				}
				mvts.dists.sum[i] <- sum(mvts.dists)
			}else{
				mvts.dists.sum[i] <- 0
			}
		}
		SS1.sim[event] = mean(mvts.dists.sum)
		
		
		g <- graph.adjacency(overlap.between.inds, weighted=T)
		net <- get.data.frame(g)
		
		## SS2: Clustering coefficient of the social network (tendency of individuals to cluster together into tightly knit groups)
		SS2.sim[event] <- clustering_w(net, measure="am")
		
		## SS3: Mean weigthed vertex degree (sum of the friendship strenght [weights] of the direct friendships for each individual)
		SS3.sim[event] <- mean(strength(g))
		
		
		
		
		
		
		
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
		summary.statistic.spatial.sim[event] <- (sum(apply(feeders.dists * overlap.between.feeders, 1, sum)) / 2) / (sum(apply(overlap.between.feeders, 1, sum)) / 2)
	}
		
}



plot(1:length(summary.statistic.spatial.sim), summary.statistic.spatial.sim, pch=20, ylim=c(0,1000))
plot(1:length(summary.statistic.social1.sim), summary.statistic.social1.sim, pch=20, ylim=c(0,1))
plot(1:length(summary.statistic.social2.sim), summary.statistic.social2.sim, pch=20, ylim=c(0,20))















#summary.statistic.social1.sim <- vector()
#summary.statistic.social2.sim <- vector()
#intersection.between.inds <- matrix(0, ncol=dim(EI)[2], nrow=dim(EI)[2]) 
#overlap.between.inds <- matrix(0, ncol=dim(EI)[2], nrow=dim(EI)[2]) 
# For each feeding event e, compute the probability of each individual i being sampled to attend
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
				
		attending.inds[k] <- sample(1:N, 1, prob=Pie)	
	}
	FI[attending.inds, f] <- FI[attending.inds, f] + 1
	
	EI[event,attending.inds] <- 1 #individual.status[attending.inds]
		
}

#Weighted social network
intersection.between.inds[attending.inds, attending.inds] <- intersection.between.inds[attending.inds, attending.inds] + 1
for(i in 1:length(attending.inds)){
	for(j in 1:N){
		overlap.between.inds[attending.inds[i], j] <- intersection.between.inds[attending.inds[i], j] / (intersection.between.inds[attending.inds[i], attending.inds[i]] + intersection.between.inds[j, j] - intersection.between.inds[attending.inds[i], j])
		overlap.between.inds[j, attending.inds[i]] <- intersection.between.inds[j, attending.inds[i]] / (intersection.between.inds[j, j] + intersection.between.inds[attending.inds[i], attending.inds[i]] - intersection.between.inds[j, attending.inds[i]])
	}
}
diag(overlap.between.inds) <- 0	
g <- graph.adjacency(overlap.between.inds, weighted=T)
net <- get.data.frame(g)
summary.statistic.social1.sim[event] <- clustering_w(net, measure="am")
summary.statistic.social2.sim[event] <- mean(strength(g))




# Spatial network
contact = 1
weighted.spatial.graph.sim <- list()
summary.statistic.social1.sim <- vector()
summary.statistic.social2.sim <- vector()
summary.statistic.spatial.sim <- vector()
for(w in 1:14){
	group_by_individual_weekend <- EI[contact:(contact+weekend.number.of.events[w]),]
	
#Weighted spatial network
event.location.weekend <- events.table[contact:(contact+weekend.number.of.events[w]),1]


inds.per.feeder.sim <- list()
for(i in 1:length(feeders)){
	indss <- vector()
	if(length(which(events.table[,1] == i)) > 0){
		for(j in 1:length(which(events.table[,1] == i))){
			indss <- c(indss, which(EI[which(events.table[,1] == i),][j,] != "0"))
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
weighted.spatial.graph.sim[[w]] <- overlap.between.feeders
summary.statistic.spatial.sim <- (sum(apply(feeders.dists * overlap.between.feeders, 1, sum)) / 2) / (sum(apply(overlap.between.feeders, 1, sum)) / 2)

	
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
	summary.statistic.social1.sim[w] <- clustering_w(net, measure="am")
	summary.statistic.social2.sim[w] <- mean(strength(g))
	
	contact = contact + weekend.number.of.events[w]	
}
sumstat.spatial.sim = mean(summary.statistic.spatial.sim)
sumstat.social1.sim = mean(summary.statistic.social1.sim) 
sumstat.social2.sim = mean(summary.statistic.social2.obs) 














	for(i in 1:length(attending.inds)){
		for(j in 1:length(attending.inds)){
			overlap.between.inds[attending.inds[i], attending.inds[j]] <- intersection.between.inds[attending.inds[i], attending.inds[j]] /(intersection.between.inds[attending.inds[i], attending.inds[i]] + intersection.between.inds[attending.inds[j], attending.inds[j]] - intersection.between.inds[attending.inds[i], attending.inds[j]])
		}
	}


#Weighted social network	
	ind.number.of.events <- as.vector(apply(group_by_individual_weekend, 2, function(x) length(which(x != "0"))))
	
	intersection.between.inds <- matrix(0, ncol=dim(EI)[2], nrow=dim(EI)[2]) 
	for(i in 1:dim(EI)[2]){
		if(length(which(group_by_individual_weekend[,i] != "0")) > 1){
			
			intersection.between.inds[i,] <- 
			
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
	summary.statistic.social1.sim[event] <- clustering_w(net, measure="am")
	summary.statistic.social2.sim[event] <- mean(strength(g))
	




###############################
#Spread of information
	Pr.nothing.happens = (1-gamma) * exp(- beta * ((sum(individual.status[attending.inds] == "L") + sum(individual.status[attending.inds] == "R"))) / (sum(individual.status[attending.inds] == "L") + sum(individual.status[attending.inds] == "R") + sum(individual.status[attending.inds] == "S")))
	if(Pr.nothing.happens < (1-gamma)){
		Pr.becomes.L = ((sum(individual.status[attending.inds] == "L") / (sum(individual.status[attending.inds] == "L") + sum(individual.status[attending.inds] == "R"))) * (1-Pr.nothing.happens-gamma)) + (gamma/2)
		Pr.becomes.R = ((sum(individual.status[attending.inds] == "R") / (sum(individual.status[attending.inds] == "L") + sum(individual.status[attending.inds] == "R"))) * (1-Pr.nothing.happens-gamma)) + (gamma/2)
	}else{
		Pr.becomes.L = (gamma/2)
		Pr.becomes.R = (gamma/2)
	}
	for(a in length(attending.inds)){
		individual.status[attending.inds[a]] <- sample(c(individual.status[attending.inds[a]], "L", "R"), 1, prob=c(Pr.nothing.happens, Pr.becomes.L, Pr.becomes.R))
	}
###############################






############      ANALYSE RESULTS - MODEL OUTPUTS



# Spatial network
contact = 1
weighted.spatial.graph.sim <- list()
summary.statistic.social1.sim <- vector()
summary.statistic.social2.sim <- vector()
summary.statistic.spatial.sim <- vector()
for(w in 1:14){
	group_by_individual_weekend <- EI[contact:(contact+weekend.number.of.events[w]),]
	
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
	weighted.spatial.graph.sim[[w]] <- overlap.between.feeders
	summary.statistic.spatial.sim[w] <- (sum(apply(feeders.dists * overlap.between.feeders, 1, sum)) / 2) / (sum(apply(overlap.between.feeders, 1, sum)) / 2)
	
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
	summary.statistic.social1.sim[w] <- clustering_w(net, measure="am")
	summary.statistic.social2.sim[w] <- mean(strength(g))
	
	contact = contact + weekend.number.of.events[w]	
}
sumstat.spatial.sim = mean(summary.statistic.spatial.sim)
sumstat.social1.sim = mean(summary.statistic.social1.sim) 
sumstat.social2.sim = mean(summary.statistic.social2.obs) 










plot(feeders.dists, weighted.spatial.graph.obs[[1]], pch=20, ylim=c(0,1))
for(i in 2:14){
	points(feeders.dists, weighted.spatial.graph.obs[[i]], pch=20)
}
points(feeders.dists, weighted.spatial.graph.sim[[1]], pch=20, ylim=c(0,1), cex=0.5)
for(i in 2:14){
	points(feeders.dists, weighted.spatial.graph.sim[[i]], pch=20, cex=0.5)
}


dist.mat <- vector()
for(i in 1:13){
	dist.mat[i] <- sum(abs(weighted.spatial.graph.sim[[i]] - weighted.spatial.graph.obs[[i]]))
}




# Social network


################################################














##  Analyse model outputs  ##

contact = 1
summary.statistic.social.sim <- vector()
summary.statistic.spatial.sim <- vector()
weighted.spatial.graph.sim <- list
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
	weighted.spatial.graph.sim[[w]] <- overlap.between.feeders
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








#Get the number of feeding events per day and per week-end
daily.number.of.events <- vector()
for(i in 1:length(unique(group_data$Date))){
	daily.number.of.events[i] <- length(which(group_data$Date == unique(group_data$Date)[i]))
}
weekend.number.of.events <- daily.number.of.events[seq(1, 28, 2)] + daily.number.of.events[seq(2, 28, 2)]
weekend.id <- c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14)





# inds per feeder
inds.per.feeder <- list()
for(i in 1:length(feeders.id)){
	indss <- vector()
	if(length(group_by_individual[which(group_data[,1] == feeders.id[i]),1]) > 0){
		for(j in 1:length(group_by_individual[which(group_data[,1] == feeders.id[i]),1])){
			indss <- c(indss, which(group_by_individual[which(group_data[,1] == feeders.id[i]),][j,] == 1))
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
	weighted.spatial.graph.obs[[w]] <- overlap.between.feeders
	summary.statistic.spatial.obs[w] <- (sum(apply(feeders.dists * overlap.between.feeders, 1, sum)) / 2) / (sum(apply(overlap.between.feeders, 1, sum)) / 2)
	
	

weighted.spatial.graph.obs <- list()
summary.statistic.spatial.obs <- vector()
for(w in 1:14){
	dates <- unique(group_data$Date)[which(weekend.id == w)]
	group_data_weekend <- group_data[which(group_data$Date == dates[1] | group_data$Date == dates[2]),]
	group_by_individual_weekend <- group_by_individual[which(group_data$Date == dates[1] | group_data$Date == dates[2]),]	
	
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
	weighted.spatial.graph.obs[[w]] <- overlap.between.feeders
	summary.statistic.spatial.obs[w] <- (sum(apply(feeders.dists * overlap.between.feeders, 1, sum)) / 2) / (sum(apply(overlap.between.feeders, 1, sum)) / 2)
}
sumstat.spatial.obs = mean(summary.statistic.spatial.obs)  # Average weigthed distance travelled







summary.statistic.social1.obs <- vector()
summary.statistic.social2.obs <- vector()
for(w in 1:14){
	dates <- unique(group_data$Date)[which(weekend.id == w)]
	group_data_weekend <- group_data[which(group_data$Date == dates[1] | group_data$Date == dates[2]),]
	group_by_individual_weekend <- group_by_individual[which(group_data$Date == dates[1] | group_data$Date == dates[2]),]	
	#weighted social network	
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
	summary.statistic.social1.obs[w] <- clustering_w(net, measure="am")
	summary.statistic.social2.obs[w] <- mean(strength(g))
}
g <- graph.adjacency(overlap.between.inds, weighted=T)
net <- get.data.frame(g)
## SS2: Clustering coefficient of the social network
SS2.obs <- clustering_w(net, measure="am")
## SS3: Mean weigthed vertex degree
SS3.obs <- mean(strength(g))



