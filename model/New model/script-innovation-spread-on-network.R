
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
library(MASS)


setwd("/Users/mariussomveille/Desktop/Oxford/Project_Ben_Robin/Wytham-tits-information-flow")

# Load data

load("Group_by_individual_all_winter_2013.RData")
load("Group_by_individual_data_all_winter_2013.RData")

loggers_coords <- read.csv("Wytham_loggers_coordinates.csv")

wyt <-readOGR("/Users/mariussomveille/Desktop/Oxford/Project_Ben_Robin/Wytham-tits-information-flow", "perimeter poly with clearings_region")
poly.sp<-SpatialPolygons(list(wyt@polygons[[1]]))
poly.owin<-as(poly.sp,"owin")


# Change site label to match loggers coordinates

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

group_by_individual = group_by_individual[do.call(order, group_data),]
group_data = group_data[do.call(order, group_data),]


Nb.contacts.per.ind <- apply(group_by_individual, 2, sum)
Nb.ind.per.contacts <- apply(group_by_individual, 1, sum)

Nb.patches.per.ind <- vector()
for(i in 1:dim(group_by_individual)[2]){
	Nb.patches.per.ind[i] <- length(unique(group_data[which(group_by_individual[,i] == 1),1]))
}

## Number of contact per patch
Nb.contacts.per.patch <- vector()
for(i in 1:length(unique(group_data$Location))){
	Nb.contacts.per.patch[i] <- length(which(group_data$Location == unique(group_data$Location)[i]))
}
Nb.contacts.per.patch <- data.frame(unique(group_data$Location), as.numeric(Nb.contacts.per.patch))
#Nb.contacts.per.patch = Nb.contacts.per.patch[do.call(order, Nb.contacts.per.patch),]


## Number of individual per contact in each patch
Nb.ind.per.contact.per.patch <- list()
for(i in 1:length(unique(group_data$Location))){
	Nb.ind.per.contact.per.patch[[i]] <- Nb.ind.per.contacts[which(group_data$Location == unique(group_data$Location)[i])]
}
hist(unlist(lapply(Nb.ind.per.contact.per.patch, mean)))


## Proportion of contact spent per patch for each individual (1)
Prop.patches.per.ind <- list()
for(i in 1:dim(group_by_individual)[2]){
	aaa <- vector()
	for(j in 1:length(unique(group_data$Location[which(group_by_individual[,i] == 1)]))){
		aaa[j] <- length(which(group_data$Location[which(group_by_individual[,i] == 1)] == unique(group_data$Location[which(group_by_individual[,i] == 1)])[j])) / length(group_data$Location[which(group_by_individual[,i] == 1)])
}
Prop.patches.per.ind[[i]] <- aaa
}

hist(unlist(lapply(Prop.patches.per.ind, max)), xlim=c(0,1))


## Proportion of contact spent with other individuals for each individual (2)
Prop.ind.per.ind <- list() #matrix(ncol=dim(group_by_individual)[2], nrow=dim(group_by_individual)[2])
for(i in 1:dim(group_by_individual)[2]){
	if(length(which(group_by_individual[,i] == 1)) > 1){
		Prop.ind.per.ind[[i]] <- apply(group_by_individual[which(group_by_individual[,i] == 1),], 2, sum) / length(which(group_by_individual[,i] == 1))	
	}else{
		Prop.ind.per.ind[[i]] <- rep(0,dim(group_by_individual)[2])
		Prop.ind.per.ind[[i]][i] <- 1
		}
}

hist(unlist(lapply(Prop.ind.per.ind, sum)) / unlist(lapply(Prop.ind.per.ind, function(x) length(which(x>0)))))


## Is there a correlation between (1) and (2) and patches id (i.e. do individuals who move more have less close friends? And is there an effect of the spatial location on this relationship?)






plot(poly.owin)
for(i in 1:65){
	add.pie(z=model_res_list[[i]], x=loggers_coords[i,"x"], y=loggers_coords[i,"y"], labels="", radius=Nb.contacts.per.patch[i,2]/15, col=c("light grey", "red", "blue"))
}






### THE MODEL

# Modelling the frequency and "size" of contact events at each feeder

# 1 weekend = 1350 minutes, and there are 14 week-ends

contact.events <- list()
for(i in 1:length(Nb.contacts.per.patch[,1])){
	contact.events[[i]] <- unique(round(runif(Nb.contacts.per.patch[i,2], 0, 1350*14)))
}
ind.per.contact <- list()
for(i in 1:length(contact.events)){
	ipc <- rep(0,length(contact.events[[i]]))
	for(j in 1:length(contact.events[[i]])){
		while(ipc[j] < 1){
			ipc[j] <- rpois(1, fitdistr(Nb.ind.per.contact.per.patch[[i]], "Poisson")$estimate)
		}
	}
	ind.per.contact[[i]] <- ipc
}

contact.events.feeder <- vector()
contact.events.numberindividuals <- vector()
for(i in 1:max(unlist(contact.events))){
	events.locations <- unlist(lapply(contact.events, function(x) match(i, x)))
	
	if(length(which(events.locations > 0)) > 1){
		events.feeder <- sample(which(events.locations > 0))
	}else{
		events.feeder <- which(events.locations > 0)
	}
	
	if(length(events.feeder > 0)){
		events.numberindividuals <- vector()
		for(j in 1:length(events.feeder)){
			events.numberindividuals[j] <- ind.per.contact[[events.feeder[j]]][events.locations[events.feeder[j]]]
		}
		contact.events.feeder <- c(contact.events.feeder, events.feeder)
		contact.events.numberindividuals <- c(contact.events.numberindividuals, events.numberindividuals)
	}
}
contact.events.table <- data.frame(contact.events.feeder, contact.events.numberindividuals)
colnames(contact.events.table) <- c("Feeder", "No of individuals")

# unique(group_data$Location)[contact.events.table[,1]]






# Run the model


N = 729  # total number of individuals 
P = 65  # total number of patches

FI = matrix(0, ncol=P, nrow=N)  # Matrix of number of events previously attended by each individual in each patch

CI = matrix(0, ncol=N, nrow=N)  # Matrix of number of contact that each individual had with each other individual

D = as.matrix(dist(loggers_coords[,2:3], upper=T, diag=T)) # Matrix of distances between each pair of patches


a = 4
b = 0.1
c = 1

# For each event Ept (occuring in patch p at time t), compute the probability of each individual i being sampled to attend
attending.individuals <- list()
for(event in 1:dim(contact.events.table)[1]){
	
	p = contact.events.table[event,1]  # in which patch does the event occur
	
	weights.ip <- vector()
	for(i in 1:N){
		weights.ip[i] = ((1 + FI[i,p])^a) / (1 + sum(FI[i,] * (b*D[p,])))
	}
	
	attending.ind <- vector()
	for(ind in 1:contact.events.table[event,2]){
		weights.ij <- vector()
		for(i in 1:N){
			weight.ij = 0
			if(length(attending.ind) > 0){
				for(j in 1:length(attending.ind)){ 
					weight.ij = weight.ij + CI[i,j]
				}
			}
			weights.ij[i] = weight.ij
		}
		PiEpt = weights.ip + (c * weights.ij)  
		PiEpt = PiEpt / sum(PiEpt)
		PiEpt[attending.ind] = 0	
		attending.ind[ind] <- sample(1:N, 1, prob=PiEpt)	
	}

	attending.individuals[[event]] <- attending.ind
	
	FI[attending.ind, p] <- FI[attending.ind, p] + 1
	CI[attending.ind, attending.ind] <- CI[attending.ind, attending.ind] + 1
	diag(CI) <- 0
}




## Comparing the observed networks with the simulated ones

### Bipartite individuals-patches graph
# Observed
Nb.patches.per.ind <- vector()
for(i in 1:dim(group_by_individual)[2]){
	Nb.patches.per.ind[i] <- length(unique(group_data[which(group_by_individual[,i] == 1),1]))
}
k.obs = 1:max(Nb.patches.per.ind)
Pk.obs = sapply(k.obs, function(x) length(which(Nb.patches.per.ind == x)))
Pk.obs = Pk.obs / sum(Pk.obs)
plot(log(k.obs,base=10), log(Pk.obs,base=10), col="black", pch=20)
# Simulated
Nb.patches.per.ind.simu = apply(FI, 1, function(x) length(which(x>0)))
k.sim = 1:max(Nb.patches.per.ind.simu)
Pk.sim = sapply(k.sim, function(x) length(which(Nb.patches.per.ind.simu == x)))
Pk.sim = Pk.sim / sum(Pk.sim)
points(log(k.sim,base=10), log(Pk.sim,base=10), col="red", pch=20)

#Pk.sim.powerlaw = k.sim^-2
#lines(log(k.sim,base=10), log(Pk.sim.powerlaw,base=10), col="red")


### Social network
# Observed
Nb.friends.per.ind <- vector()
for(i in 1:dim(group_by_individual)[2]){
	if(length(which(group_by_individual[,i] == 1)) > 1){
		Nb.friends.per.ind[i] = length(which(apply(group_by_individual[which(group_by_individual[,i] == 1),-i], 2, sum) > 0))
	}else{
		Nb.friends.per.ind[i] = 0
	}
}

k.obs = 1:max(Nb.patches.per.ind)
Pk.obs = sapply(k.obs, function(x) length(which(Nb.patches.per.ind == x)))
Pk.obs = Pk.obs / sum(Pk.obs)
plot(log(k.obs,base=10), log(Pk.obs,base=10), col="black", pch=20)
# Simulated
Nb.friends.per.ind.simu = apply(CI, 1, function(x) length(which(x>0)))
k.sim = 1:max(Nb.friends.per.ind.simu)
Pk.sim = sapply(k.sim, function(x) length(which(Nb.friends.per.ind.simu == x)))
Pk.sim = Pk.sim / sum(Pk.sim)
points(log(k.sim,base=10), log(Pk.sim,base=10), col="red", pch=20)



# Spatial network



















# old

attending.individuals <- list()
for(event in 1:dim(contact.events.table)[1]){
	
	p = contact.events.table[event,1]  # in which patch does the event occur
	
	weights.ip <- vector()
	for(i in 1:N){
		weight.ip = 0
		for(k in 1:P){
			#if(sum(FI[i,]) > 0){
				weight.ip = weight.ip + ( ((1+FI[i,k])^b)/(1+sum(FI[i,]))) / (1+(c*D[p,k])) 
			#}
		}
		weights.ip[i] = weight.ip
	}
	

	attending.ind <- vector()
	for(ind in 1:contact.events.table[event,2]){
		
		# compute the probability of attending the event for each individual
		
		weights.ij <- vector()
		for(i in 1:N){
			weight.ij = 0
			if(length(attending.ind) > 0){
				for(j in 1:length(attending.ind)){ 
					if(sum(CI[i,]) > 0){
						weight.ij = weight.ij + (CI[i,j]/sum(CI[i,]))
					}
				}
			}
			weights.ij[i] = weight.ij
		}
		
		PiEpt = exp(-( (a/weights.ip) + (d/(1+weights.ij)) ))
		PiEpt = PiEpt / sum(PiEpt)
		
		attending.ind[ind] <- sample(1:N, 1, prob=PiEpt)	
	}
	
	attending.individuals[[event]] <- attending.ind
	
	FI[attending.ind, p] <- FI[attending.ind, p] + 1
	CI[attending.ind, attending.ind] <- CI[attending.ind, attending.ind] + 1
	diag(CI) <- 0
	
}








