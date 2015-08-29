library(nsga2R)
source('log.R')
nspso <- function(fitFunc, varcnt, objDim, lowerbound, upperbound, c1, c2, maxgen, popsize, seed, problem, logS){

	#parameters settings
	set.seed(seed)
	varNo <- varcnt
	popSize <- popsize
	w <- 1
	velocity <- matrix(rep(1, varNo * popSize), nrow = popSize)
	pBest <- matrix(rep(1, (varNo+4) * popSize), nrow = popSize)
	front <- vector()
	fitFunctions <- fitFunc
	tourSize <- 2
	#===================Component Functions Start========================================
	generate_population <- function(varNo, size, initialisation = F){
		pop <- vector()
		count <- 0
	
		#initialisation
		#if(initialisation == T){
			#size <- size - 1
			#low_cost_particle <- as.vector(search_minimum_cost(noService, noCandLoc))
			#pop <- rbind(pop, low_cost_particle)
		#}

		repeat {
			if(count > size - 1){
				break
			}
			particle <- rbinom(varNo, 1, 0.5)
			pop <- rbind(pop, particle)
			count <- count + 1
		}
		return(pop)
	}

	init_velocity <- function(varNo, size){
		for(i in 1:size){
			for(j in 1:varNo){
				velocity[i, j] <<- runif(1, 0, 1)
			}
		}
	}



	evaluate <- function(pop, fitFunctions){
		tempFitness <- vector()
		for(i in 1:nrow(pop)){
			tempFitness <- rbind(tempFitness, fitFunctions(pop[i, ]))
		}
		colnames(tempFitness) <- c('costF', 'latencyF')
		pop <- cbind(pop, tempFitness)
		pop
	}

	updateParticle <- function(particle) {

	}

	transformation <- function(v) {
		return(1/(1 + exp(-v)))
	}
	#===================Component Functions End========================================
	
	#------plot function Start-----------
	plotme <- function(fitness){
		plot(fitness, xlim = c(0, 1), ylim = c(0, 1), col = 'red')
		par(new = T)
	}
	#------plot function End-----------

	#-------------------Algorithm Start--------------------------------
	pop <- generate_population(varNo, popSize, initialisation)
	init_velocity(varNo, popSize)

	for(iter in 1:maxgen){
		pop <- evaluate(pop, fitFunctions)
		ranking <- fastNonDominatedSorting(pop[, (varNo + 1): (varNo + objDim)])
		rnkIndex <- integer(popSize)
		i <- 1
		while(i <= length(ranking)){
			rnkIndex[ranking[[i]]] <- i
			i <- i + 1
		}
		pop <- cbind(pop, rnkIndex)
		objRange <- apply(pop[, (varNo + 1):(varNo + objDim)], 2, max) - apply(pop[, (varNo + 1):(varNo + objDim)], 2, min)
		cd <- crowdingDist4frnt(pop, ranking, objRange)
		pop <- cbind(pop, apply(cd, 1, sum))
		#Copy all the particles in Swarm to a union
		union <- pop[order(pop[, varNo + objDim + 1], -pop[, varNo + objDim + 2]), ]

		#update the Pbest for each particle
		for(i in 1:popSize){
			if(pop[i, varNo + 1] <= pBest[i, varNo + 1] &&
			   pop[i, varNo + 2] <= pBest[i, varNo + 2]){
				pBest[i, ] <- pop[i, ]
			}
			#Randomly choose a gBest from highest ranked solutions
			gBest <- pop[sample(1:nrow(pop[pop[, "rnkIndex"] == 1,]), 1), 1:varNo]
		
			#update velocity for i particle
			for(j in 1:varNo){
				velocity[i, j] <- w * velocity[i, j] + c1 * runif(1, 0, 1) * (pBest[i, j] - pop[i, j]) + c2 * runif(1, 0, 1) * (gBest[j] - pop[i, j])
			}

			#update particle
			for(j in 1:varNo){
				if(runif(1, 0, 1) < transformation(velocity[i, j])){
					pop[i, j] <- 1
				} else {
					pop[i, j] <- 0
				}
			}
			union <- rbind(union, pop[i, ])
		}
		pop <- vector()
		i <- 1
		repeat {
			#first time
			if(is.null(nrow(pop))){
				currentPopSize <- 0
			} else{
				currentPopSize <- nrow(pop)
			}
			if(currentPopSize >= popSize){
				break
			}
			if(currentPopSize + nrow(union[union[, "rnkIndex"] == i,]) <= popSize){
				pop <- rbind(pop, union[union[, "rnkIndex"] == i,])
				i <- i + 1
			} else{
				unionI <- union[union[, "rnkIndex"] == i,]
				unionI <- unionI[order(unionI[, varNo + objDim + 1], -unionI[, varNo + objDim + 2]),]
				pop <- rbind(pop, unionI[1:(popSize - currentPopSize), ])
				#print(nrow(unionI))
			}
		}
		front <- pop[pop[, "rnkIndex"] == 1, (varNo + 1):(varNo + objDim)]
		if(logS == T){
			if(as.logical(match(1, front[, 2], nomatch = 0))){
				tempfront <- front[-which(front[, 2] == 1), ]
				if(as.logical(match(1, tempfront[, 1], nomatch = 0))){
					tempfront <- front[-which(tempfront[, 1] == 1), ]
			}
				log(problem, seed, iter, pop[pop[, "rnkIndex"] == 1, 1:varNo], tempfront[, 1:2])
			} else{
				log(problem, seed, iter, pop[pop[, "rnkIndex"] == 1, 1:varNo], front[, 1:2])
			}
		}
		pop <- pop[, 1:varNo]
		w <- w - (0.4 / iter)
	}
	#-------------------Algorithm End--------------------------------
	front
}
