source('preData.R')
source('log.R')
bpso <- function(fitFunctions, varcnt, obj, lowerbound, upperbound, c1, c2, maxgen, weight = 0.5, popsize, seed, problem){

	#parameters settings
	set.seed(seed)
	varcount <- varcnt
	popSize <- popsize
	w <- weight
	velocity <- matrix(rep(1, varcount * popSize), nrow = popSize)
	fitness <- matrix(rep(1, obj * popSize), nrow = popSize)
	pBest <- matrix(rep(1, varcount * popSize), nrow = popSize)
	pBestFitness <- matrix(rep(1, obj * popSize), nrow = popSize)
	gBest <- rep(1, varcount)
	gBestFitness <- rep(1, obj)
	#===================Component Functions Start========================================
	generate_population <- function(varcnt, size, initialisation = F){
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
			particle <- rbinom(varcount, 1, 0.5)
			pop <- rbind(pop, particle)
			count <- count + 1
		}
		return(pop)
	}

	evaluate <- function(pop, fitFunctions){
		tempFitness <- vector()
		for(i in 1:nrow(pop)){
			tempFitness <- rbind(tempFitness, fitFunctions(pop[i, ]))
		}
		#fitness <<- (1 - w) * tempFitness[, 1] + w * tempFitness[, 2]
		fitness <<- tempFitness
	}

	updatePBest <- function(pop){
		for(i in 1:popSize){
			combinedFitness <- (1 - w) * fitness[i, 1] + w * fitness[i, 2]
			pBestCombinedFitness <- (1 - w) * pBestFitness[i, 1] + w * pBestFitness[i, 2]
			if(combinedFitness <= pBestCombinedFitness){
				pBest[i, ] <<- pop[i, ]
				pBestFitness[i, ] <<- fitness[i, ]
			}
		}
	}

	updateGBest <- function(pop){
		for(i in 1:popSize){
			pBestCombinedFitness <- (1 - w) * pBestFitness[i, 1] + w * pBestFitness[i, 2]
			gBestCombinedFitness <- (1 - w) * gBestFitness[1] + w * gBestFitness[2]
			if(pBestCombinedFitness <= gBestCombinedFitness){
				gBest <<- pBest[i, ]
				gBestFitness <<- pBestFitness[i, ]
			}
		}
	}

	updateVelocity <- function(pop) {
		for(i in 1:nrow(pop)){
			for(j in 1:varcount){
				velocity[i, j] <<- velocity[i, j] + c1* runif(1, 0, 1) * 
								(pBest[i, j] - pop[i, j]) + c2 * runif(1, 0, 1) * 
								(gBest[j] - pop[i,j])
			}
		}
	}

	updateParticle <- function(pop) {
		for(i in 1:nrow(pop)){
			for(j in 1:varcount){
				if(runif(1, 0, 1) < transformation(velocity[i, j])){
					pop[i, j] <<- 1
				} else {
					pop[i, j] <<- 0
				}
			}
		}
	}

	transformation <- function(v) {
		return(1/(1 + exp(-v)))
	}
	updatePop <- function(){
		updateVelocity(pop)
		updateParticle(pop)
	}
	#===================Component Functions End========================================
	
	#------plot function Start-----------
	plotme <- function(fitness){
		plot(fitness, xlim = c(0, 1), ylim = c(0, 1), col = 'red')
		par(new = T)
	}
	#------plot function End-----------

	#-------------------Algorithm Start--------------------------------
	pop <- generate_population(varcnt, popSize, initialisation)
	for(iter in 1:maxgen){
		evaluate(pop, fitFunctions)
		updatePBest(pop)
		updateGBest(pop)
		updatePop()
		log(problem, seed, iter, pop, fitness)
		#plotme(fitness)
	}
	#-------------------Algorithm End--------------------------------
	colnames(fitness) <- c('cost', 'latency')
	fitness
}
