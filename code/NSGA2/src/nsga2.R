library(foreach)
library(GA)
library(mco)
library(nsga2R)
source('log.R')
run_algorithm <- function(noSerivce, noCandLoc, noUserCen, seed = 1, cost_limitation, max_cost, min_cost, max_latency, min_latency, problem, initialisation, logS){
	ptm <- proc.time()
	front <- algorithm(noSerivce, noCandLoc, noUserCen, seed, cost_limitation, max_cost, min_cost, max_latency, min_latency, 
					   problem, logS, initialisation)
	print((proc.time() - ptm)[1])
	fitness_value <- evaluate_front(front, noSerivce, noCandLoc, noUserCen, max_cost, min_cost, max_latency, min_latency)
	fitness_value <- cbind(fitness_value, (proc.time() - ptm)[1])
	colnames(fitness_value) <- c("costF", "latencyF", "time")
	fitness_value
}



algorithm <- function(noSerivce, noCandLoc, noUserCen, run, cost_limitation, max_cost, min_cost, max_latency, min_latency, problem, logS, initialisation = F){
	popSize <- 50
	objDim <- 2
	varNo <- noSerivce * noCandLoc
	tourSize <- 3
	MutDistIdx <- 20
	mprob <- 0.2
	XoverDistIdx <- 20
	cprob <- 0.8
	generations <- 50
	front <- vector()
	#front_pool <- vector()
#=============================================================================
set.seed(run)
#========================Algorithm Starts=====================================
	#	step 1, initialize the population
	parent <- generate_population(noSerivce, noCandLoc, noUserCen, popSize, cost_limitation, initialisation)

#====================Calculate Fitness================================
	#	step 2, calculate the fitness
	fitness_value <- t(apply(parent, 1, fitness, noSerivce=noSerivce, noCandLoc=noCandLoc, noUserCen=noUserCen, max_cost = max_cost, min_cost = min_cost, 
						   												max_latency = max_latency, min_latency = min_latency))
	parent <- cbind(parent, fitness_value)
#====================Normalization Ends================================

	#	step 3, ranking 
	ranking <- fastNonDominatedSorting(parent[, (varNo + 1):(varNo + objDim)])
	rnkIndex <- integer(popSize)
	i <- 1
	while (i <= length(ranking)){
		rnkIndex[ranking[[i]]] <- i
		i <- i + 1
	}
	parent <- cbind(parent, rnkIndex)

	#	step 4, calculate the crowding value
	objRange <- apply(parent[, (varNo + 1) : (varNo + objDim)], 2, max) - apply(parent[, (varNo + 1) : (varNo + objDim)], 2, min)
	cd <- crowdingDist4frnt(parent, ranking, objRange)

	parent <- cbind(parent, apply(cd, 1, sum))
	#initialize the front_pool
	#front_pool <- parent[parent[, varNo + 3] == 1, ]

	for(iter in 1:generations){
		if(logS == T){
			tempfront <- parent[parent[, varNo + 3] == 1, (varNo + 1):(varNo + objDim)]
			if(as.logical(match(1, tempfront[, 2], nomatch = 0))){
				tempfront <- tempfront[-which(tempfront[, 2] == 1), ]
				if(as.logical(match(1, tempfront[, 1], nomatch = 0))){
					tempfront <- tempfront[-which(tempfront[, 1] == 1), ]
			}
				log(problem, run, iter, parent[parent[, varNo + 3] == 1, 1:varNo], tempfront)
			} else {
				log(problem, run, iter, parent[parent[, varNo + 3] == 1, 1:varNo], parent[parent[, varNo + 3] == 1, (varNo + 1):(varNo + objDim)])
			}
		}
		#step 5, selection
		matingPool <- tournamentSelection(parent, popSize, tourSize)
		#print(matingPool)
	
		#	step 6, Crossover
		childAfterX <- crossover(matingPool[, 1:varNo], cprob)
	
		#	step 7 Mutation
		childAfterM <- mutation(childAfterX, mprob)
		#Check for validation
		for(j in 1:nrow(childAfterM)){
			chromosome <- services_constraint_check(childAfterM[j, ], noSerivce, noCandLoc)
			if(is.logical(chromosome)){
				childAfterM[j, ] <- repair_service_minimum(childAfterM[j, ], noSerivce, noCandLoc)
			}
			chromosome <- cost_constraint_check(childAfterM[j, ], cost_limitation, noSerivce, noCandLoc)
			if(is.logical(chromosome)){
				childAfterM[j, ] <- repair_cost_maximum(childAfterM[j, ], cost_limitation, noSerivce, noCandLoc)
			}
		}
		#Evaluation with fitness function
		childAfterM <- cbind(childAfterM, t(apply(childAfterM, 1, fitness, noSerivce, noCandLoc, 
												 noUserCen, max_cost, min_cost, max_latency, min_latency)))
		

		parentNext <- rbind(parent[, 1:(varNo + objDim)], childAfterM)
		ranking <- fastNonDominatedSorting(parentNext[, (varNo + 1) : (varNo + objDim)])
		i <- 1
		while (i <= length(ranking)){
			rnkIndex[ranking[[i]]] <- i
			i <- i + 1
		}
		parentNext <- cbind(parentNext, rnkIndex)
		objRange <- apply(parent[, (varNo + 1) : (varNo + objDim)], 2, max) - apply(parent[, (varNo + 1) : (varNo + objDim)], 2, min)
		cd <- crowdingDist4frnt(parentNext, ranking, objRange)
		parentNext <- cbind(parentNext, apply(cd, 1, sum))
		parentNext.sort <- parentNext[order(parentNext[, varNo + objDim + 1]), ]
		parent <- parentNext.sort[1:popSize, ]
		front <- parent[parent[, varNo + 3] == 1, ]
		if (is.matrix(front) == F){
			front <- matrix(front, ncol = varNo + 4)
		}
	}

	result = list(functions = fitness, parameterDim = varNo, objectiveDim = objDim, popSize = popSize,
				  tournamentSize = tourSize, generations = generations, XoverProb = cprob, mutationProb = mprob,
				  parameters = parent[,1:varNo], objectives = parent[, (varNo + 1):(varNo + objDim)], 
				  paretoFrontRank = parent[, varNo + objDim + 1], crowdingDistance = parent[, varNo + objDim + 2])
	class(result) = "nsga2R"
	#cat("Repair time: ", repair_time, "\n", sep = "")
	unique(front[, 1:varNo])
}




evaluate_front <- function(front, noSerivce, noCandLoc, noUserCen, max_cost, min_cost, max_latency, min_latency){
	fitness_value <- t(apply(front[, 1:(noSerivce * noCandLoc)], 1, fitness, noSerivce, noCandLoc, noUserCen, max_cost, min_cost, max_latency, min_latency))
	fitness_value
}



generate_population <- function(noSerivce, noCandLoc, noUserCen, size, cost_limitation, initialisation = F){
	parent <- vector()
	count <- 0
	if(initialisation == T){
		size <- size - 1
		low_cost_chromosome <- as.vector(search_minimum_cost(noSerivce, noCandLoc))
		parent <- rbind(parent, low_cost_chromosome)
	}
	repeat {
		check <- 1
		if(count > size - 1){
			break
		}
		chromosome <- rbinom(noSerivce * noCandLoc, 1, 0.5)
		
		#Check the chromosome is valid
		check <- services_constraint_check(chromosome, noSerivce, noCandLoc)
		if(is.logical(check)){
			#If the chromosome is invalid, then fix it
			chromosome <- repair_service_minimum(chromosome, noSerivce, noCandLoc)
			check <- 1
		}
		check <- cost_constraint_check(chromosome, cost_limitation, noSerivce, noCandLoc)
		if(is.logical(chromosome)){
		#If the chromosome is invalid, then fix it
			chromosome <- repair_cost_maximum(chromosome, cost_limitation, noSerivce, noCandLoc)
		}
		parent <- rbind(parent, chromosome)
		count <- count + 1
	}
	parent
}

fitness <- function(chromosome, noSerivce, noCandLoc, noUserCen, max_cost, min_cost, max_latency, min_latency) {
	cost <- normalized_cost_fitness(chromosome, noSerivce, noCandLoc, max_cost, min_cost)
	latency <- normalized_latency_fitness(chromosome, noSerivce, noCandLoc, noUserCen, max_latency, min_latency)
	return(c(cost, latency))
}

crossover <- function(parent_chromosome, cprob){
	popSize = nrow(parent_chromosome)
	varNo = ncol(parent_chromosome)
	new_pop <- vector()
	p <- 1
	for (i in 1:(popSize / 2)){
		if(runif(1) < cprob){
			cutPoint <- floor(runif(1, 1, varNo))
			child_1 <- parent_chromosome[p, 1:cutPoint]
			child_1 <- c(child_1, parent_chromosome[p + 1, (cutPoint + 1):varNo])
			child_2 <- parent_chromosome[p + 1, 1:cutPoint]
			child_2 <- c(child_2, parent_chromosome[p, (cutPoint + 1):varNo])
		}
		else{
			child_1 <- parent_chromosome[p, ]
			child_2 <- parent_chromosome[p + 1, ]
		}
		new_pop <- rbind(new_pop, child_1)
		new_pop <- rbind(new_pop, child_2)
		p <- p + 2
	}
	new_pop
}

mutation <- function(parent_chromosome, mprob){
	popSize = nrow(parent_chromosome)
	varNo = ncol(parent_chromosome)
	new_pop <- vector()
	for(i in 1:popSize){
		child <- parent_chromosome[i, ]
		for(j in 1:varNo){
			if(runif(1) < mprob){
				child[j] <- !child[j]
			}
		}
		new_pop <- rbind(new_pop, child)
	}
	new_pop
}

normalized_cost_fitness <- function(chromosome, noSerivce, noCandLoc, max_cost, min_cost){
	cost <- unNormalized_cost_fitness(chromosome, noSerivce, noCandLoc)
	cost <- normalize(cost, max_cost, min_cost)
	cost
}
unNormalized_cost_fitness <- function(chromosome, noSerivce, noCandLoc, max_cost, min_cost){
	chromosome <- matrix(chromosome, nrow = noSerivce)
	cost <- sum(chromosome * cost_matrix)
	cost
}

normalized_latency_fitness <- function(chromosome, noSerivce, noCandLoc, noUserCen, max_latency, min_latency){
	latency <- unNormalized_latency_fitness(chromosome, noSerivce, noCandLoc, noUserCen)
	latency <- normalize(latency, max_latency, min_latency)
	latency
}

unNormalized_latency_fitness <- function(chromosome, noSerivce, noCandLoc, noUserCen){

	chromosome <- as.integer(chromosome)
	chromosome <- matrix(chromosome, nrow = noSerivce)
	latency <- 0.0
	frequency <- colSums(frequency_matrix)
	response_matrix <- vector()
	for(user_iter in 1:noUserCen){
		for(service_iter in 1:noSerivce){
			num_of_service <- sum(chromosome[service_iter, ])
			deployed_service <- which(chromosome[service_iter, ] == 1)
			if(num_of_service > 1){
				response_matrix <- c(response_matrix, min(latency_matrix[user_iter, deployed_service]))
			}
			else{
				response_matrix <- c(response_matrix, latency_matrix[user_iter, deployed_service])
			}
		}
	}
	response_matrix <- matrix(response_matrix, nrow = noUserCen, byrow = T)
	latency <- sum(response_matrix * frequency_matrix)
	latency
}

normalize <- function(value, max_value, min_value){
	#cat("value = ", value, "max_value = ", max_value, "min_value", min_value, "\n")
	normalized_data <- vector()
	normalized_data <- (value - min_value) / (max_value - min_value)
	#-----------------Debug --------------------------------
	if(normalized_data < 0){
		cat("value = ", value, "max_value = ", max_value, "min_value", min_value, "\n")
	}
	if(value > max_value){
		cat("value = ", value, "max_value = ", max_value, "min_value", min_value, "\n")
	}
	#-----------------Debug --------------------------------
	normalized_data
}


services_constraint_check <- function(chromosome, noSerivce, noCandLoc){
	chromosome_m <- matrix(chromosome, nrow = noSerivce)
	if(prod(apply(chromosome_m, 1, services_minimum)) == 0) return(F)
	return(chromosome)
	#return(T)
}

services_minimum <- function(chromosome){
	res <- sum(chromosome) > 0
	res
}

#If the minimum service number is not achieved, then 
#randomly choose a point add 1 service deployment
repair_service_minimum <- function(chromosome, noSerivce, noCandLoc){
	chromosome_m <- matrix(chromosome, nrow = noSerivce)
	for(iter in 1:noSerivce){
		if(sum(chromosome_m[iter, ]) == 0){
			cutPoint <- floor(runif(1, 1, noCandLoc))
			chromosome_m[iter, cutPoint] <- 1
		}
	}
	chromosome <- as.vector(chromosome_m)
	chromosome
}

#Limitation of cost
cost_constraint_check <- function(chromosome, limitation, noSerivce, noCandLoc){
	chromosome_m <- matrix(chromosome, nrow = noSerivce)
	cost <- sum(chromosome_m * cost_matrix)
	if(cost > limitation) return(F)
	return(chromosome)
}
repair_cost_maximum <- function(chromosome, limitation, noSerivce, noCandLoc){
	chromosome_m <- matrix(chromosome, nrow = noSerivce)
	repeat {
		row_num <- vector()
		#firstly, we check which row has more than one service deployed
		for(iter in 1:noSerivce){
			if(sum(chromosome_m[iter, ]) > 1){
				row_num <- c(row_num, iter)
			}
		}
		#Secondly, we check if there is no more improvement we can make
		if(length(row_num) == 0){
			return(as.vector(chromosome_m))
		}

		#Thirdly, find the multiple deployment, vectorized them and then randomly select one of them to drop
		#After that, do AND operation with the original chromosome so that it will change the original one
		candidate_chromosome <- chromosome_m[row_num, ]
		candidate_chromosome <- as.vector(candidate_chromosome)
		candidate_index <- which(candidate_chromosome %in% 1)
		selected_num <- sample(candidate_index, 1, replace = F)
		candidate_chromosome[selected_num] <- 0
		chromosome_m[row_num, ] <- chromosome_m[row_num, ] & candidate_chromosome
		
		chromosome <- as.vector(chromosome_m)
		check <- cost_constraint_check(chromosome, limitation, noSerivce, noCandLoc)
		if(is.logical(check) != T) {
			return(chromosome)
		}
	}
}
