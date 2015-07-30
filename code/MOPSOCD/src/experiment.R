#library(mopsocd)
library(foreach)
library(doMC)
library(mco)
library(nsga2R)
registerDoMC(8)

source("preData.R")
source("nsga2.R")
source("mopsocd_origin.R")



run <- function(noService=3, noCandLoc=3, noUserCen = 3, limitation=10000){
	#---------------Parameter settings---------------
	varcount <- noService * noCandLoc
	fncount <- 2
	lbound <- rep(0, varcount)
	ubound <- rep(1, varcount)
	optim <- 0
	threshold <- 0.7
	maxgen <- 50
	popsize <- 50
	#---------------Parameter settings---------------
	#--------------------------------Fitness Functions------------------------------------------------

	unNormalized_latency_fitness <- function(chromosome, noService, noCandLoc, noUserCen, threshold){
		#chromosome <- as.integer(chromosome)
		latency <- 0.0
		#print(chromosome)
		chromosome <- sapply(chromosome, function(x, threshold) if(x > threshold) 1 else 0, threshold)
		chromosome <- matrix(chromosome, nrow = noService)
		frequency <- colSums(frequency_matrix)
		response_matrix <- vector()
		for(user_iter in 1:noUserCen){
			for(service_iter in 1:noService){
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
		if(ncol(response_matrix) < ncol(frequency_matrix)){
			#unqualified matrix
			return(10000000)
			#return(Inf)
		}
		#print(frequency_matrix)
		latency <- sum(response_matrix * frequency_matrix)
		latency
	}
	
	normalized_cost_fitness <- function(chromosome, noService, noCandLoc, max_cost, min_cost, threshold){
		cost <- unNormalized_cost_fitness(chromosome, noService, noCandLoc, threshold)
		cost <- normalize(cost, max_cost, min_cost)
		cost
	}
	unNormalized_cost_fitness <- function(chromosome, noService, noCandLoc, threshold){
		chromosome <- sapply(chromosome, function(x, threshold) if(x > threshold) 1 else 0, threshold)
		chromosome <- matrix(chromosome, nrow = noService)
		cost <- sum(chromosome * cost_matrix)
		cost
	}
	
	normalized_latency_fitness <- function(chromosome, noService, noCandLoc, noUserCen, max_latency, min_latency, threshold){
		latency <- unNormalized_latency_fitness(chromosome, noService, noCandLoc, noUserCen, threshold)
		latency <- normalize(latency, max_latency, min_latency)
		latency
	}
	
	normalize <- function(value, max_value, min_value){
		#cat("value = ", value, "max_value = ", max_value, "min_value", min_value, "\n")
		normalized_data <- vector()
		normalized_data <- (value - min_value) / (max_value - min_value)
		#-----------------Debug --------------------------------
		#if(normalized_data < 0){
			#cat("value = ", value, "max_value = ", max_value, "min_value", min_value, "\n")
		#}
		#if(value > max_value){
			#cat("value = ", value, "max_value = ", max_value, "min_value", min_value, "\n")
		#}
		#-----------------Debug --------------------------------
		normalized_data
	}
	#--------------------------------Fitness Functions------------------------------------------------

	#---------------Pre-processing-------------------
	# In data pre-processing, Three matrices should be prepared - Cost matrix, frequency_matrix, latency_matrix
	# It should also prepare the max & min cost and latency

	#====================pre-processing functions===================================================
	search_maximum_latency <- function(noService, noCandLoc, noUserCen, threshold){
		matrixSize <- noService * noCandLoc
		initial_matrix <- matrix(c(rep(1, noService), rep(0, noService * (noCandLoc - 1))), nrow = noService)
		latency <- 0.0
		for(row_iter in 1:noService){
			for(col_iter in 1:noCandLoc){
				if(row_iter == 1 && col_iter == 1){
					#first time
					latency <- unNormalized_latency_fitness(initial_matrix, noService, noCandLoc, noUserCen, threshold)
					next
				}
				num <- which(initial_matrix[row_iter, ] == 1)
				initial_matrix[row_iter, num] <- 0
				initial_matrix[row_iter, col_iter] <- 1
				current_latency <- unNormalized_latency_fitness(initial_matrix, noService, noCandLoc, noUserCen, threshold)
				if(current_latency > latency){
					latency <- current_latency
				}
				else{
					initial_matrix[row_iter, col_iter] <- 0
					initial_matrix[row_iter, num] <- 1
				}
			}
		}
		latency
	}
	
	search_minimum_cost <- function(noService, noCandLoc){
		matrixSize <- noService * noCandLoc
		mini_cost_matrix <- matrix(rep(0, matrixSize), nrow = noService)
		for(row_iter in 1:noService){
			pos <- which(cost_matrix[row_iter, ] == min(cost_matrix[row_iter, ]))[1]
			mini_cost_matrix[row_iter, pos] <- 1
		}
		#print(mini_cost_matrix)
		mini_cost_matrix
	}
	#====================pre-processing functions===================================================
	predata(noService, noCandLoc, noUserCen)
	min_latency <- 0
	max_latency <- search_maximum_latency(noService, noCandLoc, noUserCen, threshold)

	min_cost <- sum(search_minimum_cost(noService, noCandLoc) * cost_matrix)
	max_cost <- sum(cost_matrix)
	#---------------Pre-processing-------------------

	
	
	#----------------------Constraint Functions-------------------------------------------------------
	cost_constraint_check <- function(chromosome, limitation, noService, noCandLoc, threshold){
		#print(chromosome)
		chromosome <- sapply(chromosome, function(x, threshold) if(x > threshold) 1 else 0, threshold)
		chromosome_m <- matrix(chromosome, nrow = noService)
		cost <- sum(chromosome_m * cost_matrix)
		if(cost > limitation) return(F)
		#return(chromosome)
		return(T)
	}


	services_constraint_check <- function(chromosome, noService, noCandLoc, threshold){
		chromosome <- sapply(chromosome, function(x, threshold) if(x > threshold) 1 else 0, threshold)
		chromosome_m <- matrix(chromosome, nrow = noService)
		if(prod(apply(chromosome_m, 1, function(x) sum(x) > 0)) == 0) return(F)
		#return(chromosome)
		return(T)
	}
	#----------------------Constraint Functions-------------------------------------------------------

	#----------------------Define constraint and fitness function-------------------------------------
	constraint <- function(chromosome){
		cost_constraint <- cost_constraint_check(chromosome, limitation, noService, noCandLoc, threshold)
		service_constraint <- services_constraint_check(chromosome, noService, noCandLoc, threshold)
		return(c(cost_constraint, service_constraint))
	}
	
	fitFunction <- function(chromosome) {
		cost <- normalized_cost_fitness(chromosome, noService, noCandLoc, max_cost, min_cost, threshold)
		latency <- normalized_latency_fitness(chromosome, noService, noCandLoc, noUserCen, max_latency, min_latency, threshold)
		return(c(cost, latency))
	}
	#----------------------Define constraint and fitness function-------------------------------------
	best_front <- function(data){
		temp <- unique(data[, 1:2])
		front <- vector()
		bad_kids <- NULL
		best_front <- vector()
		ranking <- fastNonDominatedSorting(temp)
		rnkIndex <- integer()
		i <- 1
		while (i <= length(ranking)){
			rnkIndex[ranking[[i]]] <- i
			i <- i + 1
		}
		front <- cbind(temp, rnkIndex)
		best_front <- front[front[, 3] == 1,]

		#check valid
		for(i in 1:nrow(best_front)){
			if(best_front[i, 1] < 0 || best_front[i, 1] > 1 ||
			   best_front[i, 2] < 0 || best_front[i, 2] > 1){
				bad_kids <- c(bad_kids, i)
			}
		}
		if(is.null(bad_kids)){
			return(best_front)
		} else{
			best_front <- best_front[-bad_kids, ]
			return(best_front)
		}
	}


	#-----------------------------------------------------

	#Now it's time to run algorithm
	NSGAdata <- vector()
	PSOdata <- vector()
	PSOtime <- vector()
	NSGAtime <- vector()
	#fire on all 8 cores
	foreach(iter = 1:40) %dopar%{
		filename_pso_time <- paste(iter, "_pso_time.csv", sep = '')
		filename_pso <- paste(iter, "_pso.csv", sep = "")
		filename_nsga <- paste(iter, "_nsga.csv", sep = "")
		filename_nsga_time <- paste(iter, "_nsga_time.csv", sep = '')
		PSOtime <- vector()
		cat("Generations: ", iter, '\n')
		ptm <- proc.time()
		#PSO begins here
		psoResult <- mopsocd(fitFunction, constraint, varcnt=varcount,
					  fncnt=fncount, lowerbound=lbound, upperbound=ubound,
					  opt=optim, maxgen=maxgen, popsize=popsize)
		PSOtime <- c(PSOtime, (proc.time() - ptm)[1])
		#NSGA begins here
		NSGAdata <- run_algorithm(noService, noCandLoc, noUserCen, iter, limitation, max_cost, min_cost, max_latency, min_latency, initialisation = F)
		NSGAtime <- NSGAdata[1, 3]
		write.csv(matrix(psoResult$objfnvalues, ncol = 2), filename_pso, row.names = F, quote = F)
		write.csv(PSOtime, filename_pso_time, row.names = F, quote = F)
		write.csv(NSGAtime, filename_nsga_time, row.names = F, quote = F)
		write.csv(NSGAdata[, 1:2], filename_nsga, row.names = F, quote = F)
	}
	for(iter in 1:40){
		filename_pso <- paste(iter, "_pso.csv", sep = "")
		filename_pso_time <- paste(iter, "_pso_time.csv", sep = "")
		filename_nsga <- paste(iter, "_nsga.csv", sep = "")
		filename_nsga_time <- paste(iter, "_nsga_time.csv", sep = "")
		PSOdata <- rbind(PSOdata, read.csv(filename_pso, header = T, sep = ','))
		PSOtime <- rbind(PSOtime, read.csv(filename_pso_time, header = T))
		NSGAdata <- rbind(NSGAdata, read.csv(filename_nsga, header = T, sep = ','))
		NSGAtime <- rbind(NSGAtime, read.csv(filename_nsga_time, header = T))
	}
	
	#best front for both
	best_pso <- best_front(PSOdata)
	best_nsga <- best_front(NSGAdata)
	best_pso <- cbind(best_pso, rep(1, nrow(best_pso)))
	best_nsga <- cbind(best_nsga, rep(2, nrow(best_nsga)))
	names(best_pso) <- c('costF', 'latencyF', 'rank', 'alg')
	names(best_nsga) <- c('costF', 'latencyF', 'rank', 'alg')
	data <- rbind(best_pso[, c(1, 2, 4)], best_nsga[, c(1, 2, 4)])



	if(noService == 20){
		filename <- paste('1',".csv", sep = '')
		PSOtimeFilename <- paste('1', "_PSO_time.csv", sep='')
		NSGAtimeFilename <- paste('1', "_NSGA_time.csv", sep='')
	}else if(noService == 50){
		filename <- paste('2',".csv", sep = '')
		PSOtimeFilename <- paste('2', "_PSO_time.csv", sep='')
		NSGAtimeFilename <- paste('2', "_NSGA_time.csv", sep='')
	}else if(noService == 100){
		filename <- paste('3',".csv", sep = '')
		PSOtimeFilename <- paste('3', "_PSO_time.csv", sep='')
		NSGAtimeFilename <- paste('3', "_NSGA_time.csv", sep='')
	}else if(noService == 200){
		filename <- paste('4',".csv", sep = '')
		PSOtimeFilename <- paste('4', "_PSO_time.csv", sep='')
		NSGAtimeFilename <- paste('4', "_NSGA_time.csv", sep='')
	}else {
		filename <- paste('other', '.csv', sep = '')
		PSOtimeFilename <- paste('other', "_PSO_time.csv", sep='')
		NSGAtimeFilename <- paste('other', "_NSGA_time.csv", sep='')
	}
	write.csv(data, filename, row.names = F, quote = F)
	write.csv(PSOtime, PSOtimeFilename, row.names = F, quote = F)
	write.csv(NSGAtime, NSGAtimeFilename, row.names = F, quote = F)
}


