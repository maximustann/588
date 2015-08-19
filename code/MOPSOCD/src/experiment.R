#library(mopsocd)
library(foreach)
library(doMC)
library(mco)
library(nsga2R)
registerDoMC(8)

source("preData.R")
source("mopsocd_origin.R")



run <- function(problem = 5, logS = F){

	#----------------------Set up Problem Start------------------------------
	if(problem == 1){
		noService <- 20
		noCandLoc <- 5
		noUserCen <- 10
		limitation <- noService * noCandLoc * 100
	} else if(problem == 2){
		noService <- 20
		noCandLoc <- 10
		noUserCen <- 10
		limitation <- noService * noCandLoc * 100
	} else if(problem == 3){
		noService <- 50
		noCandLoc <- 15
		noUserCen <- 20
		limitation <- noService * noCandLoc * 100
	} else if(problem == 4){
		noService <- 50
		noCandLoc <- 15
		noUserCen <- 40
		limitation <- noService * noCandLoc * 100
	} else if(problem == 5){
		noService <- 50
		noCandLoc <- 25
		noUserCen <- 20
		limitation <- noService * noCandLoc * 100
	} else if(problem == 6){
		noService <- 50
		noCandLoc <- 25
		noUserCen <- 40
		limitation <- noService * noCandLoc * 100
	}else if(problem == 7){
		noService <- 100
		noCandLoc <- 15
		noUserCen <- 20
		limitation <- noService * noCandLoc * 100
	} else if(problem == 8){
		noService <- 100
		noCandLoc <- 15
		noUserCen <- 40
		limitation <- noService * noCandLoc * 100
	} else if(problem == 9){
		noService <- 100
		noCandLoc <- 25
		noUserCen <- 20
		limitation <- noService * noCandLoc * 100
	} else if(problem == 10){
		noService <- 100
		noCandLoc <- 25
		noUserCen <- 40
		limitation <- noService * noCandLoc * 100
	} else if(problem == 11){
		noService <- 200
		noCandLoc <- 25
		noUserCen <- 40
		limitation <- noService * noCandLoc * 100
	} else if(problem == 12){
		noService <- 200
		noCandLoc <- 25
		noUserCen <- 80
		limitation <- noService * noCandLoc * 100
	} else if(problem == 13){
		noService <- 200
		noCandLoc <- 40
		noUserCen <- 40
		limitation <- noService * noCandLoc * 100
	} else if(problem == 14){
		noService <- 200
		noCandLoc <- 40
		noUserCen <- 80
		limitation <- noService * noCandLoc * 100
	}

	#----------------------Set up Problem End------------------------------
	#---------------Parameter settings---------------
	varcount <- noService * noCandLoc
	fncount <- 2
	lbound <- rep(0, varcount)
	ubound <- rep(1, varcount)
	optim <- 0
	threshold <- 0.7
	maxgen <- 50
	popsize <- 100
	#---------------Parameter settings---------------
	#--------------------------------Fitness Functions------------------------------------------------

	unNormalized_latency_fitness <- function(chromosome, noService, noCandLoc, noUserCen, threshold){
		latency <- 0.0
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
			#death penalty of latency
			return(max_latency)
		}
		latency <- sum(response_matrix * frequency_matrix)
		latency
	}
	
	normalized_cost_fitness <- function(chromosome, noService, noCandLoc, max_cost, min_cost, threshold){
		cost <- unNormalized_cost_fitness(chromosome, noService, noCandLoc, threshold)
		cost <- normalize(cost, max_cost, min_cost)
		if(cost < 0){
			#death penalty of cost
			return(1)
		}
		#if(validation(chromosome, noService) == F){

			#return(max_cost)
		#}
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
		normalized_data
	}

	validation <- function(chromosome, noService){
		chromosome <- matrix(chromosome, nrow = noService)
		if(prod(rowSums(chromosome)) == 0){
			return(F)
		}
		return(T)
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
		chromosome <- sapply(chromosome, function(x, threshold) if(x > threshold) 1 else 0, threshold)
		chromosome_m <- matrix(chromosome, nrow = noService)
		cost <- sum(chromosome_m * cost_matrix)
		if(cost > limitation) return(F)
		return(T)
	}


	services_constraint_check <- function(chromosome, noService, noCandLoc, threshold){
		chromosome <- sapply(chromosome, function(x, threshold) if(x > threshold) 1 else 0, threshold)
		chromosome_m <- matrix(chromosome, nrow = noService)
		if(prod(apply(chromosome_m, 1, function(x) sum(x) > 0)) == 0) return(F)
		return(T)
	}
	#----------------------Constraint Functions-------------------------------------------------------

	#----------------------Define constraint and fitness function-------------------------------------
	constraint <- function(chromosome){
		cost_constraint <- cost_constraint_check(chromosome, limitation, noService, noCandLoc, threshold)
		service_constraint <- services_constraint_check(chromosome, noService, noCandLoc, threshold)
		return(c(cost_constraint, service_constraint))
	}
	
	fitFunction <- function(chromosome, gen) {
		#if(gen != 50){
			#threshold <- 0.7 - 0.5 / (50 - gen)
			threshold <- -(1/5000)*gen^2 + 0.7
			#threshold <- -(6/25000)*gen^2 + 0.8 #0.8~0.2
		#}
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
		#print(temp)
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
	dir.create(paste('../result/', problem, '/run/', sep = ''), showWarnings = F)
	dir.create(paste('../result/', problem, '/best/', sep = ''), showWarnings = F)
	mPSOdata <- vector()
	mPSOtime <- vector()
	#fire on all 8 cores
	foreach(iter = 1:40) %dopar%{
		filename_mpso_time <- paste('../result/', problem, '/run/', iter, "_mpso_time.csv", sep = '')
		filename_mpso <- paste('../result/', problem, '/run/', iter, "_mpso.csv", sep = "")
		mPSOtime <- vector()
		cat("run: ", iter, '\n')
		ptm <- proc.time()
		#mPSO begins here
		mpsoResult <- mopsocd(fitFunction, varcnt=varcount,
					  fncnt=fncount, lowerbound=lbound, upperbound=ubound,
					  opt=optim, maxgen=maxgen, popsize=popsize, seed = iter, problem = problem, logS = logS)
		mPSOtime <- c(mPSOtime, (proc.time() - ptm)[1])
		write.csv(matrix(mpsoResult$objfnvalues, ncol = 2), filename_mpso, row.names = F, quote = F)
		write.csv(mPSOtime, filename_mpso_time, row.names = F, quote = F)
	}
	print("Start compiling...")
	for(iter in 1:40){
		filename_mpso <- paste('../result/', problem, '/run/', iter, "_mpso.csv", sep = "")
		filename_mpso_time <- paste('../result/', problem,'/run/', iter, "_mpso_time.csv", sep = "")
		mPSOdata <- rbind(mPSOdata, read.csv(filename_mpso, header = T, sep = ','))
		mPSOtime <- rbind(mPSOtime, read.csv(filename_mpso_time, header = T))
	}
	
	totalFrontNum <- nrow(mPSOdata)
	eachCoreNum <- floor(totalFrontNum/8) # 8 cores
	#best front
	tempBestfront <- foreach(iter = 1:8, .combine=rbind) %dopar%{
						if(iter == 8){
							best_front(mPSOdata[(1 + (iter - 1) * eachCoreNum):nrow(mPSOdata), ])
						} else{
							best_front(mPSOdata[(1 + (iter - 1) * eachCoreNum):(iter*eachCoreNum), ])
						}
					}
	best_mpso <- best_front(tempBestfront)
	names(best_mpso) <- c('costF', 'latencyF', 'rank')


	#prepare filename
	print("prepare writing file")
	filename <- paste('../result/', problem, '/best/', problem, '.csv', sep = '')
	mPSOtimeFilename <- paste('../result/', problem, '/best/', problem, "_mPSO_time.csv", sep='')

	write.csv(best_mpso[, 1:2], filename, row.names = F, quote = F)
	write.csv(mPSOtime, mPSOtimeFilename, row.names = F, quote = F)
}


