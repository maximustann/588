library(doMC)
library(nsga2R)
registerDoMC(8)
source('preData.R')

source("bpso.R")
run <- function(problem = 5, logS = F){

	#-------------------Set up Problem Start----------------------------
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

	#-------------------Set up Problem End----------------------------



	varcount <- noService * noCandLoc
	lbound <- rep(0, varcount)
	ubound <- rep(1, varcount)
	maxgen <- 50
	popSize <- 50
	weight <- 0.5
	obj <- 2
	c1 <- 2
	c2 <- 2

	#-----------------------------Fitness Functions Start-----------------------------------
	unNormalized_latency_fitness <- function(particle, noService, noCandLoc, noUserCen){
		latency <- 0.0
		particle <- matrix(particle, nrow = noService)
		frequency <- colSums(frequency_matrix)
		response_matrix <- vector()
		for(user_iter in 1:noUserCen){
			for(service_iter in 1:noService){
				num_of_service <- sum(particle[service_iter, ])
				deployed_service <- which(particle[service_iter, ] == 1)
				if(num_of_service > 1){
					response_matrix <- c(response_matrix, min(latency_matrix[user_iter, deployed_service]))
				} else {
					response_matrix <- c(response_matrix, latency_matrix[user_iter, deployed_service])
				}
			}
		}
		response_matrix <- matrix(response_matrix, nrow = noUserCen, byrow = T)
		if(ncol(response_matrix) < ncol(frequency_matrix)){
			#Death penalty of latency
			return(max_latency)
		}
		latency <- sum(response_matrix * frequency_matrix)
		latency
	}

	normalized_latency_fitness <- function(particle, noService, noCandLoc, noUserCen, max_latency, min_latency){
		latency <- unNormalized_latency_fitness(particle, noService, noCandLoc, noUserCen)
		latency <- normalize(latency, max_latency, min_latency)
		latency
	}

	normalize <- function(value, max_value, min_value){
		normalized_data <- vector()
		normalized_data <- (value - min_value) / (max_value - min_value)
		normalized_data
	}

	unNormalized_cost_fitness <- function(particle, noService, noCandLoc){
		particle <- matrix(particle, nrow = noService)
		cost <- sum(particle * cost_matrix)
		if(validation(particle, noService) == F){
			#death penalty of cost
			return(max_cost)
		}
		cost
	}

	normalized_cost_fitness <- function(particle, noService, noCandLoc, max_cost, min_cost){
		cost <- unNormalized_cost_fitness(particle, noService, noCandLoc)
		cost <- normalize(cost, max_cost, min_cost)
		cost
	}

	fitFunc <- function(particle) {
		cost <- normalized_cost_fitness(particle, noService, noCandLoc, max_cost, min_cost)
		latency <- normalized_latency_fitness(particle, noService, noCandLoc, noUserCen, max_latency, min_latency)
		return(c(cost, latency))
	}

	validation <- function(particle, noService){
		particle <- matrix(particle, nrow = noService)
		if(prod(rowSums(particle)) == 0){
			return(F)
		}
		return(T)
	}
	#-----------------------------Fitness Functions End-------------------------------------


	#-----------------------------Use fast Non-Dominated Sorting Start----------------------
	best_front <- function(data){
		temp <- unique(data[, 1:2])
		front <- vector()
		best_front <- vector()
		ranking <- fastNonDominatedSorting(temp)
		rnkIndex <- integer()
		i <- 1
		while(i <= length(ranking)){
			rnkIndex[ranking[[i]]] <- i
			i <- i + 1
		}
		front <- cbind(temp, rnkIndex)
		best_front <- front[front[, 3] == 1, ]
		best_front
	}
	#-----------------------------Use fast Non-Dominated Sorting End----------------------

	#====================pre-processing functions Start===============================================
	search_maximum_latency <- function(noService, noCandLoc, noUserCen){
		matrixSize <- noService * noCandLoc
		initial_matrix <- matrix(c(rep(1, noService), rep(0, noService * (noCandLoc - 1))), nrow = noService)
		latency <- 0.0
		for(row_iter in 1:noService){
			for(col_iter in 1:noCandLoc){
				if(row_iter == 1 && col_iter == 1){
					#first time
					latency <- unNormalized_latency_fitness(initial_matrix, noService, noCandLoc, noUserCen)
					next
				}
				num <- which(initial_matrix[row_iter, ] == 1)
				initial_matrix[row_iter, num] <- 0
				initial_matrix[row_iter, col_iter] <- 1
				current_latency <- unNormalized_latency_fitness(initial_matrix, noService, noCandLoc, noUserCen)
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

	search_minimum_latency <- function(noService, noCandLoc, noUserCen){
			minimum_latency <- 0
			#ideal situation
			chromosome <- matrix(rep(1, noService * noCandLoc), nrow = noService)
			minimum_latency <- unNormalized_latency_fitness(chromosome, noService, noCandLoc, noUserCen)
			minimum_latency
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
	#====================pre-processing functions End===============================================

	#---------------Pre-processing Start-----------------
	predata(noService, noCandLoc, noUserCen)
	normFilename <- paste("/home/st-james1/tanboxi/588_project/code/dataset/norm", noService, "_", noCandLoc, "_", noUserCen, ".csv", sep = '')
	if(file.exists(normFilename)){
		normData <- as.vector(as.matrix(read.table(normFilename, header = F, sep = ',')))
		#print(normData)
		min_latency <- normData[1]
		max_latency <- normData[2]
		min_cost <- normData[3]
		max_cost <- normData[4]
	} else{
		min_latency <- search_minimum_latency(noService, noCandLoc, noUserCen, threshold)
		max_latency <- search_maximum_latency(noService, noCandLoc, noUserCen, threshold)

		min_cost <- sum(search_minimum_cost(noService, noCandLoc) * cost_matrix)
		max_cost <- sum(cost_matrix)
		write.table(c(min_latency, max_latency, min_cost, max_cost), normFilename, row.names = F, col.names = F, sep = ',')
	}
	
	#---------------Pre-processing End-------------------

	#---------------Run Algorithm Start---------------------
	bPSOdata <- vector()
	bPSOtime <- vector()

	#fire on all 8 cores
	print("Start")
	dir.create(paste('/home/st-james1/tanboxi/588_project/code/binaryPSO/result/', problem, '/run/', sep = ''), showWarnings = F)
	dir.create(paste('/home/st-james1/tanboxi/588_project/code/binaryPSO/result/', problem, '/best/', sep = ''), showWarnings = F)
	foreach(iter = 1:40) %dopar%{
		cat("run: ", iter,"\n")
		filename_bpso_time <- paste('/home/st-james1/tanboxi/588_project/code/binaryPSO/result/', problem, '/run/', iter, '_bpso_time.csv', sep = '')
		filename_bpso <- paste('/home/st-james1/tanboxi/588_project/code/binaryPSO/result/', problem, '/run/', iter, '_bpso.csv', sep = '')
		ptm <- proc.time()
		bPSOdata <- bpso(fitFunc, varcount, obj, lbound, ubound, c1, c2, maxgen, weight, popSize, iter, problem, logS)
		bPSOtime <- c(bPSOtime, (proc.time() - ptm)[1])
		write.csv(bPSOdata, filename_bpso, row.names = F, quote = F)
		write.csv(bPSOtime, filename_bpso_time, row.names = F, quote = F)
	}
	print("Compiling...")
	for(iter in 1:40){
		filename_bpso <- paste('/home/st-james1/tanboxi/588_project/code/binaryPSO/result/', problem, '/run/', iter, "_bpso.csv", sep = "")
		filename_bpso_time <- paste('/home/st-james1/tanboxi/588_project/code/binaryPSO/result/', problem, '/run/', iter, '_bpso_time.csv', sep = "")
		bPSOdata <- rbind(bPSOdata, read.csv(filename_bpso, header = T, sep = ','))
		bPSOtime <- rbind(bPSOtime, read.csv(filename_bpso_time, header = T))
	}
	totalFrontNum <- nrow(bPSOdata)
	eachCoreNum <- floor(totalFrontNum/8) #8 cores
	#best front
	tempBestfront <- foreach(iter = 1:8, .combine=rbind) %dopar%{
					if(iter == 8){
						best_front(bPSOdata[(1 + (iter - 1) * eachCoreNum):nrow(bPSOdata), ])
					} else{
						best_front(bPSOdata[(1 + (iter - 1) * eachCoreNum):(iter*eachCoreNum), ])
					}
				}
	best_bpso <- best_front(tempBestfront)
	names(best_bpso) <- c('costF', 'latencyF', 'rank')

	#prepare file name 
	filename <- paste('/home/st-james1/tanboxi/588_project/code/binaryPSO/result/', problem, '/best/', problem, '.csv', sep = '')
	timeFilename <- paste('/home/st-james1/tanboxi/588_project/code/binaryPSO/result/', problem, '/best/', problem,'_time.csv', sep = '')

	#write final result
	write.csv(best_bpso[, 1:2], filename, row.names = F, quote = F)
	write.csv(bPSOdata, timeFilename, row.names = F, quote = F)
	#---------------Run Algorithm End---------------------
}


