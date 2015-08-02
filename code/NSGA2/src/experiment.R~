source("predata.R")
source("nsga2.R")
source("ga.R")
library(GA)
library(mco)
library(nsga2R)
library(foreach)
library(doMC)
registerDoMC(8)

run <- function(initialisation = F){
	#-------------------------------Set up Problem Start-----------------------------
	if(problem == 1){
		noService <- 20
		noCandLoc <- 5
		noUserCen <- 10
		limitation <- 10000
	} else if(problem == 2){
		noService <- 50
		noCandLoc <- 15
		noUserCen <- 20
		limitation <- 75000
	} else if(problem == 3){
		noService <- 100
		noCandLoc <- 25
		noUserCen <- 40
		limitation <- 250000
	} else if(problem == 4){
		noService <- 200
		noCandLoc <- 40
		noUserCen <- 80
		limitation <- 800000
	} else {
		noService <- 3
		noCandLoc <- 3
		noUserCen <- 3
		limitation <- 900
	}

	#-------------------------------Set up Problem End-----------------------------
	NSGAdata <- vector()
	GAdata <- vector()
	NSGAtime<- vector()
	GAtime<- vector()

	#------------------------------Proprocessing Functions Start------------------------
	search_maximum_latency <- function(noService, noCandLoc, noUserCen){
		initial_matrix <- matrix(c(rep(1, noService), rep(0, noService * (noCandLoc - 1))), nrow = noService)
		latency <- 0.0
		for(row_iter in 1:noService){
			for(col_iter in 1:noCandLoc){
				if(row_iter == 1 && col_iter == 1){
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
	
	search_minimum_cost <- function(noService, noCandLoc){
		matrixSize <- noService * noCandLoc
		mini_cost_matrix <- matrix(rep(0, matrixSize), nrow = noService)
		for(row_iter in 1:noService){
			pos <- which(cost_matrix[row_iter, ] == min(cost_matrix[row_iter, ]))[1]
			mini_cost_matrix[row_iter, pos] <- 1
		}
		mini_cost_matrix
	}

	best_front <- function(gadata){
		temp <- unique(gadata[, 1:2])
		ga <- vector()
		best_ga <- vector()
		ranking <- fastNonDominatedSorting(temp)
		rnkIndex <- integer()
		i <- 1
		while (i <= length(ranking)){
			rnkIndex[ranking[[i]]] <- i
			i <- i + 1
		}
		ga <- cbind(temp, rnkIndex)
		best_ga <- ga[ga[, 3] == 1,]
		print(best_ga)
		best_ga
	}
	#------------------------------Proprocessing Functions End------------------------
	#------------------------------Preprocessing Start-------------------------------
	predata(noService, noCandLoc, noUserCen)
	max_cost <- unNormalized_cost_fitness(matrix(rep(1, matrixSize * matrixSize), nrow = matrixSize), matrixSize)
	min_cost <- unNormalized_cost_fitness(search_minimum_cost(matrixSize), matrixSize)
	max_latency <- search_greatest_latency(matrixSize)
	min_latency <- 0
	#------------------------------Preprocessing End--------------------------------
	foreach(iter = 1:40) %dopar%{
		cat("Generation:", iter, '\n')
		NSGAdata <- run_algorithm(matrixSize, iter, cost_limitation, max_cost, min_cost, max_latency, min_latency, initialisation)
		
		if(initialisation == T){

			NSGAtime <- NSGAdata[1, 3]
			filename_nsga <- paste(iter, "_initialisation_nsga.csv", sep = "")
			filename_nsga_time <- paste(iter, "_initialisation_time_nsga.csv", sep = "")
			write.csv(NSGAdata[, 1:2], filename_nsga, quote = F, row.names = F)
			write.csv(NSGAtime, filename_nsga_time, quote = F, row.names = F)
		}
		else{
			NSGAtime <- NSGAdata[1, 3]
			filename_nsga <- paste(iter, "_nsga.csv", sep = "")
			filename_nsga_time <- paste(iter, "_time_nsga.csv", sep = "")
			write.csv(NSGAdata[, 1:2], filename_nsga, quote = F, row.names = F)
			write.csv(NSGAtime, filename_nsga_time, quote = F, row.names = F)
		}

	}

	for(iter in 1:40){
		if(initialisation == T){
			filename_nsga <- paste(iter, "_initialisation_nsga.csv", sep = "")
			filename_nsga_time <- paste(iter, "_initialisation_time_nsga.csv", sep = "")
			NSGAdata <- rbind(NSGAdata, read.csv(filename_nsga, header = T, sep = ','))
			NSGAtime <- rbind(NSGAtime, read.csv(filename_nsga_time, header = T, sep = ","))
		}
		else{
			filename_nsga <- paste(iter, "_nsga.csv", sep = "")
			filename_nsga_time <- paste(iter, "_time_nsga.csv", sep = "")
			NSGAdata <- rbind(NSGAdata, read.csv(filename_nsga, header = T, sep = ','))
			NSGAtime <- rbind(NSGAtime, read.csv(filename_nsga_time, header = T, sep = ","))
		}
	}

	time_data <- rbind(NSGAtime, GAtime)
	best_nsga <- best_front(NSGAdata)
	data <- rbind(best_nsga[, 1:2], GAdata)
	colnames(data) <- c("costF", "latencyF")
	if(initialisation == T){
		filename <- paste(matrixSize, "_initialisation.csv", sep = "")
		filename_time <- paste(matrixSize, "_initialisation_time.csv", sep="")
	}
	else{
		filename <- paste(matrixSize, ".csv", sep = "")
		filename_time <- paste(matrixSize, "_time.csv", sep="")
	}
	write.csv(data, filename, quote = F, row.names = F)
	write.csv(time_data, filename_time, quote = F, row.names = F)
}
