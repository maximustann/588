source("preData.R")
source("nsga2.R")
library(mco)
library(nsga2R)
library(foreach)
library(doMC)
registerDoMC(8)

run <- function(problem = 5, logS = F, initialisation = F){
	#-------------------------------Set up Problem Start-----------------------------
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
		best_ga
	}
	#------------------------------Proprocessing Functions End------------------------
	#------------------------------Preprocessing Start-------------------------------
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
	#------------------------------Preprocessing End--------------------------------
	print("Start")
	dir.create(paste('/home/st-james1/tanboxi/588_project/code/NSGA2/result/', problem, '/run/', sep = ''), showWarnings = F)
	dir.create(paste('/home/st-james1/tanboxi/588_project/code/NSGA2/result/', problem, '/best/', sep = ''), showWarnings = F)
	foreach(iter = 1:40) %dopar%{
		cat("run:", iter, '\n')

		NSGAdata <- run_algorithm(noService, noCandLoc, noUserCen, iter, limitation, max_cost, min_cost, max_latency, min_latency, problem, logS, initialisation = F)
		
		if(initialisation == T){

			NSGAtime <- NSGAdata[1, 3]
			filename_nsga <- paste("/home/st-james1/tanboxi/588_project/code/NSGA2/result/", problem, '/', iter, "_initialisation_nsga.csv", sep = "")
			filename_nsga_time <- paste("../result/", problem, '/', iter, "_initialisation_time_nsga.csv", sep = "")
			write.csv(NSGAdata[, 1:2], filename_nsga, quote = F, row.names = F)
			write.csv(NSGAtime, filename_nsga_time, quote = F, row.names = F)
		}
		else{
			NSGAtime <- NSGAdata[1, 3]
			filename_nsga <- paste('/home/st-james1/tanboxi/588_project/code/NSGA2/result/', problem, '/run/', iter, "_nsga.csv", sep = "")
			filename_nsga_time <- paste("/home/st-james1/tanboxi/588_project/code/NSGA2/result/", problem,'/run/', iter, "_time_nsga.csv", sep = "")
			write.csv(NSGAdata[, 1:2], filename_nsga, quote = F, row.names = F)
			write.csv(NSGAtime, filename_nsga_time, quote = F, row.names = F)
		}

	}

	for(iter in 1:40){
		if(initialisation == T){
			filename_nsga <- paste('/home/st-james1/tanboxi/588_project/code/NSGA2/result/', problem, '/run/', iter, "_initialisation_nsga.csv", sep = "")
			filename_nsga_time <- paste('/home/st-james1/tanboxi/588_project/code/NSGA2/result/', problem, '/run/', iter, "_initialisation_time_nsga.csv", sep = "")
			NSGAdata <- rbind(NSGAdata, read.csv(filename_nsga, header = T, sep = ','))
			NSGAtime <- rbind(NSGAtime, read.csv(filename_nsga_time, header = T, sep = ","))
		}
		else{
			filename_nsga <- paste('/home/st-james1/tanboxi/588_project/code/NSGA2/result/', problem, '/run/', iter, "_nsga.csv", sep = "")
			filename_nsga_time <- paste('/home/st-james1/tanboxi/588_project/code/NSGA2/result/', problem, '/run/', iter, "_time_nsga.csv", sep = "")
			NSGAdata <- rbind(NSGAdata, read.csv(filename_nsga, header = T, sep = ','))
			NSGAtime <- rbind(NSGAtime, read.csv(filename_nsga_time, header = T, sep = ","))
		}
	}
	
	totalFrontNum <- nrow(NSGAdata)
	eachCoreNum <- floor(totalFrontNum/8) # 8 cores
	#best front
	tempBestfront <- foreach(iter = 1:8, .combine=rbind) %dopar% {
		if(iter == 8){
			best_front(NSGAdata[(1 + (iter - 1) * eachCoreNum):nrow(NSGAdata), ])
		} else{
			best_front(NSGAdata[(1 + (iter - 1) * eachCoreNum):(iter*eachCoreNum), ])
		}
	}
	time_data <- rbind(NSGAtime, GAtime)
	best_nsga <- best_front(tempBestfront)
	data <- rbind(best_nsga[, 1:2], GAdata)
	colnames(data) <- c("costF", "latencyF")
	if(initialisation == T){
		filename <- paste('/home/st-james1/tanboxi/588_project/code/NSGA2/result/', problem, '/', problem, "_initialisation.csv", sep = "")
		filename_time <- paste('/home/st-james1/tanboxi/588_project/code/NSGA2/result/', problem, '/', problem, "_initialisation_time.csv", sep = "")
	}
	else{
		filename <- paste('/home/st-james1/tanboxi/588_project/code/NSGA2/result/', problem, '/best/', problem,  ".csv", sep = "")
		filename_time <- paste('/home/st-james1/tanboxi/588_project/code/NSGA2/result/', problem, '/best/', problem, "_time.csv", sep = "")
	}
	write.csv(data, filename, quote = F, row.names = F)
	write.csv(time_data, filename_time, quote = F, row.names = F)
}
