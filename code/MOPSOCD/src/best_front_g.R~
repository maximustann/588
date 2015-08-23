library(nsga2R)
library(doMC)
library(mco)
registerDoMC(8)


gen_best_front <- function(problem = 5, iter = 40, maxgen = 50){

	best_front <- function(data){
		temp <- unique(data[, 1:2])
		front <- vector()
		best_front <- vector()

		#varify the data, if not valid, add a fake one
		if(nrow(temp) == 1){
			temp <- rbind(temp, c(temp[1,1] + 0.01, temp[1, 2] + 0.01))
		}

		ranking <- fastNonDominatedSorting(temp)
		rnkIndex <- integer()
		bad_kids <- NULL



		i <- 1
		while(i <= length(ranking)){
			rnkIndex[ranking[[i]]] <- i
			i <- i + 1
		}

		front <- cbind(temp, rnkIndex)
		best_front <- front[front[, 3] == 1, ]

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


	path <- paste("/home/st-james1/tanboxi/588_project/code/MOPSOCD/logData/", problem, '/', sep = '')
	foreach(i = 1:iter) %dopar% {
		cat("iter", i, '\n')
		iterDirect <- paste(path, i, '/', sep = '')
		for(j in 1:maxgen){
			genDirect <- paste(iterDirect, j, '/', sep = '')
			filename <- paste(genDirect, 'fitness.csv', sep='')
			front_filename <- paste(genDirect, 'front.csv', sep='')
			data <- read.csv(filename, header = T, sep = ',')
			data <- data[complete.cases(data), ]
			front <- best_front(data)
			names(front) <- c("costF", "latencyF", "rnkIndex")
			write.csv(front, front_filename, row.names = F, quote = F)
		}
	}
	
	print("Finished")

}


