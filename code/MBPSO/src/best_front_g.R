library(nsga2R)
library(doMC)
library(mco)
registerDoMC(8)


gen_best_front <- function(problem = 5, iter = 40, maxgen = 50){

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


	path <- paste("../logData/", problem, '/', sep = '')
	foreach(i = 1:iter) %dopar% {
		iterDirect <- paste(path, i, '/', sep = '')
		for(j in 1:maxgen){
			genDirect <- paste(iterDirect, j, '/', sep = '')
			filename <- paste(genDirect, 'fitness.csv', sep='')
			front_filename <- paste(genDirect, 'front.csv', sep='')
			data <- read.csv(filename, header = T, sep = ',')
			front <- best_front(data)
			names(front) <- c("costF", "latencyF", "rnkIndex")
			write.csv(front, front_filename, row.names = F, quote = F)
		}
	}
	
	print("Finished")

}


