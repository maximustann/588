library(nsga2R)

generateTrueFront <- function(algorithms = c("MOPSOCD", "binaryPSO", "NSGA2", "NSPSO"), problem = 1:14){
	findTrueFront <- function(problem){
		data <- vector()
		bestFront <- vector()
		for(algorithm in algorithms){
			filename <- paste('../', algorithm, '/result/', problem, '/best/', problem, '.csv', sep = '')
			data <- rbind(data, read.table(filename, header = T, sep = ',')[, 1:2])
		}
		ranking <- fastNonDominatedSorting(data)
		rnkIndex <- integer()
		i <- 1
		while(i <= length(ranking)){
			rnkIndex[ranking[[i]]] <- i
			i <- i + 1
		}
		data <- cbind(data, rnkIndex)
		bestFront <- data[data[, 3] == 1, 1:2]
		bestFront
	}
	for(i in problem){
		trueFront <- findTrueFront(i)
		write.table(trueFront, paste("../dataset/trueFront/", i, ".csv", sep = ''), row.names = F, col.names = F, quote = F, sep = ',')
	}
}
