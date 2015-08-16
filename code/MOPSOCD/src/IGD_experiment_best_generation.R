source('IGD.R')
library(nsga2R)

igd_measures <- function(problem = 1, generation = 1:50){

	generation_best_front <- function(problem, generation){
		data <- data.frame()
		for(i in 1:40){
			ref_file_name <- paste("../logData/", problem, '/', i, '/',generation, '/fitness.csv', sep = "")
			subdata <- read.table(ref_file_name, header = T, sep = ',')
			data <- rbind(data, subdata[complete.cases(subdata), ])
		}
		temp <- unique(data[, 1:2])
		front <- vector()
		bad_kids <- NULL
		best_front <- vector()
		ranking <- fastNonDominatedSorting(temp)
		rnkIndex <- integer()
		i <- 1
		while(i <= length(ranking)){
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
	
	igd_value <- vector()
	reference_front <- vector()
	for(i in generation){
		frontfilename <- paste("../logData/", problem, "/best_front_each_generation/", i, '.csv', sep = '')
		refFrontfilename <- paste("../result/", problem, "/", problem, '.csv', sep = '')
		if(file.exists(frontfilename)){
			front <- read.table(frontfilename, header = T, sep = ',')
		}else{
			front <- generation_best_front(problem, i)[, 1:2]
			write.table(front, frontfilename, row.names = F, quote = F, sep = ',')
		}
		reference_front <- read.table(refFrontfilename, header = T, sep = ',')[, 1:2]
		reference_front <- reference_front[order(reference_front[, 2]), ]
		front <- front[order(front[, 2]), ]
		igd_value <- c(igd_value, igd_i(front, reference_front))
	}

	igd_value
}
