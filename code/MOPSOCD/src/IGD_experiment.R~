source('IGD.R')
library(nsga2R)

igd_measures <- function(problem = 1, run = 1, generation = 1:50){

	generate_front <- function(front){
		temp <- front[complete.cases(front), ]
		return(unique(temp[, 1:2]))
	}
	igd_value <- vector()
	reference_front <- vector()
	for(i in generation){
		frontfilename <- paste("../logData/", problem, '/', run, '/', i, '/', 'fitness.csv', sep = '')
		refFrontfilename <- paste("../result/", problem, "/", problem, '.csv', sep = '')
		front <- read.table(frontfilename, header = T, sep = ',')[, 1:2]
		front <- generate_front(front)
		reference_front <- read.table(refFrontfilename, header = T, sep = ',')[, 1:2]
		#reference_front <- reference_front[order(reference_front[, 2]), ]
		#front <- front[order(front[, 2]), ]
		igd_value <- c(igd_value, igd_i(front, reference_front))
	}

	igd_value
}
