source('IGD.R')
library(nsga2R)

igd_measures <- function(algorithms, problem = 1, result_or_log = "result", run = 1, generation = 1:50){
	generate_front <- function(front){
		temp <- front[complete.cases(front), ]
		return(unique(temp[, 1:2]))
	}
	
	igd_value <- vector()
	reference_front <- vector()

	refFrontfilename <- paste("../../dataset/trueFront/", problem, '.csv', sep = '')
	reference_front <- read.table(refFrontfilename, header = T, sep = ',')[, 1:2]


	for(algorithm in algorithms){
		if(result_or_log == "result"){
			filedir <- paste("../../", algorithm, "/result/", problem, "/best/", sep ='')
			filename <- paste(filedir, problem, ".csv", sep='')
			front <- read.table(filename, header = T, sep = ',')[, 1:2]
			igd_value <- igd_i(front, reference_front)
			write.table(igd_value, paste(filedir, "igd.csv", sep=''), row.names = F, quote = F, sep = ',')
		}else{
			for(i in generation){
				filedir <- paste("../", algorithm, '/logData/', sep = '')
				filename <- paste(filedir, problem, "/", run, "/", i, "/fitness.csv", sep = '')
				front <- read.table(filename, header = T, sep = ',')[, 1:2]
				front <- generate_front(front)
				reference_front <- read.table(refFrontfilename, header = T, sep = ',')[, 1:2]
				igd_value <- c(igd_value, igd(front, reference_front))
				write.table(igd_value, paste("../../", algorithm, "/result/", problem, "/run/igd.csv", sep = ""),
							quote = F, row.names = F, sep = ',')
			}
		}
	}
}
