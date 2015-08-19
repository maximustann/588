library(emoa)

update_hypervolume <- function(algorithms, result_or_log = "result", problem = 1, run = 1, generation = 1:50){
	value <- vector()
	generate_front <- function(front){
		temp <- front[complete.cases(front), ]
		return(unique(temp[, 1:2]))
	}
	refFrontname <- paste("../../dataset/trueFront/", problem, ".csv", sep='')
	refFront <- as.matrix(read.table(refFrontname, header = T, sep = ",")[, 1:2])

	for(algorithm in algorithms){
		if(result_or_log == "result"){
			filedir <- paste("../../", algorithm, "/result/", problem, "/best/", sep = "")
			filename <- paste(filedir, problem, ".csv", sep = "")
			front <- as.matrix(read.table(filename, header = T, sep = ',')[, 1:2])
			value <- hypervolume_indicator(t(front), t(refFront))
			write.table(value, paste(filedir, "hv.csv", sep=''), row.names = F, quote = F, sep = ',')
		}else {
			for(i in generation){
				filedir <- paste("../", algorithm, "/logData/", sep = "")
				filename <- paste(filedir, problem, "/", run, "/", i, "/fitness.csv", sep = '')
				front <- read.table(filename, header = T, sep = ',')
				front <- as.matrix(generate_front(front))
				value <- c(value, hypervolume_indicator(t(front), t(refFront)))
				write.table(value, paste("../../", algorithm, "/result/", problem, "/run/hv.csv", sep = ''), quote = F, row.names = F, sep = ',')
			}
		}
	}
}
