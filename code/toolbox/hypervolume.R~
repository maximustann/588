library(emoa)

update_hypervolume <- function(algorithms, problem = 1, generation = 1:50){
	value <- vector()
	generate_front <- function(front){
		temp <- front[complete.cases(front), ]
		return(unique(temp[, 1:2]))
	}
	refFrontname <- paste("/home/st-james1/tanboxi/588_project/code/dataset/trueFront/", problem, ".csv", sep='')
	refFront <- as.matrix(read.table(refFrontname, header = F, sep = ",")[, 1:2])

	for(algorithm in algorithms){
		for(i in generation){
			rowvalue <- 0
			for(j in 1:40){
				filedir <- paste("/home/st-james1/tanboxi/588_project/code/", algorithm, "/logData/", sep = "")
				filename <- paste(filedir, problem, "/", j, "/", i, "/front.csv", sep = '')
				front <- read.table(filename, header = F, sep = ',')
				front <- as.matrix(generate_front(front))
				rowvalue <- c(rowvalue, hypervolume_indicator(t(front), t(refFront)))
			}
			value <- rbind(value, c(mean(rowvalue), i))
		}
		write.table(value, paste("/home/st-james1/tanboxi/588_project/code/", algorithm, "/result/", 
								 problem, "/best/hv.csv", sep = ''), quote = F, row.names = F, col.names = F, sep = ',')
		}
}
