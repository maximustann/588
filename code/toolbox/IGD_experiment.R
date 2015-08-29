source('IGD.R')
library(doMC)
registerDoMC(1)
igd_measures <- function(algorithms, problem = 1, final = T, generation = 1:50){
	generate_front <- function(front){
		temp <- front[complete.cases(front), ]
		return(unique(temp[, 1:2]))
	}
	
	igd_value <- vector()
	reference_front <- vector()

	refFrontfilename <- paste("/home/st-james1/tanboxi/588_project/code/dataset/trueFront/", problem, '.csv', sep = '')
	reference_front <- read.table(refFrontfilename, header = F, sep = ',')[, 1:2]
	mean_sd <- function(algorithm, igd){
		mean_value <- mean(igd)
		standD <- sd(igd)
		cat(algorithm, " mean = ", mean_value, "standard Deviation: ", standD, '\n')
	}

	if(final == T){
		for(algorithm in algorithms){
			igd_value <- vector()
			for(i in 1:40){
				frontName <- paste("/home/st-james1/tanboxi/588_project/code/", algorithm, '/logData/', problem, '/', i,'/50/', 'front.csv', sep = '')
				front <- read.table(frontName, header = T, sep = ',')
				igd_value <- c(igd_value, igd(front, reference_front))
			}
			mean_sd(algorithm, igd_value)
			write.table(igd_value, paste("/home/st-james1/tanboxi/588_project/code/", 
											 algorithm, "/result/", problem, "/best/igd_final.csv", sep = ""), quote = F, row.names = F, col.names = F, sep = ',')
		}
	} else {
		for(algorithm in algorithms){
			for(i in generation){
				rowvalue <- 0
				rowvalue <- foreach(j = 1:40) %dopar% {
					filedir <- paste("/home/st-james1/tanboxi/588_project/code/", algorithm, '/logData/', sep = '')
					filename <- paste(filedir, problem, "/", j, "/", i, "/front.csv", sep = '')
					front <- read.table(filename, header = F, sep = ',')[, 1:2]
					front <- generate_front(front)
					reference_front <- read.table(refFrontfilename, header = T, sep = ',')[, 1:2]
					igd(front, reference_front)
				}
				igd_value <- rbind(igd_value, c(mean(unlist(rowvalue)), i))
			}
			write.table(igd_value, paste("/home/st-james1/tanboxi/588_project/code/", 
										 algorithm, "/result/", problem, "/best/igd.csv", sep = ""),
						quote = F, row.names = F, sep = ',')
			}
	}

	




}

