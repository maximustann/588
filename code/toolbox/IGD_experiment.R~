source('IGD.R')
library(doMC)
registerDoMC(1)
igd_measures <- function(algorithms, problem = 1, generation = 1:50){
	generate_front <- function(front){
		temp <- front[complete.cases(front), ]
		return(unique(temp[, 1:2]))
	}
	
	igd_value <- vector()
	reference_front <- vector()

	refFrontfilename <- paste("/home/st-james1/tanboxi/588_project/code/dataset/trueFront/", problem, '.csv', sep = '')
	reference_front <- read.table(refFrontfilename, header = F, sep = ',')[, 1:2]


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
				print(igd_value)
			}
			write.table(igd_value, paste("/home/st-james1/tanboxi/588_project/code/", 
										 algorithm, "/result/", problem, "/best/igd.csv", sep = ""),
						quote = F, row.names = F, sep = ',')
		}
}
