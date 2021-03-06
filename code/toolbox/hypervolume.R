library(emoa)

update_hypervolume <- function(algorithms, problem = 1, final = T, generation = 1:50){
	value <- vector()
	generate_front <- function(front){
		temp <- front[complete.cases(front), ]
		return(unique(temp[, 1:2]))
	}
	#refFrontname <- paste("/home/st-james1/tanboxi/588_project/code/dataset/trueFront/", problem, ".csv", sep='')
	#refFront <- as.matrix(read.table(refFrontname, header = F, sep = ",")[, 1:2])
	refPoint <- c(1, 1)
	mean_sd <- function(algorithm, hv){
		mean_value <- mean(hv)
		standD <- sd(hv)
		cat(algorithm, " mean = ", mean_value, "standard Deviation: ", standD, '\n')
	}
	if(final == T){
		for(algorithm in algorithms){
			final_value <- vector()
			for(i in 1:40){
				filename <- paste("/home/st-james1/tanboxi/588_project/code/", algorithm, "/logData/", problem, '/', i, '/50/', 'front.csv', sep = "")
				front <- as.matrix(read.table(filename, header = T, sep = ','))
				value <- dominated_hypervolume(t(front), refPoint)
				final_value <- c(final_value, value)
			}
			mean_sd(algorithm, final_value)
			write.table(final_value, paste("/home/st-james1/tanboxi/588_project/code/", algorithm, "/result/", 
									 problem, "/best/hv_final.csv", sep = ''), quote = F, row.names = F, col.names = F, sep = ',')
		}
	} else {
		for(algorithm in algorithms){
			for(i in generation){
				rowvalue <- 0
				for(j in 1:40){
					filedir <- paste("/home/st-james1/tanboxi/588_project/code/", algorithm, "/logData/", sep = "")
					filename <- paste(filedir, problem, "/", j, "/", i, "/front.csv", sep = '')
					front <- as.matrix(read.table(filename, header = F, sep = ','))
					#front <- as.matrix(generate_front(front))
					#rowvalue <- c(rowvalue, hypervolume_indicator(t(front), t(refFront)))
					rowvalue <- c(rowvalue, dominated_hypervolume(t(front), refPoint))
				}
				value <- rbind(value, c(mean(rowvalue), i))
			}
			write.table(value, paste("/home/st-james1/tanboxi/588_project/code/", algorithm, "/result/", 
									 problem, "/best/hv.csv", sep = ''), quote = F, row.names = F, col.names = F, sep = ',')
			}
	}

}
