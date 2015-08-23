



plotAll <- function(algorithms, problem, trueFront = F){

	data <- list()
	
	i <- 1
	max_value <- c(0, 0)
	color <- c("red", "blue", "darkgreen", "purple")
	pc <- c(0, 1, 4, 17)
	# read data
	for(algorithm in algorithms){
		filename <- paste("~/588_project/code/", algorithm, "/result/", problem, "/best/", problem, ".csv", sep = '')
		data[[i]] <- read.table(filename, sep =',', header = T)
		temp <- data[[i]][which(data[[i]][,2] == max(data[[i]][, 2])),] # find the max value
		if(temp[2] > max_value[2]) max_value <- temp
		i <- i + 1
	}


	plotSingle <- function(algorithm, problem, color = "red", pchtype = 1, data, max_value, count){
		plot(data[order(data[, 2]), 1:2], xlim = c(0, 1), ylim = c(0, max_value[, 2]), col = color, xlab = 'cost', ylab = 'latency', pch = pchtype, type = 'b')
	}
	trueFrontfilename <- paste("~/588_project/code/dataset/trueFront/", problem, ".csv", sep = '')
	front <- read.table(trueFrontfilename, sep = ',', header = F)
	#par(new = T)

	i <- 1
	for(algorithm in algorithms){
		plotSingle(algorithm, problem, color[i], pc[i], data[[i]], max_value, i)
		par(new = T)
		i <- i + 1
	}

	if(trueFront == T){
		plot(front[order(front[, 2]), 1:2], xlim = c(0, 1), ylim = c(0, max_value[, 2]), col = "black", xlab = 'cost', 
			 ylab = 'latency', pch = 3, type = 'b')
	}
	par(xpd = T)
	if(trueFront){
		fcolor <- c(color[1:length(algorithms)], "black")
		fpc <- c(pc[1:length(algorithms)], 3)
		algorithms <- c(algorithms, "trueFront")
		legend("topright", algorithms, col = fcolor[1:length(fcolor)], pch = fpc[1:length(algorithms)])
	} else {
		legend("topright", algorithms, col = color[1:length(algorithms)], pch = pc[1:length(algorithms)])
	}
}
