plotnsga <- function(problem = 1, color = "red", pchtype = 1){
	read_data <- function(filename){
		data <- read.csv(filename, sep = ',', header = T)
		n <- nrow(data)
		return(data[1:n,])
	}

	filename <- paste("./", problem, '/', problem, '.csv', sep = "")
	nsga_data <- read_data(filename)
	par(font.axis = 2)
	plot(nsga_data[order(nsga_data$latencyF), 1:2], xlim = c(0, 1), ylim = c(0, 1), col = 'red', xlab = 'cost', ylab = 'latency', pch = 1, type = 'b')
}



