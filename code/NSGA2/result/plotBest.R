plotme <- function(number = 1){
	filename <- paste(number, '/', number, '.csv', sep = "")
	data <- read_data(filename)
	#initialisation_nsga_data <- read_nsga_data(initialisation_filename)
	#initialisation_ga_data <- read_ga_data(initialisation_filename)
	par(font.axis = 2)
	#plot(nsga_data[order(nsga_data$latencyF),], xlim = c(left_x, right_x), ylim = c(left_y, right_y), col = 'blue', xlab = 'cost', ylab = 'latency',  pch = 20, type = 'b', font.lab = 2)
	#plot(ga_data, xlim = c(left_x,right_x), ylim = c(left_y, right_y), col = 'red', xlab = '', ylab = '', pch = 4)
	plot(pso_data[order(data$latencyF), 1:2], xlim = c(0, 1), ylim = c(0, 1), col = 'red', xlab = 'cost', ylab = 'latency', pch = 1, type = 'b')
	#par(new = T)
	#plot(initialisation_nsga_data[order(initialisation_nsga_data$latencyF),], xlim = c(0, 0.5), ylim = c(0, 0.7), col = 'green', xlab = '', ylab = '', pch = 17, type = 'b')
	#par(new = T)
	#plot(initialisation_ga_data, xlim = c(0, 0.5), ylim = c(0, 0.7), col = 'black', xlab = '', ylab = '', pch = 4)

	par(xpd = T)
	legend("topright","NSGAII", col = "red", pch = 1)
	#legend("topright",c("MOPSOCD", "NSGAII", "Initialized NSGA", "Initialized GA"), col = c("blue", "red", "green", "black"), pch = c(15, 1, 17, 4))

}

read_pso_data <- function(filename){
	data <- read.csv(filename, sep = ',', header = T)
	n <- nrow(data)
	return(data[1:n,])
}
