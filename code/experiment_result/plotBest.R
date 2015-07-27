plotme <- function(number = 1){
	if(number == 1){
		filename <- paste(number, '/', number, ".csv", sep = "")
		#initialisation_filename <- paste("3_3", "/", condition, "/", number, "_initialisation.csv", sep = "")
	}
	else if(number == 2){
		filename <- paste(number, '/', number, ".csv", sep = "")
		#initialisation_filename <- paste("5_5", "/", condition, "/", number, "_initialisation.csv", sep = "")
	}
	else if(number == 3){
		filename <- paste(number, '/', number, ".csv", sep = "")
		#initialisation_filename <- paste("10_10", "/", condition, "/", number, "_initialisation.csv", sep = "")
	}
	else if(number == 4){
		filename <- paste(number, '/', number, ".csv", sep = "")
		#initialisation_filename <- paste("15_15", "/", condition, "/", number, "_initialisation.csv", sep = "")
	}else{
		filename <- paste('./other/other.csv', sep = "")
	}
	pso_data <- read_pso_data(filename)
	nsga_data <- read_nsga_data(filename)
	#initialisation_nsga_data <- read_nsga_data(initialisation_filename)
	#initialisation_ga_data <- read_ga_data(initialisation_filename)
	par(font.axis = 2)
	#plot(nsga_data[order(nsga_data$latencyF),], xlim = c(left_x, right_x), ylim = c(left_y, right_y), col = 'blue', xlab = 'cost', ylab = 'latency',  pch = 20, type = 'b', font.lab = 2)
	plot(nsga_data[order(nsga_data$latencyF), 1:2], xlim = c(0, 1), ylim = c(0, 1), col = 'blue', xlab = 'cost', ylab = 'latency',  pch = 15, type = 'b', font.lab = 2)
	par(new = T)
	#plot(ga_data, xlim = c(left_x,right_x), ylim = c(left_y, right_y), col = 'red', xlab = '', ylab = '', pch = 4)
	plot(pso_data[order(pso_data$latencyF), 1:2], xlim = c(0, 1), ylim = c(0, 1), col = 'red', xlab = '', ylab = '', pch = 1, type = 'b')
	#par(new = T)
	#plot(initialisation_nsga_data[order(initialisation_nsga_data$latencyF),], xlim = c(0, 0.5), ylim = c(0, 0.7), col = 'green', xlab = '', ylab = '', pch = 17, type = 'b')
	#par(new = T)
	#plot(initialisation_ga_data, xlim = c(0, 0.5), ylim = c(0, 0.7), col = 'black', xlab = '', ylab = '', pch = 4)

	par(xpd = T)
	legend("topright",c("MOPSOCD", "NSGAII"), col = c("red", "blue"), pch = c(1, 15))
	#legend("topright",c("MOPSOCD", "NSGAII", "Initialized NSGA", "Initialized GA"), col = c("blue", "red", "green", "black"), pch = c(15, 1, 17, 4))

}

read_pso_data <- function(filename){
	data <- read.csv(filename, sep = ',', header = T)
	n <- nrow(data[data$alg == 1, ])
	return(data[1:n,])
}

read_nsga_data <- function(filename){
	data <- read.csv(filename, sep = ',', header = T)
	pso <- nrow(data[data$alg == 1, ])
	n <- nrow(data[data$alg == 2, ])
	return(data[(pso + 1):(pso + n),])
}

