
plot_gen <- function(algorithm, problem = 1, iter = 1, maxgen = 50){
	path <- paste("../", algorithm, "/logData/", problem, '/', iter, '/', sep='')

	mydata <- list()
	for(i in 1:maxgen){
		filename <- paste(path, i, '/', "front.csv", sep='')
		data <- read.csv(filename, header = T, sep=',')
		mydata[[i]] <- data
	}

	for(i in 1:maxgen){
		Sys.sleep(0.1)
		gen <- mydata[[i]]
		#print(gen)
		plot(gen[order(gen$latencyF), 1:2], xlim = c(0, 1), ylim = c(0, 1), 
		 			col = 'red', xlab = 'cost', ylab = 'latency', pch = 1, type = 'b')
		#par(new = T)
	}

}

