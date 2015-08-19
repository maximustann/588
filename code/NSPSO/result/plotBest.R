plotnspso <- function(nunser = 1, color = "red", pchtype = 1){
	read_nspso_data <- function(filename){
		data <- read.csv(filename, sep = ',', header = T)
		n <- nrow(data)
		return(data[1:n,])
	}

	filename <- paste("/home/st-james1/tanboxi/588_project/code/NSPSO/result/",nunser, '/', nunser, ".csv", sep = "")
	nspso_data <- read_nspso_data(filename)
	par(font.axis = 2)
	#plot(ga_data, xlim = c(left_x,right_x), ylim = c(left_y, right_y), col = 'red', xlab = '', ylab = '', pch = 4)
	plot(nspso_data[order(nspso_data$latencyF), 1:2], xlim = c(0, 1), ylim = c(0, 1), col = color, xlab = 'cost', ylab = 'latency', pch = pchtype, type = 'b')

	#par(xpd = T)
	#legend("topright","BPSO", col = "red", pch = 1)

}




