plotmpso <- function(number = 1, color = "red", pchtype = 1){
	read_mpso_data <- function(filename){
		data <- read.csv(filename, sep = ',', header = T)
		n <- nrow(data)
		return(data[1:n,])
	}


	filename <- paste("/home/st-james1/tanboxi/588_project/code/MOPSOCD/result/",number, '/', number, '.csv', sep = "")
	mpso_data <- read_mpso_data(filename)
	par(font.axis = 2)
	plot(mpso_data[order(mpso_data$latencyF), 1:2], xlim = c(0, 1), ylim = c(0, 1), col = color, xlab = 'cost', ylab = 'latency', pch = pchtype, type = 'b')
	#legend("topright","MOPSOCD", col = "red", pch = 1)
}



