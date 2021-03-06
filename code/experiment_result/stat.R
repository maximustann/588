stat <- function(dataset){
	Psodata <- read_file(dataset, "PSO")
	NSGAdata <- read_file(dataset, "NSGA")
	mean_nsga <<- mean(NSGAdata$x)
	mean_pso <<- mean(Psodata$x)
	sd_nsga <- sd(NSGAdata$x)
	sd_pso <- sd(Psodata$x)
	cat("NSGA-II Mean : ", mean_nsga, "SD: ", sd_nsga, "\n")
	cat("PSO Mean : ", mean_pso, "SD: ", sd_pso, "\n")
}

read_file <- function(dataset, type){
	filename <- paste(dataset,'/', dataset, '_', type, '_time.csv', sep = "")
	data <- read.csv(filename, sep = ",", header = T)
	data
}

stat_plot <- function(){
	nsga_experiments <- vector()
	pso_experiments <- vector()
	for(i in 1:4){
		stat(i)
		nsga_experiments <- c(nsga_experiments, mean_nsga)
		pso_experiments <- c(pso_experiments, mean_pso)
	}
	nsga_experiments <- cbind(c(1:4), matrix(nsga_experiments, ncol = 1))
	pso_experiments <- cbind(c(1:4), matrix(pso_experiments, ncol = 1))
	
	plot(nsga_experiments, type='b', pch = 15, col = 'blue', ylim = c(0, 17000), 
		 ylab = "Process time (s)", xlab = 'Problems', axes = F)
	par(new = T)
	plot(pso_experiments, type='b', pch = 1, col = 'red', ylim = c(0, 17000), xlab='', ylab='', axes = F)
	legend("topleft", c("MOPSOCD", "NSGA-II"), col=c("red", "blue"), pch = c(1, 15))
	axis(1, at=1:4)
	axis(2, at= seq(0, 18000, by = 3000))
}
