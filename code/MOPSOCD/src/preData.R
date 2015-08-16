#source("DataExtraction.R")
library(Matrix)
predata <- function(noService=3, noCandLoc=3, noUserCen=3){
	set.seed(1)
	filename <- paste('../../dataset/', noCandLoc, '_', noUserCen, '.csv', sep='')
	latency_matrix <<- read.table(filename, sep=',', header = F)
	latency_matrix <<- as.matrix(latency_matrix, nrow = nrow(latency_matrix))
	#generate a matrix which column represent the candidate locations, and row represent the services
	#I assume in one location, the price for each service is identical, I guess that's a fair assumption
	#	[,1] [,2] [,3] [,4] [,5]
	#[1,]   62  119   94  120  108
	#[2,]   62  119   94  120  108
	#[3,]   62  119   94  120  108
	cost_matrix <<- matrix(
						   rep(floor(rnorm(noCandLoc, 100, 20)), noService),
						   nrow = noService,
						   byrow = T)



	frequency_matrix <<- matrix(
								data = floor(runif(noService * noUserCen, 1, 120)),
								nrow = noUserCen)
}

