#source("DataExtraction.R")
library(Matrix)
predata <- function(noService=3, noCandLoc=3, noUserCen=3){
	set.seed(1)
	latencyfilename <- paste('/home/st-james1/tanboxi/588_project/code/dataset/', noCandLoc, '_', noUserCen, '.csv', sep='')
	latency_matrix <<- read.table(latencyfilename, sep=',', header = F)
	latency_matrix <<- as.matrix(latency_matrix, nrow = nrow(latency_matrix))
	#generate a matrix which column represent the candidate locations, and row represent the services
	#I assume in one location, the price for each service is identical, I guess that's a fair assumption
	#	[,1] [,2] [,3] [,4] [,5]
	#[1,]   62  119   94  120  108
	#[2,]   62  119   94  120  108
	#[3,]   62  119   94  120  108
	costfilename <- paste('/home/st-james1/tanboxi/588_project/code/dataset/cost_', noService, '_', noCandLoc, '.csv', sep='')
	cost_matrix <<- read.table(costfilename, sep=',', header = F)
	cost_matrix <<- as.matrix(cost_matrix, nrow = nrow(cost_matrix))



	freqfilename <- paste('/home/st-james1/tanboxi/588_project/code/dataset/freq_', noService, '_', noUserCen, '.csv', sep='')
	frequency_matrix <<- read.table(freqfilename, sep=',', header = F)
	frequency_matrix <<- as.matrix(frequency_matrix, nrow = nrow(frequency_matrix))
}

