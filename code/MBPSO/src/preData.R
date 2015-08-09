#source("DataExtraction.R")
library(Matrix)
predata <- function(noService=3, noCandLoc=3, noUserCen=3){
	set.seed(1)
	common <- function(col){
		common_part <- matrix()
		tmp <- matrix(runif(col * col, 0 ,10), ncol = col)
		common_part <- forceSymmetric(tmp)
		d <- diag(rep(1, col), ncol = col)
		d <- !d
		common_part <- matrix(common_part * d, ncol = col)
		common_part
	}
	generate_latency_matrix <- function(noUserCen, noCandLoc){
		latency_matrix <- matrix()
		if(noUserCen > noCandLoc){
			common_part <- common(noCandLoc)
			t <- matrix(runif((noUserCen - noCandLoc) * noCandLoc, 0, 10), ncol = noCandLoc)
			latency_matrix <- rbind(common_part, t)
		}
		else if(noUserCen < noCandLoc){
			common_part <- common(noUserCen)
			t <- matrix(runif(noUserCen * (noCandLoc - noUserCen), 0, 10), ncol = (noCandLoc - noUserCen))
			latency_matrix <- cbind(common_part, t)
		}
		else if(noUserCen == noCandLoc){
			latency_matrix <- common(noUserCen)
		}
		latency_matrix
	}
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
	latency_matrix <<- generate_latency_matrix(noService, noUserCen)
}

