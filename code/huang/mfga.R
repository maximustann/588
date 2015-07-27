
mfga <- function(noService, noCandLoc, noUserCen){
#--------List of parameters---------
	popSize <- 50

#--------List of parameters---------



	
	#--------Initialize population------
	generatePop <- function(noService, noCandLoc, noUserCen, popSize){
		pop <- vector()
		for (i in 1:popSize)
			count
			chromosome <- rbinom(noService * noCandLoc, 1, 0.5)
			pop <- rbind(pop, chromosome)
		}
		pop
	}
	#--------Initialize population------



	#--------Constraint function--------
	costCons <- function(chromosome, noService, noCandLoc, costBound){
		dim(chromosome) <- c(noService, noCandLoc)
		cost <- sum(chromosome * cost_matrix)
		result <- cost / costBound
		result
	}

	serviceNumCons <- function(chromosome, noService, noCandLoc){
		dim(chromosome) <- c(noService, noCandLoc)
		row <- apply(chromosome, 1, sum)
		result <- prod(row) #If the result = 0 means there is one row add up to 0
		if(result == 0){
			return(0)
		} else {
			return(1)
		}
	}
	#--------Fitness function-----------



	responseTimeCal <- function(chromosome, noService, noCandLoc, noUserCen){
		dim(chromosome) <- c(noService, noCandLoc)
		result <- vector()
		for(i in 1:noUserCen){
			rowVector <- vector()
			for(j in 1:noService){
				rowVector <- c(rowVector, min(latency_matrix[i, ] * chromosome[j, ]))
			}
			result <- rbind(result, rowVector)
		}
		result
	}
	
	fitness <- function(chromosome, noService, noCandLoc, noUserCen){
		p <- costCons(chromosome, noService, noCandLoc, costBound) * 
				serviceNumCons(chromosome, noService, noCandLoc)
	}
	#--------Fitness function-----------


}

