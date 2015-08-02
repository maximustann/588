source('./binaryPSO/result/plotBest.R')
source('./MOPSOCD/result/plotBest.R')

viewData <- function(problem){
	plotmpso(problem, col = "red", pch = 1)
	par(new = T)
	plotbpso(problem, col = "blue", pch = 24)
	par(xpd = T)
	legend("topright", c("MOPSOCD", "BPSO"), col = c("red", "blue"), pch = c(1, 24))
}
