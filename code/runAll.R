runAll <- function(algorithms, problem = 1, logSv){
	i <- 1
	for(algorithm in algorithms){
		runScriptdir <- paste("~/588_project/code/", algorithm, "/src/", sep = '')
		setwd(runScriptdir)
		source("experiment.R")
		run(problem, logSv[i])
		i <- i + 1
	}
}
