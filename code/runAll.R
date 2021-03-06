runAll <- function(algorithms = c("MOPSOCD", "binaryPSO", "NSGA2", "NSPSO"), problem = 1, logSv){
	i <- 1
	for(algorithm in algorithms){
		runScriptdir <- paste("/home/st-james1/tanboxi/588_project/code/", algorithm, "/src/", sep = '')
		print(runScriptdir)
		setwd(runScriptdir)
		source("experiment.R")
		cat(problem, logSv[i])
		run(problem, logSv[i])
		i <- i + 1
	}
}
