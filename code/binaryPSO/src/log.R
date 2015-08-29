log <- function(problem, iter, gen, pop, fitness){
	#cat("iter: ", iter, '\n')
	#cat("gen: ", gen, '\n')
	path <- paste("/home/st-james1/tanboxi/588_project/code/binaryPSO/logData/", problem, '/', sep = '')
	iterDirect <- paste(path, iter, '/', sep = '')
	dir.create(iterDirect, mode = "0777")
	genDirect <- paste(iterDirect, gen, '/', sep = '')
	dir.create(genDirect, mode = "0777")

	filefitness <- paste(genDirect, 'fitness.csv', sep = '')
	#filePop <- paste(genDirect, 'pop.csv', sep = '')
	write.csv(fitness, filefitness, row.names = F, quote = F)
	#write.csv(pop, filePop, row.names = F, quote = F)
}
