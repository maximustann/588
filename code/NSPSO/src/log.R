log <- function(problem, iter, gen, pop, fitness){
	#cat("iter: ", iter, '\n')
	#cat("gen: ", gen, '\n')
	path <- paste("/home/st-james1/tanboxi/588_project/code/NSPSO/logData/", problem, '/', sep = '')
	iterDirect <- paste(path, iter, '/', sep = '')
	dir.create(iterDirect, showWarnings = F, mode = "0777")
	genDirect <- paste(iterDirect, gen, '/', sep = '')
	dir.create(genDirect, showWarnings = F, mode = "0777")

	filefront <- paste(genDirect, 'front.csv', sep = '')
	filefitness <- paste(genDirect, 'fitness.csv', sep = '')
	filePop <- paste(genDirect, 'pop.csv', sep = '')
	write.table(fitness, filefront, row.names = F, col.names = F, sep=',')
	write.table(fitness, filefitness, row.names = F, quote = F, sep = ',')
	write.table(pop, filePop, row.names = F, quote = F, sep = ',')
}
