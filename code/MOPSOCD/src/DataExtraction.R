library(Matrix)
extract <- function(noCandLoc, noUserCen) {
	#reading files
	#userData <<- read.table("userlist.txt", sep  = "\t", header = F)
	#latencyData <<- read.table("rtmatrix.txt", sep = "\t", header = F)
	#serviceData <<- read.table("wslist.txt", sep = "\t", header = F)

	#Give them names
	#names(userData) <<- c("ID", "IP", "Country", "longtitude", "Latitude")
	#names(serviceData) <<- c("ID", "WSDL", "Provider", "Country")

	latencyMatrix <<- matrix()
	if(noCandLoc == 3 && noUserCen == 3){
		#From first US user, first JP user and first UK user
		#To first Arg user, first Aust user and first Bel user
		#testList <<- userData[userData$Country == "United States", ][1, ]
		#testList <<- rbind(testList, userData[userData$Country == "Japan", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "United Kingdom", ][1, ])
		#testService <<- serviceData[serviceData$Country == "Argentina", ][1, ]
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Australia", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Belgium", ][1, ])

		#change ID "0" to "1"
		#check_zero_label(testList, testService)
		#latencyMatrix <<- latencyData[testList$ID, paste("V", testService$ID, sep = '')]
		#names(latencyMatrix) <<- c("Argentina", "Australia", "Belgium")
		#rownames(latencyMatrix) <<- c("United States", "Japan", "United Kingdom")

		#costMatrix <<- matrix(data = rnorm(3 * 4, mean = 100, sd = 20), nrow = 3, ncol = 4)
		#colnames(costMatrix) <<- c("Argentina", "Australia", "Belgium", "United States")
		#rownames(costMatrix) <<- c("S1", "S2", "S3")


		#test case 1
		#latencyVector <- c(5.982, 2.130, 0.710, 5.776, 0, 0.760, 6.984, 2.035, 1.863)
		#test case 2
		#latencyVector <- c(0, 5.776, 6.984,
						   #5.776, 0, 2.035,
						   #6.984, 2.035, 0)
		tmp <- matrix(runif(num * num, 0, 10), ncol = num)
		latencyMatrix <- forceSymmetric(tmp)
		d <- diag(rep(1, num), ncol = num)
		d <- !d
		latencyMatrix <- latencyMatrix * d


		latencyMatrix <<- matrix(latencyVector, nrow = noUserCen)
	}
	else if(noCandLoc == 5 && noUserCen == 5){
		#testList <<- userData[userData$Country == "United States", ][1, ]
		#testList <<- rbind(testList, userData[userData$Country == "Japan", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "United Kingdom", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Canada", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Norway", ][1, ])
		#testService <<- serviceData[serviceData$Country == "Argentina", ][1, ]
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Australia", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Belgium", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "United States", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Brazil", ][1, ])

		#check_zero_label(testList, testService)

		latencyVector <- c(0, 5.776, 6.984, 1.842, 5.569,
						  5.776, 0, 2.035, 0.799, 0.684,
						  0.684, 2.035, 0, 1.424, 0.519,
						  0.639, 0.68, 0.672, 0, 0.812,
						  5.569, 0.684, 0.519, 0.812, 0)
		latencyMatrix <<- matrix(latencyVector, nrow = noUserCen)
		#latencyMatrix <<- latencyData[testList$ID, paste("V", testService$ID, sep = '')]
	}
	else if(noCandLoc == 10 && noUserCen == 10){
		#testList <<- userData[userData$Country == "United States", ][1, ]
		#testList <<- rbind(testList, userData[userData$Country == "Japan", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "United Kingdom", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Canada", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Norway", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "United States", ][3, ])
		#testList <<- rbind(testList, userData[userData$Country == "Germany", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Italy", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Spain", ][2, ])
		#testList <<- rbind(testList, userData[userData$Country == "Switzerland", ][1, ])
		#testService <<- serviceData[serviceData$Country == "Argentina", ][1, ]
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Australia", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Belgium", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "United States", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Brazil", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Germany", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "France", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Denmark", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Canada", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "China", ][1, ])
		#check_zero_label(testList, testService)
		#latencyMatrix <<- latencyData[testList$ID, paste("V", testService$ID, sep = '')]
		latencyVector <- c(0, 5.776, 6.984, 1.842, 5.569, 0.329, 5.334, 5.511, 6.19, 5.567,
						  5.776, 0, 2.035, 0.799, 0.684, 0.244, 0.382, 0.718, 1.026, 0.567,
						  0.684, 2.035, 0, 1.424, 0.519, 0.346, 0.316, 0.564, 0.823, 0.445,
						  0.639, 0.68, 0.672, 0, 0.812, 0.39, 0.13, 0.11, 0.399, 0.807,
						  5.569, 0.684, 0.519, 0.812, 0, 0.275, 0.391, 0.73, 1.124, 0.584,
						  0.329, 0.244, 0.346, 0.39, 0.275, 0, 0.329, 0.344, 0.965, 0.363,
						  5.334, 0.382, 0.316, 0.13, 0.391, 0.329, 0, 0.453, 0.280, 0.26,
						  5.511, 0.718, 0.564, 0.11, 0.73, 0.344, 0.453, 0, 0.42, 0.029,
						  6.19, 1.026, 0.823, 0.399, 1.124, 0.965, 0.280, 0.42, 0, 0.192,
						  5.567, 0.567, 0.445, 0.807, 0.584, 0.363, 0.26, 0.029,0.192, 0)
		latencyMatrix <<- matrix(latencyVector, nrow = noUserCen)
	}

	else if(noCandLoc == 15 && noUserCen == 15){
		#testList <<- userData[userData$Country == "United States", ][1, ]
		#testList <<- rbind(testList, userData[userData$Country == "Japan", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "United Kingdom", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Canada", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Norway", ][1, ])

		#testList <<- rbind(testList, userData[userData$Country == "United States", ][3, ])
		#testList <<- rbind(testList, userData[userData$Country == "Germany", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Italy", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Spain", ][2, ])
		#testList <<- rbind(testList, userData[userData$Country == "Switzerland", ][1, ])

		#testList <<- rbind(testList, userData[userData$Country == "United States", ][4, ])
		#testList <<- rbind(testList, userData[userData$Country == "Israel", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Singapore", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Hong Kong", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Taiwan", ][1, ])

		#testService <<- serviceData[serviceData$Country == "Argentina", ][1, ]
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Australia", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Belgium", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "United States", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Brazil", ][1, ])

		#testService <<- rbind(testService, serviceData[serviceData$Country == "Germany", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "France", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Denmark", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Canada", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "China", ][1, ])

		#testService <<- rbind(testService, serviceData[serviceData$Country == "Taiwan", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Iceland", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "United Kingdom", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Singapore", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Slovenia", ][1, ])

		#check_zero_label(testList, testService)
		#latencyMatrix <<- latencyData[testList$ID, paste("V", testService$ID, sep = '')]
		latencyVector <- c(0, 5.776, 6.984, 1.842, 5.569, 0.329, 5.334, 5.511, 6.19, 5.567, 5.710,0.134,1.512,8.877,5.950,
						  5.776, 0, 2.035, 0.799, 0.684, 0.244, 0.382, 0.718, 1.026, 0.567, 0.707,0.207,1.188,0.924,1.15,
						  0.684, 2.035, 0, 1.424, 0.519, 0.346, 0.316, 0.564, 0.823, 0.445, 0.719,0.167,1.572,0.877,0.98,
						  0.639, 0.68, 0.672, 0, 0.812, 0.39, 0.13, 0.11, 0.399, 0.807, 0.167,0.045,0.534,0.554,0.46,
						  5.569, 0.684, 0.519, 0.812, 0, 0.275, 0.391, 0.73, 1.124, 0.584, 0.872,0.166, 1.9,1.07,1.012,
						  0.329, 0.244, 0.346, 0.39, 0.275, 0, 0.329, 0.344, 0.965, 0.363, 5.480,0.308,1.916, 3.785, 1.181,
						  5.334, 0.382, 0.316, 0.13, 0.391, 0.329, 0, 0.453, 0.280, 0.26, 0.697, 0.153, 2.014, 1.256, 1.532,
						  5.511, 0.718, 0.564, 0.11, 0.73, 0.344, 0.453, 0, 0.42, 0.029, 0.516, 0.252, 1.116, 0.413, 0.556,
						  6.19, 1.026, 0.823, 0.399, 1.124, 0.965, 0.280, 0.42, 0, 0.192, 0.406, 0.047, 1.008, 0.614, 0.522,
						  5.567, 0.567, 0.445, 0.807, 0.584, 0.363, 0.26, 0.029,0.192, 0, 0.148, 0.132, 0.436, 0.242, 0.334,
						  5.710, 0.707, 0.719, 0.167, 0.872, 5.480, 0.697, 0.516, 0.406, 0.148, 0, 0.387, 0.427, 0.549, 0.582,
						  0.134, 0.207, 0.167, 0.045, 0.166, 0.308, 0.153, 0.252, 0.047, 0.123, 0.189, 0, 0.174, 0.204, 0.096,
						  1.512, 1.188, 1.572, 0.534, 1.9, 1.916, 2.014, 1.116, 1.008, 0.436, 0.685, 1.432, 0, 1.716, 1.668,
						  8.877, 0.924, 0.877, 0.554, 1.07, 3.785, 1.256, 0.413, 0.614, 0.242, 0.473, 0.662, 0.804, 0, 0.749,
						  5.950, 1.15, 0.98, 0.46, 1.012, 1.818, 1.532, 0.566, 0.522, 0.334, 0.406, 0.610, 0.644, 0.749, 0
						  )
		latencyMatrix <<- matrix(latencyVector, nrow = noUserCen)
	}
	#else if(noCandLoc == 20 && noUserCen == 20){
		#testList <<- userData[userData$Country == "United States", ][1, ]
		#testList <<- rbind(testList, userData[userData$Country == "Japan", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "United Kingdom", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Canada", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Norway", ][1, ])

		#testList <<- rbind(testList, userData[userData$Country == "United States", ][3, ])
		#testList <<- rbind(testList, userData[userData$Country == "Germany", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Italy", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Spain", ][2, ])
		#testList <<- rbind(testList, userData[userData$Country == "Switzerland", ][1, ])

		#testList <<- rbind(testList, userData[userData$Country == "United States", ][4, ])
		#testList <<- rbind(testList, userData[userData$Country == "Israel", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Singapore", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Hong Kong", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Taiwan", ][1, ])

		#testList <<- rbind(testList, userData[userData$Country == "Greece", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Korea, Republic of", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Ireland", ][2, ])
		#testList <<- rbind(testList, userData[userData$Country == "Portugal", ][1, ])
		#testList <<- rbind(testList, userData[userData$Country == "Japan", ][2, ])

		#testService <<- serviceData[serviceData$Country == "Argentina", ][1, ]
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Australia", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Belgium", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "United States", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Brazil", ][1, ])

		#testService <<- rbind(testService, serviceData[serviceData$Country == "Germany", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "France", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Denmark", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Canada", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "China", ][1, ])

		#testService <<- rbind(testService, serviceData[serviceData$Country == "Taiwan", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Iceland", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "United Kingdom", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Singapore", ][1, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Slovenia", ][1, ])

		#testService <<- rbind(testService, serviceData[serviceData$Country == "United States", ][2, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Denmark", ][2, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "United Kingdom", ][2, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "France", ][2, ])
		#testService <<- rbind(testService, serviceData[serviceData$Country == "Germany", ][2, ])

		#check_zero_label(testList, testService)
		#latencyMatrix <<- latencyData[testList$ID, paste("V", testService$ID, sep = '')]
		
		#tmp <- matrix(runif(num * num, 0, 10), ncol = num)
		#latencyMatrix <- forceSymmetric(tmp)
		#d <- diag(rep(1, num), ncol = num)
		#d <- !d
		#latencyMatrix <- latencyMatrix * d
	#}
	#else if(num == 30){
		#tmp <- matrix(runif(num * num, 0, 10), ncol = num)
		#latencyMatrix <- forceSymmetric(tmp)
		#d <- diag(rep(1, num), ncol = num)
		#d <- !d
		#latencyMatrix <- latencyMatrix * d
	#}

	latencyMatrix
}

#check_zero_label <- function(testList, testService){
	#if(match(0, testList$ID)){
		#testList[testList$ID == 0][1, 1] <<- 1
	#}

	#if(match(0, testService$ID)){
		#testService[testService$ID == 0][1, 1] <<- 1
	#}
#}
