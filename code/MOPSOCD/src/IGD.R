
#igd_r <- function(front, reference_front){
	#value <- 0
	#dist <- 0
	#distanceToCloestPoint <- function(reference_point, mfront){
		#if(nrow(mfront) == 1){
			#dist <- sqrt(dist_measure(reference_point, mfront[1,]))
			#return(dist)
		#}
		#if(dist_measure(reference_point, mfront[1, ]) <= dist_measure(reference_point, mfront[nrow(mfront), ])){
			#distanceToCloestPoint(reference_point, mfront[1:ceiling(nrow(mfront) / 2), ])
		#}else{
			#distanceToCloestPoint(reference_point, mfront[(floor(nrow(mfront) / 2) + 1):nrow(mfront), ])
		#}
	#}
	
	#dist_measure <- function(point1, point2){
		#return((point1[1] - point2[1])^2 + (point1[2] - point2[2])^2)
	#}

	#refeFrontSize <- nrow(reference_front)
	#for(i in 1:refeFrontSize){
		#value <- value + distanceToCloestPoint(reference_front[i, ], front)
	#}
	#value
#}

igd_i <- function(front, reference_front){
	value <- 0
	flag <- 0
	distanceToCloestPoint <- function(reference_point, mfront){
		frontSize <- nrow(mfront)
		dit <- dist_measure(reference_point, mfront[1,])
		for(i in 2:frontSize){
			new_dit <- dist_measure(reference_point, mfront[i, ])
			if(new_dit <= dit){
				dit <- new_dit
				flag <- i
			}
		}
		dit <- sqrt(dit)
	}
	dist_measure <- function(point1, point2){
		return((point1[1] - point2[1])^2 + (point1[2] - point2[2])^2)
	}
	refeFrontSize <- nrow(reference_front)
	for(i in 1:refeFrontSize){
		value <- value + distanceToCloestPoint(reference_front[i,], front)
	}
	value

}



