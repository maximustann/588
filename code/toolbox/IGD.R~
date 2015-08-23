
igd <- function(front, reference_front){
	value <- 0
	flag <- 0
	distanceToCloestPoint <- function(reference_point, mfront){
		frontSize <- nrow(mfront)
		new_dit <- vector()
		for(i in 1:frontSize){
			new_dit <- c(new_dit, dist_measure(reference_point, mfront[i, ]))
		}
		dit <- sqrt(min(unlist(new_dit)))
		dit
	}
	dist_measure <- function(point1, point2){
		return((point1[1] - point2[1])^2 + (point1[2] - point2[2])^2)
	}
	refeFrontSize <- nrow(reference_front)
	for(i in 1:refeFrontSize){
		value <- value + distanceToCloestPoint(reference_front[i,], front)
	}

	value <- value / nrow(reference_front)
	value
}



