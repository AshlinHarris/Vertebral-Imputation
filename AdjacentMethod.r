
AdjacentMethod <- function(data) {

	# Coefficients for estimating vertebra from lower neighbor: length = c + b(lower)
	lc <- c(NA,NA,23.904,2.249,3.159,2.038,2.079,4.987,4.815,2.879,3.17,1.692,2.912,2.999,1.81,2.045,3.368,1.764,3.213,5.314,4.342,5.592,4.113,6.128,NA)
	lb <- c(NA,NA, 0.953,0.874,0.779,0.825,0.74, 0.584,0.653,0.842,0.809,0.88,0.823,0.826,0.892,0.876,0.813,0.888,0.799,0.735,0.804,0.755,0.829,0.78,NA)

	# Coefficients for estimating vertebra from upper neighbor: length = c + b(upper)
	uc <- c(NA,NA,NA,2.919,3.329,2.137,3.573,5.507,5.671,6.238,4.227,4.057,2.185,1.461,1.115,3.624,3.096,2.09,3.546,5.667,4.768,5.248,4.031,2.127,3.424)
	ub <- c(NA,NA,NA,0.291,0.712,0.809,0.737,0.708,0.743,0.71,0.763,0.792,0.912,0.95,0.965,0.836,0.873,0.929,0.867,0.812,0.851,0.815,0.875,0.937,0.862)

	# Coefficients for estimating vertebra from both neighbors: length = c + ub(upper) + lb(lower)
	bc   <- c(NA,NA,NA,1.034,1.485,0.344,0.386,3.411,2.779,1.757,1.58,0.852,0.965,0.75,0.04,1.386,1.744,0.172,0.528,3.106,1.517,3.196,1.065,0.008,NA)
	bub <- c(NA,NA,NA,0.056,0.414,0.493,0.511,0.501,0.424,0.263,0.44,0.254,0.53,0.491,0.623,0.419,0.489,0.482,0.61,0.487,0.457,0.445,0.379,0.642,NA)
	blb <- c(NA,NA,NA,0.808,0.466,0.458,0.413,0.288,0.42,0.661,0.463,0.682,0.423,0.473,0.381,0.507,0.427,0.507,0.361,0.392,0.492,0.427,0.583,0.373,NA)

	# positions of first and last vertebrae
	first <- cervical[1]
	last <- tail(lumbar,n=1)

	# If a gap exists at the beginning, find the first value and estimate all previous values from below.
	current <- first
	while(current <= last && is.na(data[current])){
		current <- current + 1
	}

	if(current > last){
		return(NA)
	}

	i <- current
	while (i > first){
		
		# Estimate individual vertebra from lower vertebra
		data[i-1] <- lc[i-1] + lb[i-1]*data[i]

		i <- i-1
	}

	# Find any remaining gap
	while(current <= last){
		if(is.na(data[current])){
			before <- current-1
			after <- current+1
			while(after <= last && is.na(data[after])){
				after <- after + 1
			}

			reached_end <- FALSE
			if(after > last){
				reached_end <- TRUE
			}

			# For a gap at the end, estimate all missing values from neighbor above
			if(reached_end){
				for (i in current:last){
				data[i] <- uc[i] + ub[i]*data[i-1]
				}
			}
			else{
				# Determine middle position, if applicable
				middle <- (before + after)/2

				# Estimate missing values above the middle with upper neighbor
				i <- before
				while(i < middle-1){
					data[i+1] <- uc[i+1] + ub[i+1]*data[i]
					i <- i + 1
				}

				# Estimate missing values below the middle with lower neighbor
				i <- after
				while(i > middle+1){
					data[i-1] <- lc[i-1] + lb[i-1]*data[i]
					i <- i - 1
				}

				# Middle value (if applicable) must be estimated by both neighbors
				if(middle%%1==0){
					data[middle] = bc[middle] + bub[middle]*data[middle-1] + blb[middle]*data[middle+1]
				}

			}
			
			# update current value to position of next potential missing value
			current <- after + 1
		}
		else{

			# update current value to next position
			current <- current + 1
		}
	}

	return(sum(data[vertebrae]))
}

