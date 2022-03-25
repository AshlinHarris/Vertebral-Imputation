
compareMethods <- function(subset_size) {

	total_elements <- length(vertebrae)
	total_entries <- dim(Input)[1]

	AM_CE <- 0
	RM_CE <- 0

	total_lengths <- rowSums(Input[vertebrae])

	all_subsets <- combn(vertebrae,subset_size)
	total_combinations <- dim(all_subsets)[2]

	for (j in 1:total_combinations){
		subset <- all_subsets[,j]

		data <- Input
		data[,subset] <- NA

		for (k in 1:total_entries){
			AM_CE <- AM_CE + (as.double(total_lengths[k]) - AdjacentMethod(data[k,]))**2
			RM_CE <- RM_CE + (as.double(total_lengths[k]) -   RegionMethod(data[k,]))**2
		}
	}
	#lengths <- numeric(8)

	AM_MSE <- AM_CE / (total_entries * total_combinations)
	RM_MSE <- RM_CE / (total_entries * total_combinations)

	return(c(AM_MSE, RM_MSE))

}

