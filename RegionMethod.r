RegionMethod <- function(data) {

	if(any(is.na(data[cervical]))) {
		if(any(is.na(data[thoracic]))) {
			if(any(is.na(data[lumbar]))) {
				# This method requires the length of at least 1 intact region.
				return(NA)
			}
			else {
				lumbar_length <- sum(data[lumbar])

				# Estimate cervical and thoracic lengths using lumbar length:
				cervical_length <- 30.401 + 0.566*lumbar_length
				thoracic_length <- 49.822 + 1.438*lumbar_length
			}
		}
		else {
			thoracic_length <- sum(data[thoracic])
			if(any(is.na(data[lumbar]))) {
				# Estimate cervical and lumbar lengths using thoracic length:
				cervical_length <- 9.979 + 0.397*thoracic_length
				lumbar_length <- 25.212 + 0.422*thoracic_length
			}
			else {
				lumbar_length <- sum(data[lumbar])
				# Estimate cervical length using thoracic and lumbar lengths:
				cervical_length <- 10.322 + 0.403*thoracic_length - 0.014*lumbar_length
			}
		}
	}
	else {
		cervical_length <- sum(data[cervical])
		if(any(is.na(data[thoracic]))) {
			if(any(is.na(data[lumbar]))) {
				# Estimate thoracic and lumbar lengths using cervical length:
				thoracic_length <- 58.728 + 1.712*cervical_length
				lumbar_length <- 51.878 + 0.751*cervical_length
			}
			else {
				lumbar_length <- sum(data[lumbar])
				# Estimate thoracic length using cervical and lumbar lengths:
				thoracic_length <- 16.402 + 1.099*cervical_length + 0.816*lumbar_length
			}
		}
		else {
			thoracic_length <- sum(data[thoracic])
			if(any(is.na(data[lumbar]))) {
				# Estimate lumbar length using cervical and thoracic lengths:
				lumbar_length = 25.416 - 0.02*cervical_length + 0.451*thoracic_length
			}
			else {
				lumbar_length <- sum(data[lumbar])

				# The vertebral column is intact; no estimation is required.
			}
		}
	}

	total_length <- cervical_length + thoracic_length + lumbar_length
	return(total_length)

}
