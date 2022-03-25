RegionMethod <- function(data) {

	# Each combination of incomplete regions will produce a unique value
	indicator <- any(is.na(data[cervical])) +
	             2*any(is.na(data[thoracic])) +
	             4*any(is.na(data[lumbar]))

	switch(indicator,
	       "0"= { # No missing element
	       }
	       "1"= { # Missing cervical element
	       }
	       "2"= { # Missing thoracic element
	       }
	       "3"= { # Missing cervical and thoracic elements
	       }
	       "4"= { # Missing lumbar element
	       }
	       "5"= { # Missing cervical and lumbar elements

			# Calculate thoracic column length
			thoracic_length <- sum(data[thoracic])

			# Estimate cervical and lumbar lengths using thoracic length:
			cervical_length <- 9.979 + 0.397*thoracic_length
			lumbar_length <- 25.212 + 0.422*thoracic_length

			# Estimate total column length
			return(cervical_estimate + thoracic_length + lumbar_length)
	       }
	       "6"= { # Missing cervical and thoracic elements

			# Calculate lumbar column length
			lumbar_length <- sum(data[lumbar])

			# Estimate cervical and thoracic column lengths using lumbar column length:
			cervical_estimate <- 30.401 + 0.566*lumbar_length
			thoracic_estimate <- 49.822 + 1.438*lumbar_length

			# Estimate total column length
			return(cervical_estimate + thoracic_estimate + lumbar_length)
	       }
	       "7"= { # Missing cervical, thoracic, and lumbar elements
			return(NA)
	       }
}

