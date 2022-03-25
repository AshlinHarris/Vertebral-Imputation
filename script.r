
source("AdjacentMethod.r")
source("RegionMethod.r")
source("CompareMethods.r")

#max(vertebrae) - min(vertebrae)

Input <- read.table("data.csv", header=TRUE, sep=",", row.names="ID")

#dim(Input)
#names(Input)

#pdf(file="pairs.pdf",width=30,height=20, pointsize=6)
#pairs(Input)
#dev.off()

#boxplot(Input)

# get some additional data totals for the data set
#cervical_length <- rowSums(Input[,cervical])
#thoracic_length <- rowSums(Input[,thoracic])
#lumbar_length <- rowSums(Input[,lumbar])
#total_length <- cervical_length + thoracic_length + lumbar_length

#length.data <- data.frame(cervical_length, thoracic_length, lumbar_length, total_length)

#https://stackoverflow.com/questions/21585721/how-to-create-an-empty-matrix-in-r#21585775

# Specify column ranges for each vertebral region
vertebrae <- 3:25
cervical <- 3:8
thoracic <- 9:20
lumbar <- 21:25

# copy an individual's data 
x <- Input[1,]
x

# verify that both methods retun the exact answer when no vertebrae are missing
RegionMethod(x)
AdjacentMethod(x)

# delete the cervical vertebrae from the copy
x[cervical] <- NA

# compare the two methods
RegionMethod(x)
AdjacentMethod(x)

#TODO: analyze male and female separately
#TODO: account for NAs in RegionMethod()







