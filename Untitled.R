setwd("/Users/andrescobos/statistics_master/multivariate_analysis/project")
data = read.csv("data_of_the_world.csv")
View(data)
head(data)
names(data)
zero_to_clean = c()

no_data_to_clean = c("net.migration","Infant.mortality..per.1000.births.",
                    "GDP....per.capita.","Literacy....", "Phones..per.1000.",
                    "Arable....", "Crops....", "Other....","Climate", "Birthrate","Agriculture","Industry",  "Service")


##Codigo para eliminar los huecos en blanco presentes en los nombres de las regiones
data$Region <- as.character(data$Region)

data[data$Region=="ASIA (EX. NEAR EAST)     	",]$Region <- "ASIA(EX.NEAR_EAST)"
data[data$Region=="EASTERN EUROPE                 	",]$Region <- 
  "EASTERN_EUROPE"
data[data$Region=="NORTHERN AFRICA                	",]$Region <- "NORTHERN_AFRICA"
data[data$Region=="OCEANIA                        	",]$Region <- "OCEANIA"
data[data$Region=="WESTERN EUROPE                 	",]$Region <- "WESTERN_EUROPE"
data[data$Region=="SUB-SAHARAN AFRICA             	",]$Region <- "SUB-SAHARAN_AFRICA"
data[data$Region=="LATIN AMER. & CARIB	",]$Region <- "LATIN_AMER.&CARIB"
data[data$Region=="C.W. OF IND. STATES ",]$Region <- "C.W._OF_IND._STATES"
data[data$Region=="NEAR EAST                      	",]$Region <- "NEAR_EAST"
data[data$Region=="NORTHERN AMERICA               	",]$Region <- "NORTHERN_AMERICA"
data[data$Region=="BALTICS                        	",]$Region  <- "BALTICS"


data$Region <- as.factor(data$Region) 






#

unique(data$Region)


for (climate_region in  unique(data$Region)){
  print(climate_region)
  sub_data = data[data$Region == climate_region,c("Climate")]
}

for (climate_region in  unique(data$Region)){
  print(climate_region)
  sub_data = data[data$Region == "WESTERN EUROPE",]
}


data$ClimatePredicted = 


data[data$Climate == NA, ]

for (i in names(data)){
  a = data[data$i == 0, ]
  a
}
unique(data$Region)

























summary(data)

# This function computes the power of a square matrix
matrixpower <- function(M,k) {
  # ARGUMENTS:
  # M: square matrix 
  # k: exponent
  
  if(dim(M)[1]!=dim(M)[2]) return(print("Error: matrix M is not square"))
  
  if (k == 0) return(diag(dim(M)[1]))  # if k=0 returns the identity matrix
  if (k == 1) return(M)
  if (k > 1)  return(M %*% matrixpower(M, k-1)) # if k>1 recursively apply the function
}

alpha=c(0.25,0.25,0.25,0.25)
l_matrix = matrix(c(0.1, 0.4, 0.2, 0.3, 0.6, 0, 0.2, 0.2, 0.1, 0.3, 0.3,0.3, 0.5, 0.3 ,0.1, 0.1), nrow = 4, byrow = TRUE)
sq= matrixpower(l_matrix,2 )
sq[4,3]
multi = alpha*l_matrix

#probability distribution of x2
px2 = t(alpha)%*%matrixpower(l_matrix,2 )

#expectation

px2 %*% (1:4)

install.packages("microbenchmark")
library(microbenchmark)