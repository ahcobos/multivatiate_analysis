setwd("/Users/andrescobos/statistics_master/multivariate_analysis/project")
data = read.csv("countries_of_the_world.csv", sep=",")
View(data)
head(data)
names(data)
zero_to_clean = c()

no_data_to_clean = c("net.migration","Infant.mortality..per.1000.births.",
                     "GDP....per.capita.","Literacy....", "Phones..per.1000.",
                     "Arable....", "Crops....", "Other....","Climate", "Birthrate","Agriculture","Industry",  "Service")

colnames(data)<-c("Country","Region","Population","Area_squared_km","Pop_dens_squared_km","Coastline_ratio",
                       "Net_migration","Infant_Mortality_1000","GDP","Literacy","Phones_1000","Arable",
                       "Crops","Other","Climate","Birthrate","Deathrate","Agriculture","Industry","Services")
#
data$Region <- as.character(data$Region)
data[data$Region=="ASIA (EX. NEAR EAST)         ",]$Region <- "ASIA(EX.NEAR_EAST)"
data[data$Region=="EASTERN EUROPE                     ",]$Region <- "EASTERN_EUROPE"
data[data$Region=="NORTHERN AFRICA                    ",]$Region <- "NORTHERN_AFRICA"
data[data$Region=="OCEANIA                            ",]$Region <- "OCEANIA"
data[data$Region=="WESTERN EUROPE                     ",]$Region <- "WESTERN_EUROPE"
data[data$Region=="SUB-SAHARAN AFRICA                 ",]$Region <- "SUB-SAHARAN_AFRICA"
data[data$Region=="LATIN AMER. & CARIB    ",]$Region <- "LATIN_AMER.&CARIB"
data[data$Region=="C.W. OF IND. STATES ",]$Region <- "C.W._OF_IND._STATES"
data[data$Region=="NEAR EAST                          ",]$Region <- "NEAR_EAST"
data[data$Region=="NORTHERN AMERICA                   ",]$Region <- "NORTHERN_AMERICA"
data[data$Region=="BALTICS                            ",]$Region  <- "BALTICS"
data$Region <- as.factor(data$Region)

data[data$Climate=="1,5",]$Climate = 2
data[data$Climate=="2,5",]$Climate = 3

#assign most frequent climate of the region to the countries
for(data_region in unique( data$Region) ){
  data_subset = data[data$Region==data_region & data$Climate!="" ,"Climate"]
  table_data = data.frame(table(data_subset))
  first_item = table_data[which.max(table_data$Freq),]
  most_occurrence = as.character(first_item$data_subset[[1]])
  print(data_region)
  print(most_occurrence)
  if(nrow( data[data$Region==data_region & (data$Climate=="") ,])){
    data[data$Region==data_region & (data$Climate=="") ,]$Climate = most_occurrence
  }
}
#force columns types 
data$Population <- as.numeric( sub(",", ".",data$Population))
data$Area_squared_km <- as.numeric(sub(",", ".",as.character(data$Area_squared_km)))
data$Pop_dens_squared_km <- as.numeric(sub(",", ".",as.character(data$Pop_dens_squared_km) ))
data$Coastline_ratio <- as.numeric(sub(",", ".",as.character(data$Coastline_ratio)))
data$Net_migration <- as.numeric(sub(",", ".",as.character(data$Net_migration)))
data$Infant_Mortality_1000 <- as.numeric(sub(",", ".",as.character(data$Infant_Mortality_1000)))
data$GDP <- as.numeric(sub(",", ".",as.character(data$GDP)))
data$Literacy <- as.numeric(sub(",", ".",as.character(data$Literacy)))
data$Phones_1000<- as.numeric(sub(",", ".",as.character(data$Phones_1000)))
data$Arable <- as.numeric(sub(",", ".",as.character(data$Arable)))
data$Crops <- as.numeric(sub(",", ".",as.character(data$Crops)))
data$Other <- as.numeric(sub(",", ".",as.character(data$Other)))
data$Birthrate <- as.numeric(sub(",", ".",as.character(data$Birthrate)))
data$Deathrate <- as.numeric(sub(",", ".",as.character(data$Deathrate)))
data$Agriculture <- as.numeric(sub(",", ".",as.character(data$Agriculture)))
data$Industry <- as.numeric(sub(",", ".",as.character(data$Industry)))
data$Services <- as.numeric(sub(",", ".",as.character(data$Services)))
View(data)

