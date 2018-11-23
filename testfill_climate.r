#load any required libraries
install.packages("pastecs")
library(ggplot2)
library("corrplot")
library("pastecs")
library(dplyr)


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
data$Country <- trimws(data$Country)

#add continents data



valid_correlation_data_names = c("Population", "Area_squared_km", "Pop_dens_squared_km", 
                           "Coastline_ratio","Net_migration", "Infant_Mortality_1000",
                           "GDP", "Literacy", "Phones_1000", "Arable", "Crops", 
                           "Birthrate", "Deathrate", "Agriculture", "Industry", "Services")



valid_correlation_data = data[,valid_correlation_data_names]


#Include continents information
data$Continent <- "NA"   
data[data$Region=="ASIA(EX.NEAR_EAST)",]$Continent <- "Asia"
data[data$Region=="EASTERN_EUROPE",]$Continent <- "Europe"
data[data$Region=="NORTHERN_AFRICA",]$Continent <- "Africa"
data[data$Region=="OCEANIA",]$Continent <- "Oceania"
data[data$Region=="WESTERN_EUROPE",]$Continent <- "Europe"
data[data$Region=="SUB-SAHARAN_AFRICA",]$Continent <- "Africa"
data[data$Region=="LATIN_AMER.&CARIB",]$Continent <- "America"
data[data$Region=="NEAR_EAST",]$Continent <- "Asia"
data[data$Region=="NORTHERN_AMERICA",]$Continent <- "America"
data[data$Region=="BALTICS",]$Continent <- "Europe"
data[data$Country=="Armenia",]$Continent <- "Asia"
data[data$Country=="Azerbaijan",]$Continent <- "Asia"
data[data$Country=="Belarus",]$Continent <- "Europe"
data[data$Country=="Georgia",]$Continent <- "Asia"
data[data$Country=="Kazakhstan",]$Continent <- "Asia"
data[data$Country=="Kyrgyzstan",]$Continent <- "Asia"
data[data$Country=="Moldova",]$Continent <- "Asia"
data[data$Country=="Russia",]$Continent <- "Europe"
data[data$Country=="Tajikistan",]$Continent <- "Asia"
data[data$Country=="Turkmenistan",]$Continent <- "Asia"
data[data$Country=="Ukraine",]$Continent <- "Europe"
data[data$Country=="Uzbekistan",]$Continent <- "Asia"

##Ultima grafica

##Grafica de GDP vs Literacy
ggplot(data = data, aes(x=GDP, y=Literacy, size=Population, color=Continent)) + geom_point(na.rm = TRUE)


##Codigo para sacar las proporciones de industria por continente
regionArable<-summarise(group_by(data,Continent), Arable=mean(Arable, na.rm = TRUE),Crops=mean(Crops, na.rm = TRUE),Other=mean(Other, na.rm = TRUE))

##Creo un dataframe que me permita generar los piecharts correctamente (necesita una estructura concreta)
arablePlot <- data.frame("Continent"=c(rep(regionArable$Continent[1],3),rep(regionArable$Continent[2],3),
                                       rep(regionArable$Continent[3],3),rep(regionArable$Continent[4],3),
                                       rep(regionArable$Continent[5],3)),
                         "Type"=rep(colnames(regionArable)[c(2:4)],5))

arablePlot$Values <-c(c(regionArable[1,c(2:4)]),c(regionArable[2,c(2:4)]),
                      c(regionArable[3,c(2:4)]),c(regionArable[4,c(2:4)]), c(regionArable[5,c(2:4)]))
arablePlot$Values <- as.numeric(arablePlot$Values)

#piechart per continent percent of terrain
ggplot(arablePlot, aes("",Values, fill=Type, labels=Values))+geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)+facet_wrap(~Continent)+geom_text(aes(label = round(Values,2)), position = position_stack(vjust = 0.5)) 



#correlation plot with circle graph
data.quan <- data[,c(3:14,16:20)]
head(data.quan)
data.scaled.quan<-data.frame(sapply(data.quan, function(x){(x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)}))
R.data.quan <- cor(data.scaled.quan, use="complete.obs")
corrplot(R.data.quan, type="upper", diag = FALSE, tl.col = "black")

#correlation vars with mean
means = colMeans(scale(data.quan), na.rm = TRUE)
pairs(scale(data.quan),pch=19,col=c(rep("deepskyblue2",dim(data.quan)[1]),"firebrick2"))
pairs(rbind(scale(data.quan),means) ,pch=19,col=c(rep("deepskyblue2",dim(data.quan)[1]),"firebrick2"))

#View(data)
#descriptive
stat.desc(dim(data[data[,c(3:14,16:20)]$Net_migration == 0,]))

#boxplot gdp vs continent
boxplot(data$GDP~data$Continent,main="GDP vs Continent",xlab="",ylab="GDP",col="deepskyblue2")

