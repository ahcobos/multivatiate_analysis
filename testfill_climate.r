#load any required libraries
#install.packages("pastecs")
#install.packages("pspearman")
#install.packages("factoextra")


library(ggplot2)
library("corrplot")
library("pastecs")
library(dplyr)
library("pspearman")
library("factoextra")


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

View(data[data$Continent=="America",])

qualitative_vars = colnames(data)[c(3:14,16:20)]


##Grafica de GDP vs Literacy
ggplot(data = data, aes(x=GDP, y=Literacy, size=Population, color=Continent)) + geom_point(na.rm = TRUE)+ ggtitle("GDP vs Literacy")

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
ggplot(arablePlot, aes("",Values, fill=Type, labels=Values))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)+ ggtitle("Distribution of land per continent")+
  facet_wrap(~Continent)+geom_text(aes(label = round(Values,2)), position = position_stack(vjust = 0.5)) 
  



#correlation plot with circle graph
data.quan <- data[,c(3:14,16:20)]
head(data.quan)
data.scaled.quan<-data.frame(sapply(data.quan, function(x){(x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)}))
R.data.quan <- cor(data.scaled.quan, use="complete.obs")
corrplot(R.data.quan,number.cex=0.8, method = "number",type="upper", diag = FALSE, tl.col = "black" ,title=" 
         Correlation")

correlated_columns <- qualitative_vars[!qualitative_vars %in% c("Population","Area_squared_km", "Pop_dens_squared_km", 
                                                                "Coastline_ratio","Crops", "Other","Industry")]
data.correlated = data[,correlated_columns]
#correlation vars with mean
means = colMeans(scale(data.correlated), na.rm = TRUE)
pairs( main="Util Correlation Detail",cex.labels = 1.2,rbind(scale(data.correlated),means) ,pch=19,col=c(rep("deepskyblue2",dim(data.correlated)[1]),"firebrick2"), lower.panel = NULL)


#View(data)
#descriptive
stat.desc((data[,c(3:14,16:20)]))

#boxplot gdp vs continent
boxplot(data$GDP~data$Continent,main="GDP vs Continent",xlab="",ylab="GDP",col="deepskyblue2")

#significant
cov(data[,c(3:14,16:20)], use="complete.obs")
View(cov(data[,c(3:14,16:20)], use="complete.obs"))

#correlation matrix p value
correlationPValue <- matrix(nrow=ncol(data.quan),ncol=ncol(data.quan))
for(i in 1:ncol(data.quan)){
  for(j in 1:ncol(data.quan)){
    correlationPValue[i,j]<- cor.test(data.quan[,i],data.quan[,j])$p.value
  }
}



plot_ly(as.data.frame(data),x=~data$Infant_Mortality_1000,y=~data$Phones_1000,z=~data$GDP)


#boxplot gdp vs continent
boxplot(data$Population,xlab="",ylab="GDP",col="deepskyblue2")
boxplot(data$Area_squared_km,xlab="",ylab="GDP",col="deepskyblue2")
boxplot(data$Pop_dens_squared_km,main="GDP vs Continent",xlab="",ylab="GDP",col="deepskyblue2")
boxplot(data$Coastline_ratio,xlab="",ylab="GDP",col="deepskyblue2")
boxplot(data$Net_migration,xlab="",ylab="GDP",col="deepskyblue2")
boxplot(data$Infant_Mortality_1000,xlab="",ylab="GDP",col="deepskyblue2")
boxplot(data$GDP,xlab="",ylab="GDP",col="deepskyblue2")
boxplot(data$Literacy,xlab="",ylab="GDP",col="deepskyblue2")
boxplot(data$Phones_1000,xlab="",ylab="GDP",col="deepskyblue2")
boxplot(data$Arable,xlab="",ylab="GDP",col="deepskyblue2")
boxplot(data$Crops,xlab="",ylab="GDP",col="deepskyblue2")
boxplot(data$Birthrate,xlab="",ylab="GDP",col="deepskyblue2")
boxplot(data$Deathrate,xlab="",ylab="GDP",col="deepskyblue2")
boxplot(data$Agriculture,xlab="",ylab="GDP",col="deepskyblue2")
boxplot(data$Industry,xlab="",ylab="GDP",col="deepskyblue2")
boxplot(data$Services,xlab="",ylab="GDP",col="deepskyblue2")
boxplot(data$Other,xlab="",ylab="GDP",col="deepskyblue2")


par(mfrow=c(1,2))

boxplot(data$Infant_Mortality_1000,main="Infant Mortality",xlab="",col="deepskyblue2")
boxplot(data$Phones_1000,xlab="",main="Phones per 1000",col="deepskyblue2")
boxplot(data$Literacy,xlab="",main="Literacy",col="deepskyblue2")

boxplot(data$Birthrate,xlab="",main="Birthrate",col="deepskyblue2")
boxplot(data$Arable,xlab="",main="Arable",col="deepskyblue2")
boxplot(data$Other,xlab="",main="Other",col="deepskyblue2")
boxplot(data$Agriculture,xlab="",main="Agriculture",col="deepskyblue2")
par(mfrow=c(1,2))


summary(data$Infant_Mortality_1000)
summary(data$Phones_1000)
summary(data$Literacy)
summary(data$Birthrate)
summary(data$Arable)
summary(data$Other)
summary(data$Agriculture)



correlationPValue <- matrix(nrow=ncol(data.quan),ncol=ncol(data.quan))
for(i in 1:ncol(data.quan)){
  for(j in 1:ncol(data.quan)){
    correlationPValue[i,j]<- spearman.test(data.quan[,i],data.quan[,j])$p.value
  }
}

colnames(correlationPValue)<-colnames(data.quan)
rownames(correlationPValue)<-colnames(data.quan)

hist(data$Continent)
table(data$Continent)
ggplot(data, aes(x=Continent)) + geom_histogram( stat="count", fill=("blue") )



#=====================PCA=======================


data_for_pca = data[,valid_correlation_data_names]


n.data_for_pca <- nrow(data_for_pca)
n.data_for_pca
p.data_for_pca <- ncol(data_for_pca)
p.data_for_pca


data_for_pca[is.na(data_for_pca)] <- 0


PCS.data_for_pca <- prcomp(data_for_pca,scale=TRUE)

eigen_values.data_for_pca = PCS.data_for_pca$sdev^2
fviz_eig(PCS.data_for_pca,ncp=12,addlabels=T,barfill="deepskyblue2",barcolor="deepskyblue4")

dim(PCS.data_for_pca$rotation)
PCS.data_for_pca$rotation

plot(PCS.data_for_pca$x[,1:2],pch=19,col=colors.X)
plot(PCS.data_for_pca$x[,1:3],pch=19,col=colors.X)
plot(PCS.data_for_pca$x[,1:4],pch=19,col=colors.X)
plot(PCS.data_for_pca$x[,2:3],pch=19,col=colors.X)
plot(PCS.data_for_pca$x[,2:4],pch=19,col=colors.X)
plot(PCS.data_for_pca$x[,3:4],pch=19,col=colors.X)

#significant PC to use
eval.data_for_pca <- PCS.data_for_pca$sdev^2
mean(eval.data_for_pca)
# The number of eigenvalues larger than the mean of them is 
sum(eval.data_for_pca>mean(eval.data_for_pca))

#first PC
plot(1:p.data_for_pca,PCS.data_for_pca$rotation[,1],pch=19,col="deepskyblue2",main="Weights for the first PC")
abline(h=0)
text(1:p.data_for_pca,PCS.data_for_pca$rotation[,1],labels=colnames(data_for_pca),pos=1,col="firebrick2",cex=0.5)
draw.circle(5,0,9,border="green2",lwd=3)
draw.circle(5,0,3,border="green2",lwd=3)
draw.circle(5,0,6,border="green2",lwd=3)


#second PC
plot(1:p.data_for_pca,PCS.data_for_pca$rotation[,2],pch=19,col="deepskyblue2",main="Weights for the first PC")
abline(h=0)
text(1:p.data_for_pca,PCS.data_for_pca$rotation[,2],labels=colnames(data_for_pca),pos=1,col="firebrick2",cex=0.5)

draw.circle(5,0,9,border="green2",lwd=3)
draw.circle(5,0,3,border="green2",lwd=3)
draw.circle(5,0,6,border="green2",lwd=3)



#third PC
plot(1:p.data_for_pca,PCS.data_for_pca$rotation[,3],pch=19,col="deepskyblue2",main="Weights for the first PC")
abline(h=0)
text(1:p.data_for_pca,PCS.data_for_pca$rotation[,3],labels=colnames(data_for_pca),pos=1,col="firebrick2",cex=0.5)
draw.circle(5,0,9,border="green2",lwd=3)
draw.circle(5,0,3,border="green2",lwd=3)
draw.circle(5,0,6,border="green2",lwd=3)

#fourth PC
plot(1:p.data_for_pca,PCS.data_for_pca$rotation[,4],pch=19,col="deepskyblue2",main="Weights for the first PC")
abline(h=0)
text(1:p.data_for_pca,PCS.data_for_pca$rotation[,4],labels=colnames(data_for_pca),pos=1,col="firebrick2",cex=0.5)
draw.circle(5,0,9,border="green2",lwd=3)
draw.circle(5,0,3,border="green2",lwd=3)
draw.circle(5,0,6,border="green2",lwd=3)



#5 PC
plot(1:p.data_for_pca,PCS.data_for_pca$rotation[,5],pch=19,col="deepskyblue2",main="Weights for the first PC")
abline(h=0)
text(1:p.data_for_pca,PCS.data_for_pca$rotation[,5],labels=colnames(data_for_pca),pos=1,col="firebrick2",cex=0.5)
draw.circle(5,0,9,border="green2",lwd=3)
draw.circle(5,0,3,border="green2",lwd=3)
draw.circle(5,0,6,border="green2",lwd=3)



#6 PC
plot(1:p.data_for_pca,PCS.data_for_pca$rotation[,6],pch=19,col="deepskyblue2",main="Weights for the first PC")
abline(h=0)
text(1:p.data_for_pca,PCS.data_for_pca$rotation[,6],labels=colnames(data_for_pca),pos=1,col="firebrick2",cex=0.5)
draw.circle(5,0,9,border="green2",lwd=3)
draw.circle(5,0,3,border="green2",lwd=3)
draw.circle(5,0,6,border="green2",lwd=3)



pairs(PCS.data_for_pca$x[,1:9],col=colors.X,pch=19,main="The first four PCs")

#correlation between principal components
corrplot(cor(data_for_pca,PCS.data_for_pca$x),is.corr=T)


#SIGUIENTE PASO, DECIDIR QUE AGRUPACIONES HACER PARA LOS ANALISIS


