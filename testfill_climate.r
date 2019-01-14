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
library("ISLR")
library("FactoMineR")
library("cluster")



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
                           "GDP", "Literacy", "Phones_1000", "Arable", "Crops","Other", 
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

scaled_data = scale(data_for_pca)
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
text(1:p.data_for_pca,PCS.data_for_pca$rotation[,1],labels=colnames(data_for_pca),pos=1,col="firebrick2",cex=1.2)
draw.circle(5,0,9,border="green2",lwd=3)
draw.circle(5,0,3,border="green2",lwd=3)
draw.circle(5,0,6,border="green2",lwd=3)


#second PC
plot(1:p.data_for_pca,PCS.data_for_pca$rotation[,2],pch=19,col="deepskyblue2",main="Weights for the Second PC")
abline(h=0)
text(1:p.data_for_pca,PCS.data_for_pca$rotation[,2],labels=colnames(data_for_pca),pos=1,col="firebrick2",cex=1.2)

draw.circle(5,0,9,border="green2",lwd=3)
draw.circle(5,0,3,border="green2",lwd=3)
draw.circle(5,0,6,border="green2",lwd=3)



#third PC
plot(1:p.data_for_pca,PCS.data_for_pca$rotation[,3],pch=19,col="deepskyblue2",main="Weights for the Third PC")
abline(h=0)
text(1:p.data_for_pca,PCS.data_for_pca$rotation[,3],labels=colnames(data_for_pca),pos=1,col="firebrick2",cex=1.2)
draw.circle(5,0,9,border="green2",lwd=3)
draw.circle(5,0,3,border="green2",lwd=3)
draw.circle(5,0,6,border="green2",lwd=3)

#fourth PC
plot(1:p.data_for_pca,PCS.data_for_pca$rotation[,4],pch=19,col="deepskyblue2",main="Weights for the fourth PC")
abline(h=0)
text(1:p.data_for_pca,PCS.data_for_pca$rotation[,4],labels=colnames(data_for_pca),pos=1,col="firebrick2",cex=1.2)
draw.circle(5,0,9,border="green2",lwd=3)
draw.circle(5,0,3,border="green2",lwd=3)
draw.circle(5,0,6,border="green2",lwd=3)



#5 PC
plot(1:p.data_for_pca,PCS.data_for_pca$rotation[,5],pch=19,col="deepskyblue2",main="Weights for the fifth PC")
abline(h=0)
text(1:p.data_for_pca,PCS.data_for_pca$rotation[,5],labels=colnames(data_for_pca),pos=1,col="firebrick2",cex=1.2)
draw.circle(5,0,9,border="green2",lwd=3)
draw.circle(5,0,3,border="green2",lwd=3)
draw.circle(5,0,6,border="green2",lwd=3)



#6 PC
plot(1:p.data_for_pca,PCS.data_for_pca$rotation[,6],pch=19,col="deepskyblue2",main="Weights for the first PC")
abline(h=0)
text(1:p.data_for_pca,PCS.data_for_pca$rotation[,6],labels=colnames(data_for_pca),pos=1,col="firebrick2",cex=1)
draw.circle(5,0,9,border="green2",lwd=3)
draw.circle(5,0,3,border="green2",lwd=3)
draw.circle(5,0,6,border="green2",lwd=3)





pairs(PCS.data_for_pca$x[,1:9],col=colors.X,pch=19,main="The first four PCs")

#correlation between principal components method = "number"
corrplot(method = "number",cor(data_for_pca,PCS.data_for_pca$x),is.corr=T)


#SIGUIENTE PASO, DECIDIR QUE AGRUPACIONES HACER PARA LOS ANALISIS


# FACTOR ANALYSIS 

#NUMERO DE FACTORES A CONSIDERAR



fviz_eig(PCS.data_for_pca,ncp=p,addlabels=T,barfill="deepskyblue2",barcolor="deepskyblue4")
r <- 5

M.data_for_pca <- PCS.data_for_pca$rotation[,1:r] %*% diag(PCS.data_for_pca$sdev[1:r])
M.data_for_pca <- varimax(M.data_for_pca)
M.data_for_pca <- loadings(M.data_for_pca)[1:p.data_for_pca,1:r]

p = p.data_for_pca

# The first factor appears to be an index of extraversion 

plot(1:p,M.data_for_pca[,1],pch=19,col="deepskyblue2",xlab="",ylab="Weights",main="Weights for the first factor")
abline(h=0)
text(1:p,M.data_for_pca[,1],labels=colnames(data_for_pca),pos = 1,col="firebrick2",cex=0.65)

# The second factor appears to be an index of professional conscientiousness 

plot(1:p,M.data_for_pca[,2],pch=19,col="deepskyblue2",xlab="",ylab="Weights",main="Weights for the second factor")
abline(h=0)
text(1:p,M.data_for_pca[,2],labels=colnames(data_for_pca),pos = 1,col="firebrick2",cex=0.75)

# The third factor appears to be an index of roughness 

plot(1:p,M.data_for_pca[,3],pch=19,col="deepskyblue2",xlab="",ylab="Weights",main="Weights for the third factor")
abline(h=0)
text(1:p,M.data_for_pca[,3],labels=colnames(data_for_pca),pos = 1,col="firebrick2",cex=0.85)

# The fourth factor appears to be an index of restlessness 

plot(1:p,M.data_for_pca[,4],pch=19,col="deepskyblue2",xlab="",ylab="Weights",main="Weights for the fourth factor")
abline(h=0)
text(1:p,M.data_for_pca[,4],labels=colnames(data_for_pca),pos = 1,col="firebrick2",cex=0.95)

# The fifth factor appears to be an index of friendliness

plot(1:p,M.data_for_pca[,5],pch=19,col="deepskyblue2",xlab="",ylab="Weights",main="Weights for the fifth factor")
abline(h=0)
text(1:p,M.data_for_pca[,5],labels=colnames(data_for_pca),pos = 1,col="firebrick2",cex=0.85)


scaled_data = scale(data_for_pca)
Sigma.nu.data_for_pca <- diag(diag(cov(scaled_data) - M.data_for_pca %*% t(M.data_for_pca)))

##################################################################################################################
# Communalities and uniquenesses
##################################################################################################################
comm.data_for_pca <- diag(M.data_for_pca %*% t(M.data_for_pca))
comm.data_for_pca
sort(comm.data_for_pca ,decreasing=TRUE)

uniq.data_for_pca <- diag(Sigma.nu.data_for_pca)
uniq.data_for_pca
names(uniq.data_for_pca) <- names(comm.data_for_pca)
uniq.data_for_pca
sort(uniq.data_for_pca,decreasing=TRUE)


F.data_for_pca <- scaled_data %*% solve(Sigma.nu.data_for_pca) %*% M.data_for_pca %*% solve(t(M.data_for_pca) %*% solve(Sigma.nu.data_for_pca) %*% M.data_for_pca)
pairs(F.pca,pch=19,col="deepskyblue2")
corrplot(cor(F.data_for_pca),order="hclust")

dim(F.data_for_pca)

##################################################################################################################
# Estimate the residuals
##################################################################################################################

Nu.data_for_pca <- scaled_data - F.data_for_pca %*% t(M.data_for_pca)
corrplot(cor(Nu.data_for_pca),order="hclust")


##################################################################################################################
##################################################################################################################
# Principal factor analysis
##################################################################################################################
##################################################################################################################

R.data_for_pca <- cor(data_for_pca)
MM <- R.data_for_pca- Sigma.nu.data_for_pca
MM.eig <- eigen(MM)
MM.values <- MM.eig$value
MM.vectors <- MM.eig$vectors


##################################################################################################################
# Estimate the matrix M and use the varimax rotation for interpretability
##################################################################################################################

M.data_for_pfa <- MM.eig$vectors[,1:r] %*% diag(MM.eig$values[1:r])^(1/2)
M.data_for_pfa <- varimax(M.data_for_pfa)
M.data_for_pfa <- loadings(M.data_for_pfa)[1:p,1:r]



# Compare both estimates

plot(M.data_for_pca[,1],M.data_for_pfa[,1],pch=19,col="deepskyblue2",main="First factors with PCFA and PFA")
plot(M.data_for_pca[,2],M.data_for_pfa[,2],pch=19,col="deepskyblue2",main="Second factors with PCFA and PFA")
plot(M.data_for_pca[,3],M.data_for_pfa[,3],pch=19,col="deepskyblue2",main="Third factors with PCFA and PFA")
plot(M.data_for_pca[,4],M.data_for_pfa[,4],pch=19,col="deepskyblue2",main="Fourth factors with PCFA and PFA")
plot(M.data_for_pca[,5],M.data_for_pfa[,5],pch=19,col="deepskyblue2",main="Fifth factors with PCFA and PFA")


Sigma.nu.data_for_pfa <- diag(diag(cov(scaled_data) - M.data_for_pfa %*% t(M.data_for_pfa)))
plot(diag(Sigma.nu.data_for_pca),diag(Sigma.nu.data_for_pfa),pch=19,col="deepskyblue2",main="Noise variances with PCFA and PFA")


##################################################################################################################
# Communalities and uniquenesses
##################################################################################################################

comm.data_for_pfa <- diag(M.data_for_pfa %*% t(M.data_for_pfa))
names(comm.data_for_pfa) <- colnames(scaled_data)
comm.data_for_pfa
sort(comm.data_for_pfa,decreasing=TRUE)
sort(comm.data_for_pca,decreasing=TRUE)


# With PCFA, the communalities are smaller
uniq.data_for_pfa <- diag(Sigma.nu.data_for_pfa)
uniq.data_for_pfa
names(uniq.data_for_pfa) <- names(comm.data_for_pfa)
uniq.data_for_pfa
sort(uniq.data_for_pfa,decreasing=TRUE)
sort(uniq.data_for_pfa,decreasing=TRUE)


# With PCFA, the uniquenesses are smaller


##################################################################################################################
# Estimate the factor scores
##################################################################################################################

F.data_for_pfa <- scaled_data %*% solve(Sigma.nu.data_for_pfa) %*% M.data_for_pfa %*% solve(t(M.data_for_pfa) %*% solve(Sigma.nu.data_for_pfa) %*% M.data_for_pfa)
pairs(F.pfa,pch=19,col="deepskyblue2")
corrplot(cor(F.data_for_pfa),order="hclust")

# See that the factors are uncorrelated

# Obtain the correlation matrix between the PCFA and PFA estimates

cor(F.data_for_pca,F.data_for_pfa)
corrplot(cor(F.data_for_pca,F.data_for_pfa))

##################################################################################################################
# Estimate the residuals
##################################################################################################################

Nu.data_for_pfa <- scaled_data - F.data_for_pfa %*% t(M.data_for_pfa)
corrplot(cor(Nu.data_for_pfa),order="hclust")



# Obtain the correlation matrix between the PCFA and PFA estimates

corrplot(cor(Nu.data_for_pca,Nu.data_for_pfa))


##################################################################################################################
# How many factors?
##################################################################################################################

# Start with one factor

FA.Y.1 <- factanal(scaled_data,factors=1,rotation="varimax",scores="Bartlett")
FA.Y.1$STATISTIC
FA.Y.1$PVAL


# Then, two factors

FA.Y.2 <- factanal(scaled_data,factors=2,rotation="varimax",scores="Bartlett")
FA.Y.2$STATISTIC
FA.Y.2$PVAL

# Then, three factors

FA.Y.3 <- factanal(scaled_data,factors=3,rotation="varimax",scores="Bartlett")
FA.Y.3$STATISTIC
FA.Y.3$PVAL

# Then, four factors

FA.Y.4 <- factanal(scaled_data,factors=4,rotation="varimax",scores="Bartlett")
FA.Y.4$STATISTIC
FA.Y.4$PVAL

# Then, five factors

FA.Y.5 <- factanal(scaled_data,factors=5,rotation="varimax",scores="Bartlett")
FA.Y.5$STATISTIC
FA.Y.5$PVAL

# See that the null hypothesis is always rejected. This is probably because the data is non Gaussian
# Try with five factors as in the previous cases

##################################################################################################################
# Get the loading matrix
##################################################################################################################

M.data_for_mle <- loadings(FA.Y.5)[1:p,1:r]

# Compare with PFA estimates

plot(M.data_for_pfa[,1],M.data_for_mle[,1],pch=19,col="deepskyblue2",main="First factors with PFA and MLE")
plot(M.data_for_pfa[,2],M.data_for_mle[,2],pch=19,col="deepskyblue2",main="Second factors with PFA and MLE")
plot(M.data_for_pfa[,3],M.data_for_mle[,3],pch=19,col="deepskyblue2",main="Third factors with PFA and MLE")
plot(M.data_for_pfa[,3],M.data_for_mle[,4],pch=19,col="deepskyblue2",main="Third factors with PFA and MLE")
plot(M.data_for_pfa[,3],M.data_for_mle[,5],pch=19,col="deepskyblue2",main="Third factors with PFA and MLE")
plot(M.data_for_pfa[,4],M.data_for_mle[,3],pch=19,col="deepskyblue2",main="Fourth factors with PFA and MLE")
plot(M.data_for_pfa[,5],M.data_for_mle[,4],pch=19,col="deepskyblue2",main="Fifth factors with PFA and MLE")



##################################################################################################################
# Estimate the covariance matrix of the errors
##################################################################################################################

Sigma.nu.data_for_mle <- diag(diag(cov(scaled_data) - M.data_for_mle %*% t(M.data_for_mle)))
plot(diag(Sigma.nu.data_for_pfa),diag(Sigma.nu.data_for_mle),pch=19,col="deepskyblue2",main="Noise variances with PFA and MLE")



# There are some small differences

##################################################################################################################
# Communalities and uniquenesses
##################################################################################################################

comm.data_for_mle <- diag(M.data_for_mle %*% t(M.data_for_mle))
names(comm.data_for_mle) <- colnames(scaled_data)
comm.data_for_mle
sort(comm.data_for_mle,decreasing=TRUE)
sort(comm.data_for_pfa,decreasing=TRUE)

# The communalities are quite close

uniq.data_for_mle <- diag(Sigma.nu.data_for_mle)
uniq.data_for_mle
names(uniq.data_for_mle) <- names(comm.data_for_mle)
uniq.data_for_mle
sort(uniq.data_for_mle,decreasing=TRUE)
sort(uniq.data_for_pfa,decreasing=TRUE)

##################################################################################################################
# Estimate the factor scores
##################################################################################################################

F.data_for_mle <- scaled_data %*% solve(Sigma.nu.data_for_mle) %*% M.data_for_mle %*% solve(t(M.data_for_mle) %*% solve(Sigma.nu.data_for_mle) %*% M.data_for_mle)
pairs(F.data_for_mle,pch=19,col="deepskyblue2")
corrplot(cor(F.data_for_mle),order="hclust")



dim(F.data_for_pfa)
dim(F.data_for_mle)
cor(F.data_for_pfa,F.data_for_mle)
corrplot(cor(F.data_for_pfa,F.data_for_mle))


##################################################################################################################
# Estimate the residuals
##################################################################################################################



Nu.data_for_mle <- scaled_data - F.data_for_mle %*% t(M.data_for_mle)
corrplot(cor(Nu.data_for_mle),order="hclust")

# As before, the residuals show some minor correlation that the model is not able to explain

# Obtain the correlation matrix between the PFA and MLE estimates

corrplot(cor(Nu.data_for_pfa,Nu.data_for_mle))

##### CLUSTER ANALYSIS

PCS.data_for_pca


plot(PCS.data_for_pca$x[,1:2],pch=20,col="deepskyblue2")
kmeans.data_for_pca <- kmeans(scaled_data ,centers=2,iter.max=1000,nstart=100)

colors.kmeans.data_for_pca <- c("deepskyblue2","firebrick2")[kmeans.data_for_pca$cluster]
plot(PCS.data_for_pca$x[,1:2],pch=20,col=colors.kmeans.data_for_pca)



fviz_nbclust(scaled_data,kmeans,method="wss",k.max=10)
fviz_nbclust(scaled_data,kmeans,method="silhouette",k.max=10)
fviz_nbclust(scaled_data,kmeans,method="gap",k.max=10,nboot=100)


kmeans.data_for_pca <- kmeans(scaled_data,centers=4,iter.max=1000,nstart=100)

colors.kmeans.data_for_pca <- c("deepskyblue2","firebrick2","orange","chartreuse")[kmeans.data_for_pca$cluster]
plot(PCS.data_for_pca$x[,1:2],pch=20,col=colors.kmeans.data_for_pca)

fviz_cluster(kmeans.data_for_pca,data=PCS.data_for_pca$x[,1:2])

##################################################################################################################
# Silhouette plot for the solution

sil.kmeans.data_for_pca <- silhouette(kmeans.data_for_pca$cluster,dist(scaled_data,"euclidean"))
plot(sil.kmeans.data_for_pca,col="deepskyblue2")



##################################################################################################################
# Perform k-means clustering for the principal components of the data set
##################################################################################################################

















