install.packages('tidyverse')
install.packages('ggplot2')
install.packages('DataExplorer')
install.packages('funModeling')
install.packages('corrplot')
install.packages('MASS')
install.packages("car")
install.packages('dplyr')
install.packages('car')
install.packages('reshape2')
install.packages('usmap')
install.packages('data.table')

library(dplyr)
library(data.table)
library(tidyverse)
library(ggplot2)
library(DataExplorer)
library(funModeling)
library(corrplot)
library(usmap)
library(MASS)
library(car)
library(reshape2)


#-------------------------IMPORT THE DATA FILE----------------------

setwd("~/Desktop")
Canc = read.csv("cancer_reg.csv")

View(Canc)
Canc
#-----------------------DATA CLEANING AND PRE-PROCESSING----------------

str(Canc)
#show us type of variables and dimensions in our data set Canc
plot_str(Canc)

# Check for missing values
colSums(is.na(Canc))
#check for q_na if any missing value and the percentage it contributes
df_status(Canc) 

#imputation for column22.PctEmployed16_Over
na_mean<-mean(Canc$PctEmployed16_Over,na.rm=TRUE)
na_mean
na_median<-median(Canc$PctEmployed16_Over,na.rm=TRUE)
na_median
#mean and median comes up to same value so impute with mean
Canc$PctEmployed16_Over <- ifelse(is.na(Canc$PctEmployed16_Over), mean(Canc$PctEmployed16_Over, na.rm=TRUE), Canc$PctEmployed16_Over)
sum(is.na(Canc$PctEmployed16_Over))


#imputation for column25.PctPrivateCoverageAlone
na_mean<-mean(Canc$PctPrivateCoverageAlone,na.rm=TRUE)
na_mean
na_median<-median(Canc$PctPrivateCoverageAlone,na.rm=TRUE)
na_median
Canc$PctPrivateCoverageAlone <- ifelse(is.na(Canc$PctPrivateCoverageAlone), mean(Canc$PctPrivateCoverageAlone, na.rm=TRUE), Canc$PctPrivateCoverageAlone)
sum(is.na(Canc$PctPrivateCoverageAlone))

#drop column18.PctSomeCol18_24 (contains 74.99% NA)
Canc$PctSomeCol18_24 <- NULL

#validate if there is any more NA
df_status(Canc)
colSums(is.na(Canc))

#convert Geography into two columns - County and State
Canc <- separate( Canc, Geography, c("county","state") ,sep=", ", remove=TRUE, convert = FALSE )
Canc$Geography
Canc$county <- as.factor(Canc$county)
Canc$county 
Canc$state <- as.factor(Canc$state)
Canc$state 
df_status(Canc)

#assigning integer level values to the binnedinc factors of level 10
Canc$binnedInc<-as.integer(Canc$binnedInc)
Canc$binnedInc

#------------check for outliers

#the outliers in Y are % hence removed
outlier_Y<-boxplot(Canc$TARGET_deathRate, plot=FALSE)$out 
print(outlier_Y)
length(outlier_Y)
Canc<- Canc[-which(Canc$TARGET_deathRate %in% outlier_Y),]

#outliers in median age which are impossible numbers for age

outlier_MedianAge<-boxplot(Canc$MedianAge , plot=FALSE)$out

print(outlier_MedianAge[outlier_MedianAge>200])

length(outlier_MedianAge)

Canc<- Canc[-which(Canc$MedianAge %in% outlier_MedianAge),]  

str(Canc)
summary(Canc)

#---------------------UNIVARIATE ANALYSIS----------------------
#histogram, boxplots, summary

head(Canc)

#consider all non-categorical columns
Canc_num=Canc[c(1:8,10:12,15:34)]
head(Canc_num)
summary(Canc_num)


#----histograms in one plot
ggplot(melt(Canc_num), aes(x = value)) + 
  facet_wrap(~ variable, scales = "free", ncol = 4,nrow=8) + 
  geom_histogram(fill="blue") + labs(title="Histogram ")

#-----boxplots in one plot
ggplot(melt(Canc_num) , aes(y=value))+
  geom_boxplot()+facet_wrap(~variable, nrow = 8, scales = "free")


#-----summary 
summary(Canc_num)

#individual plots

1.
ggplot(data=Canc, aes(Canc$avgAnnCount)) + 
  geom_histogram(fill="blue")+
  labs(title="Histogram for ", x="avgAnnCount", y="Count")

ggplot(Canc, aes(x="",y=Canc$avgAnnCount)) + 
  geom_boxplot()

a<-boxplot(Canc$avgAnnCount, plot=FALSE)$out #detect outliers based on the IQR 
print(a)
length(a)

2.
ggplot(data=Canc, aes(Canc$avgDeathsPerYear)) + 
  geom_histogram(fill="blue")+
  labs(title="Histogram for ", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$avgDeathsPerYear)) + 
  geom_boxplot()

a<-boxplot(Canc$avgDeathsPerYear, plot=FALSE)$out 
print(a)
length(a)

3.
ggplot(data=Canc, aes(Canc$TARGET_deathRate)) + 
  geom_histogram(fill="blue")+
  labs(title="Histogram for ", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$TARGET_deathRate)) + 
  geom_boxplot()

4.
ggplot(data=Canc, aes(Canc$incidenceRate)) + 
  geom_histogram(fill="blue")+
  labs(title="Histogram for ", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$incidenceRate)) + 
  geom_boxplot()

5.
ggplot(data=Canc, aes(Canc$medIncome)) + 
  geom_histogram(fill="blue")+
  labs(title="Histogram for Age", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$medIncome)) + 
  geom_boxplot()

6.
ggplot(data=Canc, aes(Canc$popEst2015)) + 
  geom_histogram(fill="blue")+ labs(title="Histogram for ", x=" ", y="Count")

ggplot(Canc, aes(x="",y=Canc$popEst2015)) + 
  geom_boxplot()

7.
ggplot(data=Canc, aes(Canc$povertyPercent)) + 
  geom_histogram(fill="blue")+ labs(title="Histogram for Age", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$povertyPercent)) + 
  geom_boxplot()

8.
ggplot(data=Canc, aes(Canc$studyPerCap)) + 
  geom_histogram(fill="blue")+ labs(title="Histogram for Age", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$studyPerCap)) + 
  geom_boxplot()

9.
ggplot(data=Canc, aes(Canc$MedianAge)) + 
  geom_histogram(fill="blue")+ labs(title="Histogram for Age", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$MedianAge)) + 
  geom_boxplot()

10.
ggplot(data=Canc, aes(Canc$MedianAgeMale)) + 
  geom_histogram(fill="blue")+ labs(title="Histogram for Age", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$MedianAgeMale)) + 
  geom_boxplot()

11.

ggplot(data=Canc, aes(Canc$MedianAgeFemale)) + 
  geom_histogram(fill="blue")+ labs(title="Histogram for Age", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$MedianAgeFemale)) + 
  geom_boxplot()

12.
ggplot(data=Canc, aes(Canc$AvgHouseholdSize)) + 
  geom_histogram(fill="blue")+ labs(title="Histogram for Age", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$AvgHouseholdSize)) + 
  geom_boxplot()

13.
ggplot(data=Canc, aes(Canc$PercentMarried)) + 
  geom_histogram(fill="blue")+ labs(title="Histogram for Age", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$PercentMarried)) + 
  geom_boxplot()

14.
ggplot(data=Canc, aes(Canc$PctNoHS18_24)) + 
  geom_histogram(fill="blue")+ labs(title="Histogram for Age", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$PctNoHS18_24)) + 
  geom_boxplot()

15.
ggplot(data=Canc, aes(Canc$PctHS18_24)) + 
  geom_histogram(fill="blue")+ labs(title="Histogram for Age", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$PctHS18_24)) + 
  geom_boxplot()

16.
ggplot(data=Canc, aes(Canc$PctBachDeg18_24)) + 
  geom_histogram(fill="blue")+ labs(title="Histogram for Age", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$PctBachDeg18_24)) + 
  geom_boxplot()

17.

ggplot(data=Canc, aes(Canc$PctHS25_Over)) + 
  geom_histogram(fill="blue")+ labs(title="Histogram for Age", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$PctHS25_Over)) + 
  geom_boxplot()

18.
ggplot(data=Canc, aes(Canc$PctBachDeg25_Over)) + 
  geom_histogram(fill="blue")+ labs(title="Histogram for Age", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$PctBachDeg25_Over)) + 
  geom_boxplot()

19.
ggplot(data=Canc, aes(Canc$PctEmployed16_Over)) + 
  geom_histogram(fill="blue")+ labs(title="Histogram for Age", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$PctEmployed16_Over)) + 
  geom_boxplot()

20.
ggplot(data=Canc, aes(Canc$PctUnemployed16_Over)) + 
  geom_histogram(fill="blue")+ labs(title="Histogram for Age", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$PctUnemployed16_Over)) + geom_boxplot()

21.

ggplot(data=Canc, aes(Canc$PctPrivateCoverage)) + 
  geom_histogram(fill="blue")+ labs(title="Histogram for Age", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$PctPrivateCoverage)) + geom_boxplot()

22.
ggplot(data=Canc, aes(Canc$PctPrivateCoverageAlone)) + 
  geom_histogram(fill="blue")+ labs(title="Histogram for Age", x="Age", y="Count")

ggplot(Canc, aes(x="",y=Canc$PctPrivateCoverageAlone)) + geom_boxplot()

summary(Canc$PctPrivateCoverageAlone)


#-------------------------Bivariate and multivariate analysis--------------

#----Correlation matrix

#consider all non-categorical columns
cormat <- round(cor(Canc[, sapply(Canc, is.numeric)]),2)
cormat
class(cormat)
cormat[,3] #to view seperate with respect to Target

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 10, hjust = 1))+
  coord_fixed()


#--------Scatterplots with respect to target variable

pairs(Canc)
ggplot(Canc, aes(x=avgAnnCount, y=TARGET_deathRate)) + 
  geom_point()+ geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=avgDeathsPerYear, y=TARGET_deathRate)) + 
  geom_point()+ geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=incidenceRate, y=TARGET_deathRate)) + 
  geom_point()+ geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=medIncome, y=TARGET_deathRate)) + 
  geom_point()+ geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=popEst2015, y=TARGET_deathRate)) + 
  geom_point()+ geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=povertyPercent, y=TARGET_deathRate)) + 
  geom_point()+ geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=studyPerCap, y=TARGET_deathRate)) + 
  geom_point()+ geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=MedianAge, y=TARGET_deathRate)) + 
  geom_point()+ geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=MedianAgeMale, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=MedianAgeFemale, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=AvgHouseholdSize, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=PercentMarried, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=PctNoHS18_24, y=TARGET_deathRate)) + 
  geom_point()+ geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=PctHS18_24, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=PctBachDeg18_24, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=PctHS25_Over, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=PctBachDeg25_Over, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=PctEmployed16_Over, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=PctUnemployed16_Over, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=PctPrivateCoverage, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=PctPrivateCoverageAlone, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=PctEmpPrivCoverage, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=PctPublicCoverage, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=PctPublicCoverageAlone, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=PctMarriedHouseholds, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=BirthRate, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=PctWhite , y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=PctBlack, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=PctAsian, y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)

ggplot(Canc, aes(x=PctOtherRace , y=TARGET_deathRate)) + 
  geom_point()+
  geom_smooth(method=lm,se=FALSE)


#-------------Multivariate

ggplot(Canc, aes(x=Canc$state, y=Canc$medIncome)) +
  geom_bar(aes(fill = TARGET_deathRate), stat="identity",position=position_dodge()) + 
  labs(title = "Relationship between State and Median income with cancer death rate", x= "State",
       y = "Median Income per State", colour="TARGET_deathRate") + theme(axis.text.x = element_text(angle = 90))

ggplot(Canc, aes(x=Canc$state, y=Canc$PctPrivateCoverage)) +
  geom_bar(aes(fill = TARGET_deathRate), stat="identity",position=position_dodge()) + 
  labs(title = "Relationship between state and private coverage with cancer death rate", x= "State",
       y = "Residents with PrivateCoverage (%)", colour="TARGET_deathRate") + theme(axis.text.x = element_text(angle = 90))

ggplot(Canc, aes(x=Canc$state, y=Canc$PctPublicCoverageAlone)) +
  geom_bar(aes(fill = TARGET_deathRate), stat="identity",position=position_dodge()) + 
  labs(title = "Relationship between state and public coverage alone with cancer death rate", x= "State",
       y = "Residents with PctPublicCoverageAlone (no_private_assistance)", colour="TARGET_deathRate") + theme(axis.text.x = element_text(angle = 90))


#------------Cancer death rates by US state map-----------------


Canc_map<- data.table(Canc)
Canc_map_sum <-Canc_map[,list(Sum=sum(TARGET_deathRate)), by='state']

p<-plot_usmap(data = Canc_map_sum, values = "Sum", color = "red", labels=TRUE) + 
  scale_fill_continuous(low= 'white', high= 'red',limits = c(0,40000), 
                        breaks=c(5000,10000,15000,20000,30000,40000), name = "Cancer mortality rate(per 100,000 people)" , label = scales::comma )  + 
  theme(legend.position = "right")

p$layers[[2]]$aes_params$size <- 3
print(p)

a<-tapply(Canc$TARGET_deathRate, Canc$state, FUN=sum)
sort(a)


#----------------Transformations for normality

ggplot(data=Canc, aes(log(Canc$avgAnnCount))) + geom_histogram()
ggplot(data=Canc, aes(log(Canc$avgDeathsPerYear))) + geom_histogram()
ggplot(data=Canc, aes(log(Canc$incidenceRate))) +  geom_histogram()
ggplot(data=Canc, aes(log(Canc$popEst2015))) + geom_histogram()
ggplot(data=Canc, aes(log(Canc$povertyPercent))) + geom_histogram()
ggplot(data=Canc, aes(sqrt(Canc$PctNoHS18_24))) + geom_histogram()
ggplot(data=Canc, aes(log(Canc$PctBachDeg25_Over))) + geom_histogram()
ggplot(data=Canc, aes(sqrt(Canc$PctBachDeg18_24))) + geom_histogram()

ggplot(data=Canc, aes(log(Canc$PctWhite))) + geom_histogram()
ggplot(data=Canc, aes(log10(Canc$PctWhite^(1/3)))) + geom_histogram()
ggplot(data=Canc, aes(log(Canc$PctWhite+1))) + geom_histogram()

ggplot(data=Canc, aes(log(Canc$PctAsian+1))) + geom_histogram()

ggplot(data=Canc, aes(log(Canc$PctBlack+1))) + geom_histogram()
ggplot(data=Canc, aes(sqrt(Canc$PctBlack))) + geom_histogram()

ggplot(data=Canc, aes(log(Canc$PctPrivateCoverage))) + geom_histogram() #will not consider this

ggplot(data=Canc, aes(log(Canc$PctOtherRace))) +  geom_histogram() 
#when included in model generates -inf hence consider below one
ggplot(data=Canc, aes(sqrt(Canc$PctOtherRace))) +  geom_histogram() 
ggplot(data=Canc, aes(log10(Canc$PctOtherRace^(1/3)))) +  geom_histogram() 
ggplot(data=Canc, aes(log(Canc$PctOtherRace+1))) +  geom_histogram() 

ggplot(data=Canc, aes(sqrt(Canc$BirthRate))) +  geom_histogram()


Canc$avgAnnCount=log(Canc$avgAnnCount)
Canc$avgDeathsPerYear=log(Canc$avgDeathsPerYear)
Canc$incidenceRate=log(Canc$incidenceRate)
Canc$popEst2015=log(Canc$popEst2015)
Canc$povertyPercent=log(Canc$povertyPercent)
Canc$PctNoHS18_24=sqrt(Canc$PctNoHS18_24)
#Canc$PercentMarried=(Canc$PercentMarried**2) #not required
Canc$PctBachDeg25_Over=log(Canc$PctBachDeg25_Over)
#Canc$PctMarriedHouseholds=(Canc$PctMarriedHouseholds**2) #not required
Canc$PctAsian=log(Canc$PctAsian+1)
Canc$PctBlack=log(Canc$PctBlack+1)
Canc$PctOtherRace=log(Canc$PctOtherRace+1)
Canc$BirthRate=sqrt(Canc$BirthRate)



#------------------------MODEL BUILDING----------------

Canc$PctPrivateCoverageAlone <-NULL
Canc$PctSomeCol18_24 <- NULL
Canc$binnedInc <-NULL
Canc$Geography<-NULL

full.model <- lm(Canc$TARGET_deathRate ~., data = Canc)

step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)

summary(step.model)
vif(step.model)
step.model$anova

Canc2=Canc[c(1,2,3,4,6,10,13,15,17,18,19,21,22,28,29,30)]

str(Canc2)

set.seed(0)

#model 2 ran after removing values which were not significant

f.model <- lm(Canc2$TARGET_deathRate ~., data = Canc2)
step.model <- stepAIC(f.model, direction = "both", 
                      trace = FALSE)

summary(step.model)
vif(step.model)
step.model$anova


#---------Now re-engeneering the features to obtain a better model 

install.packages('h2o')
library(dplyr)
library(data.table)
library(h2o)
library(caret)
library(ggplot2)
library(mlbench)
library(Hmisc)
library(mctest)
library(car)
library(lmtest)
library(leaps)
library(DataExplorer)
library(modelr)
library(broom)
library(boot)
set.seed(323)
canc2 <- read.csv("cancer_reg.csv")
Hmisc::describe(canc2)

# data cleaning
canc2$binnedInc<-as.factor(canc2$binnedInc)
canc2<- tidyr::separate(canc2,"Geography",into = c("County/City","State"),sep = ",")

# Impute missing values and check for outliers
h2o.init(nthreads = -1,min_mem_size = "4g")
canc2<- as.h2o(canc2)

canc.glrm<-h2o.glrm(training_frame = canc2,k=10,init = "SVD",svd_method = "GramSVD",max_iterations =3000,min_step_size = 1e-6 )
h2o.performance(canc.glrm)
canc.pred<-predict(canc.glrm,canc2)
canc.pred
feat.name<- setdiff(names(canc.pred),"reconstr_TARGET_deathRate")
feat.name
canc.dl<-h2o.deeplearning(x =feat.name, training_frame = canc.pred, autoencoder = T,reproducible = T,seed = 323,hidden = c(10,2,10),epochs = 100)
summary(canc.dl)
canc.anom<-h2o.anomaly(canc.dl,canc.pred)

canc.anom<-canc.anom %>% as.data.frame() %>% mutate(row_number=1:3047)%>% filter(Reconstruction.MSE>.09)

canc.pred<-h2o.cbind(canc.pred,canc2[,c("County/City","State")])
canc2<- canc.pred%>% as.data.table()
canc2$State<-as.factor(canc2$State)
canc2$County.City<-as.factor(canc2$County.City)

canc.anom# possible outliers

canc2[canc.anom$row_number]

canc2[new_MedianAge>100,new_MedianAge]#outliers

h2o.shutdown(F); rm(canc.glrm,canc.pred,canc.dl); gc();detach("package:h2o", unload=TRUE)

#--------------------Feature Selection

#using the variables found from STEPAIC model earlier, but using reconstructed variables now
imp_feat<-c("reconstr_avgAnnCount",
            "reconstr_PercentMarried",
            "reconstr_PctEmployed16_Over",
            "reconstr_PctMarriedHouseholds",
            "reconstr_incidenceRate",
            "reconstr_PctHS18_24",
            "reconstr_PctPrivateCoverage",
            "reconstr_BirthRate",
            "reconstr_popEst2015",
            "reconstr_PctHS25_Over",
            "reconstr_PctEmpPrivCoverage",
            "reconstr_MedianAgeMale",
            "reconstr_PctBachDeg25_Over",
            "reconstr_PctOtherRace")

#Not all the variables are significant and VIF>5, so we remove some variables and try forward stepwise again
imp_feat<-c("reconstr_avgAnnCount",
            "reconstr_PercentMarried",
            "reconstr_PctEmployed16_Over",
            "reconstr_PctMarriedHouseholds",
            "reconstr_incidenceRate",
            "reconstr_PctHS18_24",
            "reconstr_PctPrivateCoverage",
            "reconstr_BirthRate",
            "reconstr_popEst2015",
            "reconstr_PctHS25_Over")

#finally reached these set of variables which had all high significance and VIF<5. 

imp_feat<-c("reconstr_PctHS18_24",
            "reconstr_PctPrivateCoverage",
            "reconstr_BirthRate",
            "reconstr_incidenceRate",
            "reconstr_PctHS25_Over",
            "reconstr_avgAnnCount")


imp_feat[7]<-"reconstr_TARGET_deathRate"

summary(canc2[,imp_feat,with=F])

install.packages("GGally")
library(GGally)

imp_feat<-c("reconstr_PctHS18_24",
            "reconstr_PctPrivateCoverage",
            "reconstr_incidenceRate",
            "reconstr_PctHS25_Over",
            "reconstr_avgAnnCount")


GGally::ggpairs(canc2[,imp_feat,with=F])




#-----------------------Model Selection
install.packages('ridge')
library(ridge)

set.seed(323)

canc2$reconstr_binnedInc=NULL
canc2$County.City=NULL
canc2$State=NULL

intrain<-createDataPartition(canc2$reconstr_TARGET_deathRate,p=.70,list=F)
intrain
training<-canc2[intrain]
training
y_train=training$reconstr_TARGET_deathRate
y_train
x_train=training[,-3]
x_train

testing<-canc2[-intrain]
testing
y_test=testing$reconstr_TARGET_deathRate
y_test
x_test=testing[,-3]
x_test

#this is essentially forward stepwise method
models<-list()
for(i in seq_along(imp_feat[-7])){
  
  
  models[[i]]<- lm(reconstr_TARGET_deathRate~.,data=training[,imp_feat[c(1:i,7)],with=F])
  
  
}
lapply(models,summary)

do.call(anova,models)


#final model 
model1=lm(reconstr_TARGET_deathRate~reconstr_PctHS18_24+reconstr_PctPrivateCoverage+reconstr_BirthRate+reconstr_incidenceRate+reconstr_PctHS25_Over+reconstr_avgAnnCount, data=training) 
summary(model1)
vif(model1)
anova(model1)


#----------------------------------ASSUMPTION CHECKING 

#linearity assumption 
par(mfrow=c(3,2))

plot(training$reconstr_PctHS18_24,training$reconstr_TARGET_deathRate)
abline(lm(training$reconstr_TARGET_deathRate ~ training$reconstr_PctHS18_24))

plot(training$reconstr_PctPrivateCoverage,training$reconstr_TARGET_deathRate)
abline(lm(training$reconstr_TARGET_deathRate ~training$reconstr_PctPrivateCoverage ))

plot(training$reconstr_BirthRate,training$reconstr_TARGET_deathRate)
abline(lm(training$reconstr_TARGET_deathRate ~ training$reconstr_BirthRate))

plot(training$reconstr_incidenceRate,training$reconstr_TARGET_deathRate)
abline(lm(training$reconstr_TARGET_deathRate ~training$reconstr_incidenceRate ))

plot(training$reconstr_PctHS25_Over,training$reconstr_TARGET_deathRate)
abline(lm(training$reconstr_TARGET_deathRate ~ training$reconstr_PctHS25_Over))

plot(training$reconstr_avgAnnCount,training$reconstr_TARGET_deathRate)
abline(lm(training$reconstr_TARGET_deathRate ~training$reconstr_avgAnnCount ))


#residual analysis
par(mfrow=c(2,2))
#for residual vs fitted(homoscedasticity) and qq plot(normality)
plot(model1)


#residuals vs. independent variables
par(mfrow=c(3,2))
plot(training$reconstr_PctPrivateCoverage, model1$residuals, 
     ylab="Residuals", xlab="reconstr_PctPrivateCoverage", main = "Residuals vs reconstr_PctPrivateCoverage") 
abline(0, 0)                  # the horizon

plot(training$reconstr_BirthRate, model1$residuals, 
     ylab="Residuals", xlab="reconstr_BirthRate",main = "Residuals vs reconstr_BirthRate") 
abline(0, 0)                  # the horizon


plot(training$reconstr_PctHS18_24,model1$residuals,
     ylab="Residuals", xlab="reconstr_PctHS18_24", main = "Residuals vs reconstr_PctHS18_24") 
abline(0, 0)                  # the horizon

plot(training$reconstr_incidenceRate,model1$residuals,
     ylab="Residuals", xlab="reconstr_incidenceRate", main = "Residuals vs reconstr_incidenceRate") 
abline(0, 0)                  # the horizon

plot(training$reconstr_PctHS25_Over,model1$residuals,
     ylab="Residuals", xlab="reconstr_PctHS25_Over", main = "Residuals vs reconstr_PctHS25_Over") 
abline(0, 0)                  # the horizon

plot(training$reconstr_avgAnnCount,model1$residuals,
     ylab="Residuals", xlab="reconstr_avgAnnCount", main = "Residuals vs reconstr_avgAnnCount") 
abline(0, 0)                  # the horizon




#independence of errors
dwtest(model1)

#normality
par(mfrow=c(1,1))
hist(residuals(model1), main='Histogram of Residuals')

#model prediction

model_results<-predict(models[[6]],testing)

data.frame(model_results,actual=testing$reconstr_TARGET_deathRate) %>% ggplot(aes(x=model_results,y=actual)) + geom_point()+stat_smooth(method="lm",show.legend = T)
RMSE(pred = model_results,obs = testing$reconstr_TARGET_deathRate)


#--------------------------Ridge regression

ridge_mod = glmnet(xtrainmat, 
                   ytrainmat, 
                   alpha = 0) # Fit lasso model on training data

plot(ridge_mod)  

ridge=cv.glmnet(xtrainmat,ytrainmat,alpha=0)
par(mfrow=c(1,1))
plot(ridge)

bestlamridge = ridge$lambda.min # Select lamda that minimizes training MSE
bestlamridge
ridge_pred = predict(ridge_mod, s = bestlam, newx = xtestmat) # Use best lambda to predict test data
mean((ridge_pred - y_test)^2) # Calculate test MSE
RMSE(pred = ridge_pred,obs = y_test)

#-------------------------------lasso regression
install.packages('glmnet')
library(glmnet)
xtrainmat <- as.matrix(x_train)
ytrainmat=as.matrix(y_train)
xtestmat=as.matrix(x_test)
lasso_mod = glmnet(xtrainmat, 
                   ytrainmat, 
                   alpha = 1) # Fit lasso model on training data

plot(lasso_mod)  
lasso=cv.glmnet(xtrainmat,ytrainmat,alpha=1)
par(mfrow=c(1,1))
plot(lasso)

bestlamlasso = lasso$lambda.min # Select lamda that minimizes training MSE
bestlamlasso
lasso_pred = predict(lasso_mod, s = bestlam, newx = xtestmat) # Use best lambda to predict test data
mean((lasso_pred - y_test)^2) # Calculate test MSE
RMSE(pred = lasso_pred,obs = y_test)


out = glmnet(xtrainmat, ytrainmat, alpha = 1) # Fit lasso model on full dataset
lasso_coef = predict(out, type = "coefficients", s = bestlam)[1:20,] # Display coefficients using lambda chosen by CV
lasso_coef



#------------------------------random forest 
install.packages('randomForest')
library(randomForest)
require(caTools)
install.packages('quantmod')
library(quantmod)


rf <- randomForest(
  y_train ~ .,
  data=x_train
)

pred_rf = predict(rf, newdata=x_test)
pred_rf
RMSE(pred = pred_rf,obs = y_test)
postResample(pred_rf, y_test)

#calculate accuracy
mean (apply(compare_rf, 1, min)/apply(compare_rf, 1, max)) 



#------------------------------cross validation --- LOOCV
# Define training control
train.control <- trainControl(method = "LOOCV")
# Train the model
model <- train(reconstr_TARGET_deathRate ~., data = canc2, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)










