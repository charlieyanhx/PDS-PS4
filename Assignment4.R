#sample statistic
#1
library(tidyverse)
library(dplyr)
data1<-read.csv('http://politicaldatascience.com/PDS/Datasets/GSS-data.csv', stringsAsFactors = F)

#Convert this poleff11 into a numeric where higher values indicate higher levels of political efficacy 
#(1- strongly agrees with the statement; 5- strongly disagrees with the statment) and all other values 
#(‘Cant choose’ etc.) become NA’s.
data1$poleff11[data1$poleff11=="Strongly agree"]<-5
data1$poleff11[data1$poleff11=="Agree"]<-4
data1$poleff11[data1$poleff11=="Neither agree nor disagree"]<-3
data1$poleff11[data1$poleff11=="Disagree"]<-2
data1$poleff11[data1$poleff11=="Strongly disagree"]<-1
data1$poleff11[data1$poleff11!=5&data1$poleff11!=4&data1$poleff11!=3&data1$poleff11!=2&data1$poleff11!=1]<-"na"
data1$poleff11[data1$poleff11=="Not applicable"]<-"na"
data1$poleff11[data1$poleff11=="Cant choose"]<-"na"
data1$poleff11[data1$poleff11=="No answer"]<-"na"

#assuming only 4 (Agree) and 5 (Strongly agree) as though they "have a say in the government?"
temp<-data1%>%count(poleff11) #tally the number of each result
(temp$n[temp$poleff11==5]+temp$n[temp$poleff11==4])/count(data1) #number of 4+number of 5/total

#sample of 25 from this dataset
sample_25=data1%>%sample_n(size=25) #random sample of 25
temp<-sample_25%>%count(poleff11) #tally the number of each result

have_a_say<-function(temp,sample){
  if(length(temp$n[temp$poleff11==5])==0&length(temp$n[temp$poleff11==4])==0){
    return (0/count(sample))
  }
  else if(length(temp$n[temp$poleff11==5])==0){
    return (temp$n[temp$poleff11==4]/count(sample))
  }
  else if(length(temp$n[temp$poleff11==4])==0){
    return (temp$n[temp$poleff11==5]/count(sample))
  }
  else{
    return ((temp$n[temp$poleff11==5]+temp$n[temp$poleff11==4])/count(sample))
  }
}

have_a_say(temp,sample_25)

#repeat sample of 25 for 500 times

p_25 = c(1:500)


for(i in 0:500){ #use for loop to repeat
  trials_25=data1%>%sample_n(size=25) #sample of 25
  temp<-trials_25%>%count(poleff11) #tally the number of each result
  temp_p=have_a_say(temp,trials_25) #store calculated proportion to temp
  p_25[i]=temp_p #add proportion of each iteration to tracking variable sum_p
}


#repeat sample of 100 for 500 times
p_100 = c(1:500)

for(i in 0:500){ #use for loop to repeat
  trials_100=data1%>%sample_n(size=100) #sample of 100
  temp<-trials_100%>%count(poleff11) #tally the number of each result
  temp_p=have_a_say(temp,trials_100) #store calculated proportion to temp
  p_100[i]=temp_p #add proportion of each iteration to tracking variable sum_p
}


#Draw a histogram of the sampling distribution for the two trials
p1 <- hist(unlist(p_25))                     
p2 <- hist(unlist(p_100))                     
plot( p1, col="red", xlim=c(0,0.4),xlab = "Sample Proportion", main="")  
plot( p2, col="blue", xlim=c(0,0.4), add=T) 


#With larger sample size, the sample proportions were distributed closer to the true population proportion in a smaller range
#as smaller sample size produces proportions that are distributed farther to the true population proportion in a larger range

install.packages("readr")
install.packages("rsample")
library(readr)
library(rsample)
#supervised learning
#1
data2<-read.csv('http://politicaldatascience.com/PDS/Datasets/SenateForecast/PollingCandidateData92-16.csv', stringsAsFactors = F)
#2
#reducing data into election level
data2<-data2%>%group_by(Candidateidentifier)%>%summarise(startdate=min(startdate), samplesize=sum(samplesize), Poll=sum(Poll),year=mean(year),Republican=mean(Republican), Democrat=mean(Democrat), experienced =mean(experienced), Percentage.of.Vote.won.x=mean(Percentage.of.Vote.won.x), pvi=mean(pvi), GenericBallotSept=mean(GenericBallotSept), numberSupport=mean(numberSupport), Incumbent=mean(Incumbent), weightexperience=mean(weightexperience),PercentageRaised=mean(PercentageRaised), win=mean(win))
#3
#Randomly select 20 percent of your data to use as a “validation sample”
split_data <- initial_split(data2, prop=.8)
train_data <- training(split_data)
test_data <- testing(split_data)

#4 
#create two linear regression models to predict vote share for incumbents.
#model 1
model <- lm(Percentage.of.Vote.won.x ~ Democrat + factor(Incumbent), data =train_data)
prediction <- predict(model, newdata = elect_test)
model1r <- c(model1r, sqrt(mean((prediction-test_data$Percentage.of.Vote.won.x)^2)))  

#model2
model <- lm(Percentage.of.Vote.won.x ~ pvi + factor(Incumbent), data = train_data)
prediction <- predict(model, newdata = test_data)
sqrt(mean((prediction-test_data$Percentage.of.Vote.won.x)^2))

#iterated version for 1000 times
model1r<-c()
for(i in 1:1000){
  split_data <- initial_split(data2, prop=.8)
  train_data <- training(split_data)
  test_data <- testing(split_data)
  model <- lm(Percentage.of.Vote.won.x ~ Democrat + factor(Incumbent), data =train_data)
  prediction <- predict(model, newdata = test_data)
  model1r <- c(model1r, sqrt(mean((prediction-test_data$Percentage.of.Vote.won.x)^2))) 
}
mean(model1r)

model2r<-c()
for(i in 1:1000){
  split_data <- initial_split(data2, prop=.8)
  train_data <- training(split_data)
  test_data <- testing(split_data)
  model <- lm(Percentage.of.Vote.won.x ~ pvi + factor(Incumbent), data = train_data)
  prediction <- predict(model, newdata = test_data)
  model2r <- c(model2r, sqrt(mean((prediction-test_data$Percentage.of.Vote.won.x)^2))) 
}
mean(model2r)

combined_data <- data.frame(cbind(mean(model1r), mean(model2r)))
colnames(combined_data) <- c("model1", "model2") 
combined_data


#5
#  LINEAR CLASSIFIER
Model_LC<- glm(win ~ Incumbent + weightexperience, family="binomial", data=data2)
summary(Model_LC)
Model_LCP <- predict(Model_LC, type="response")
boxplot(Model_LCP ~ data2$weightexperience, xlab="Weighted Experience", ylab="Predicted Probabilities")
binaryPred <- (MModel_LCP > 0.5) * 1


#  RANDOM FOREST MODEL
install.packages("randomForest")
library(randomForest)
data2$win.factor <- as.factor(data2$win)
formula_RFM<- as.formula("win.factor ~ Incumbent + weightexperience")
Model_RFM <- randomForest(formula_RFM, data=data2, ntree=200, mtry=2)
Model_RFM
#acuracy checking
prediction_RFM <- predict(Model_RFM)
prediction_RFM

#  K-NEAREST NEIGHBORS

library(class)
data2 <- data2[,c("win", "Incumbent", "weightexperience")]
data2$win <- as.numeric(data2$win) - 1
data2$win <- (as.numeric(data2$win) + rnorm(length(data2$win), 0, .001))
#acuracy checking
Model_KNN <- knn(data2, test=data2, cl=data2$win, k=10)
table(Model_KNN, data2$win)

#6 I dont really know how to do this part
data2018 <- read.csv("http://politicaldatascience.com/PDS/Datasets/SenateForecast/PollingCandidateData18.csv")

