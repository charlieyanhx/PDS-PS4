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
plot( p1, col="red", xlim=c(0,0.4),xlab = "Sample Proportion")  
plot( p2, col="blue", xlim=c(0,0.4), add=T) 


#With larger sample size, the sample proportions were distributed closer to the true population proportion in a smaller range
#as smaller sample size produces proportions that are distributed farther to the true population proportion in a larger range



