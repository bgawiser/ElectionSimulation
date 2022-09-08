library(tidyverse)
library(tidytext)
library(stringr)
library(caret)
library(tm)
library(stats)
library(dplyr)
library(sqldf)


ongoingDSeats <- 35
ongoingRSeats <- 30

setwd("~/Personal/Election")

e_data <- read.csv("Election.csv")
s_data <- read.csv("Senate.csv")

#e_data$Trump <- e_data$Trump + 5

e_data$SDEV <- e_data$MOE/1.96
e_data$Diff <- e_data$Biden - e_data$Trump
e_data$zScore <- e_data$Diff/(2*e_data$SDEV)
e_data$Prob <- pnorm(e_data$zScore)
e_data$ExpectedEV <- e_data$Prob*e_data$EVs
e_data$EVSTDEV <- sqrt(e_data$Prob*(1-e_data$Prob))*e_data$EVs

s_data$SDEV <- s_data$MOE/1.96
s_data$Diff <- s_data$Dem - s_data$Rep
s_data$zScore <- s_data$Diff/(2*s_data$SDEV)
s_data$Prob <- pnorm(s_data$zScore)


BidenVictories <- 0
TrumpVictories <- 0
Ties <- 0
vals <- data.frame(BidenEVs=as.integer())

DemControls <- 0
RepControls <- 0
SenateTies <- 0
SenateVals <- data.frame(DemSeats=as.integer())

for(i in 1:100000)
{
  
  e_data$CurSDs <- qnorm(runif(dim(e_data)[1],0,1))
  curBidenEVs <- sum(((e_data$Biden+e_data$CurSDs*e_data$SDEV*2) > e_data$Trump) * e_data$EVs)
  curBidenMargin <- sum(((e_data$Biden+e_data$CurSDs*e_data$SDEV*2) - e_data$Trump) * e_data$Votes/100)
  
  s_data$CurSDs <- qnorm(runif(dim(s_data)[1],0,1))
  curDemSeats <- sum((s_data$Dem+s_data$CurSDs*s_data$SDEV*2) > s_data$Rep) + ongoingDSeats
  
  
  vals <- rbind(vals, c(curBidenEVs, curBidenMargin))
  SenateVals <- rbind(SenateVals, c(curDemSeats, 100 - curDemSeats))
  
  if(curBidenEVs >= 270)
  {
    BidenVictories <- BidenVictories + 1
  } else if (curBidenEVs < 269)
  {
    TrumpVictories <- TrumpVictories + 1
  } else
  {
    Ties <- Ties + 1
  }
  
  if(curDemSeats > 50)
  {
    DemControls <- DemControls + 1
  } else if (curDemSeats < 50)
  {
    RepControls <- RepControls + 1
  } else
  {
    SenateTies <- SenateTies + 1
  }
}
names(vals)[1]<-"EVs"
names(vals)[2]<-"Margins"

names(SenateVals)[1]<-"DemSeats"
names(SenateVals)[2]<-"RepSeats"

BidenEV <- round(sum(e_data$ExpectedEV))
TrumpEV <- 538 - BidenEV
EVSD <- sqrt(sum(e_data$EVSTDEV*e_data$EVSTDEV))

ggplot(data=vals, aes(EVs)) + geom_histogram(aes(y=..density..), binwidth=5, color="black", fill="white") + 
  geom_vline(aes(xintercept=mean(EVs)), color="blue", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=269), color="orange", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=median(EVs)), color="green", linetype="dashed", size=1) + 
    geom_density(alpha=0.2, fill="#FF6666")


evcounts <- vals %>% group_by(EVs) %>% count()
SenateCounts <- SenateVals %>% group_by(DemSeats) %>% count()

BidenEV
TrumpEV
pnorm(((BidenEV-TrumpEV)/2)/EVSD)
round(mean(vals$EVs))
mean(vals$Margins)
evcounts[evcounts$n==max(evcounts$n),-2]
sqldf("select sum(n) from evcounts where EVs>=320 and EVs<330")

DemControls
RepControls
SenateTies

#ggplot(data=SenateVals, aes(DemSeats)) +  geom_histogram(aes(y=..density..),binwidth=1, color="black", fill="white") + 
#  geom_vline(aes(xintercept=50), color="orange", linetype="dashed", size=1) 

