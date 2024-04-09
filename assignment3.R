library(car)
library(zoo)
library(tidyverse)
data=read.csv("DTA01.csv")
data=na.locf(data)
colnames(data)[1]="CENTER"
data$CENTER=as.factor(data$CENTER)
data$TRT=as.factor(data$TRT)
data$acr=as.factor(data$acr)

data11=read.csv("DTA11.csv")
data11=na.locf(data11)
colnames(data11)[1]="CENTER"
data11$CENTER=as.factor(data11$CENTER)
data11$TRT=as.factor(data11$TRT)
data11$acr=as.factor(data11$acr)
model=glm(acr~TRT+CENTER+PHYASMT+VAPS,family=binomial(link='logit'),data = data11)
summary(model)
anova(model)
pre=predict(model)
pre=exp(pre)/(1+exp(pre))
MSE = mean((data$acr - pre)^2)

data2=data|>
  group_by(PATIENT)|>
  filter(DAY==min(DAY))
data3=data|>
  group_by(PATIENT)|>
  filter(DAY==max(DAY))
DATA=left_join(data2,data3,by="PATIENT",keep=NULL)
model2=lm(PHYASMT.y~CENTER.x+TRT.x+PHYASMT.x,data = DATA)
summary(model2)
anova(model2)
