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

model_PAINJT=lm(PAINJT.y~TRT.x+CENTER.x+PAINJT.x,data = DATA)
summary(model_PAINJT)
anova(model_PAINJT)

model_SWELLJT=lm(SWELLJT.y~TRT.x+CENTER.x+SWELLJT.x,data = DATA)
summary(model_SWELLJT)
anova(model_SWELLJT)

model_VAPS=lm(VAPS.y~TRT.x+CENTER.x+VAPS.x,data = DATA)
summary(model_VAPS)
anova(model_VAPS)

model_CRP=lm(CRP.y~TRT.x+CENTER.x+CRP.x,data = DATA)
summary(model_CRP)
anova(model_CRP)

model_HAQ=lm(HAQ.y~TRT.x+CENTER.x+HAQ.x,data = DATA)
summary(model_HAQ)
anova(model_HAQ)

model_PAINJT2=lm(PAINJT.y~TRT.x+CENTER.x+PAINJT.x+TRT.x:CENTER.x+TRT.x:PAINJT.x,data = DATA)
summary(model_PAINJT2)
anova(model_PAINJT2)

model_SWELLJT2=lm(SWELLJT.y~TRT.x+CENTER.x+SWELLJT.x+TRT.x:CENTER.x+TRT.x:SWELLJT.x,data = DATA)
summary(model_SWELLJT2)
anova(model_SWELLJT2)

model_VAPS2=lm(VAPS.y~TRT.x+CENTER.x+VAPS.x+TRT.x:CENTER.x+TRT.x:VAPS.x,data = DATA)
summary(model_VAPS2)
anova(model_VAPS2)

model_CRP2=lm(CRP.y~TRT.x+CENTER.x+CRP.x+TRT.x:CENTER.x+TRT.x:CRP.x,data = DATA)
summary(model_CRP2)
anova(model_CRP2)

model_HAQ2=lm(HAQ.y~TRT.x+CENTER.x+HAQ.x+TRT.x:CENTER.x+TRT.x:HAQ.x,data = DATA)
summary(model_HAQ2)
anova(model_HAQ2)

anova(model_PAINJT,model_PAINJT2)
anova(model_SWELLJT,model_SWELLJT2)
anova(model_VAPS,model_VAPS2)
anova(model_CRP,model_CRP2)
anova(model_HAQ,model_HAQ2)

