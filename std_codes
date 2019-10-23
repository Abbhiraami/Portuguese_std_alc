# Portuguese_std_alc
---
title: "Student Alcohol Consumption of Portuguese"
author: "Aarthi B and Abbhiraami S"
date: "5 January, 2019"
output:
  html_document: default
  pdf_document: default
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Introduction
Alcohol consumption is a widespread and expanding practice in the modern society.Excessive alcohol consumption is an increasing problem which has assumed major dimensions in many countries.
In the society, alcohol consumption is been considered as an issue among the students community but, the reason behind the habit are not taken care of. The main objective of this paper is to concentrate on factors that leads to alcohol consumption and how it affects the academic performance of students.
A common and comphrensive measure of school student is GRADES. Grades are important outcome because it is the key determinant of college admission decisions and  job quality for those who do not attend college. 

The report examines the effects of alcohol consumed by students of Portuguese and their quality of learning. The data consists of 395 cases with 33 variables and it is being used to analyse the drinking pattern among the students of Gabriel Pereira and Mousinho da Silveria  from the age of 15 to 18.This age range of 15 to 18 is considered as under age for alcohol consumption.

## Variables

The data has 33 variables consisting of demographic variables like age, sex and locality. In addition to that factors like school, Parent cohabitation, grades, access to internet, weekday alcohol consumption, weekend alcohol consumption, study time has also been absorbed .The significant variable of this data is weekday alcohol consumption and weekend alcohol consumption , on a scale of 1 to 5 ranging from very low to very high(Categorical-ordinal) and variables like Parent status, family size, sex, address and the name of the school are nominal(Binary).Whereas, age and grades are metric which can be used in case of mathematical computations.

## Methods
There were no direct observations conducted to collect this data. The data is taken from kaggle (https://www.kaggle.com/uciml/student-alcohol-consumption). 

## Summary Of The Dataset
```{r , echo=FALSE}
Alcohol_consumption=read.csv("C:/Users/jai/Documents/Internship/Portuguese Student Report/student-mat.csv",sep=",",header=T)
attach(Alcohol_consumption)
Alcohol_consumption$travel_time=as.factor(Alcohol_consumption$traveltime)
Alcohol_consumption$go_out=as.factor(Alcohol_consumption$goout)
Alcohol_consumption$free_time=as.factor(Alcohol_consumption$freetime)
Alcohol_consumption$fail=as.factor(Alcohol_consumption$failures)
Alcohol_consumption$daily_alcohol=as.factor(Dalc)
Alcohol_consumption$weekend_alcohol=as.factor(Walc)
Alcohol_consumption$study_time=as.factor(studytime)
Alcohol_consumption$fam_rel=as.factor(Alcohol_consumption$famrel)
Alcohol_consumption$hlth=as.factor(Alcohol_consumption$health)
g=cbind(G1,G2,G3)
Alcohol_consumption$grade=rowSums(g)
Alch=Alcohol_consumption[c(-7,-8,-13,-14,-15,-24,-25,-26,-27,-28,-29,-31,-32,-33)]

```
The central measures of the data are given below:
```{r , echo=F}
library(knitr)
devtools::install_github("lbusett/insert_table")
AlC=summary(Alcohol_consumption)
AlC

```
```{r,echo=FALSE}
library(ggplot2)

```


```{r ,echo=FALSE,message=FALSE}
library(ggplot2)
library(ggridges)
library(viridis)


```


```{r,echo=FALSE, message=F}
library(ggplot2)

```


The consumption of alcohol on a daily and weekend basis with respect to the grades of students are represented in quartile plot.






```{r, echo=FALSE,message=F}
library(ggplot2)
library(ggplot2)
library(ggridges)
library(viridis)
attach(Alch)
dat=data.frame(x=grade, y=daily_alcohol)
ggplot(dat,aes(x=grade,y=daily_alcohol,fill=factor(..quantile..)))+stat_density_ridges(geom = "density_ridges_gradient",calc_ecdf = TRUE,quantiles=4,quantile_lines = TRUE)+scale_fill_viridis(discrete=TRUE,name="Quartiles")+labs(title="Grade VS Daily alcohol consumption",subtitle = "From Portugese Data",y="Daily Alc consumption",x="Grade")


```

Weekend alcohol consumption:

```{r, echo=F,message=FALSE}
library(ggplot2)
library(ggplot2)
library(ggridges)
library(viridis)
attach(Alch)
dat=data.frame(x=grade, y=weekend_alcohol)
ggplot(dat,aes(x=grade,y=weekend_alcohol,fill=factor(..quantile..)))+stat_density_ridges(geom = "density_ridges_gradient",calc_ecdf = TRUE,quantiles=4,quantile_lines = TRUE)+scale_fill_viridis(discrete=TRUE,name="Quartiles")+labs(title="Grade VS Weekend alcohol consumption",subtitle = "From Portugese Data",y="Weekend Alc consumption",x="Grade")
```




From the above graphs, it is shown that on an average students who consume very high level of alcohol are tend to score below the median and it is not very clear to interpret,since there are bumps. Therefore, a model is built below to show what are all the factors that influence students to consume alcohol and how much the consumption of alcohol affects their academic grades.

##MULTINOMIAL LOGISTIC MODEL
From the data the consuption of alcohol on a daily and weekend basis has five levels. In order to build a model for a response variable more than two levels, we use multinomial logistic  model. 

The intention behind building this model is to know the relationship between the students and the addictiveness of consuming alcohol and the factors that will cause and affect their academics. The interpretations are made only for the higher degree of alcohol consumption (Levels 5).

1. Does parent cohabitation, internet usage, study time, travel time and grade has any effect on consuming alcohol during weekdays?  

Using the results obtained from the model, the above question can be answered.
```{r,echo=F,message=F,include=F}
library(nnet)
model=multinom(daily_alcohol~Pstatus+romantic+internet+study_time+grade+travel_time,data = Alcohol_consumption)
```
```{r,echo=F,message=F}
DA= summary(model)
DA 
pre_valDA=DA$coefficients/DA$standard.errors
probs_DA= (1- pnorm(abs(pre_valDA),0,1))*2
DA_c=exp(coefficients(model))
```
The equation of the model,

$\ln(\frac{P(Dalc=4)}{P(DAlc=1)}) =b_{30}+b_{31}(Pstatus=T)+b_{32}(romantic=yes)+b_{33}(internet=yes)+b_{34}(Pstatus=T)+b_{35}(study_time=2)+b_{36}(study_time=3)+b_{37}(study_time=4)+b_{38}(grade)$

$\ln(\frac{P(Dalc=5)}{P(DAlc=1)}) =b_{40}+b_{41}(Pstatus=T)+b_{42}(romantic=yes)+b_{43}(internet=yes)+b_{44}(Pstatus=T)+b_{45}(study_time=2)+b_{46}(study_time=3)+b_{47}(study_time=4)+b_{48}(grade)$

The log odds of being in high level versus in very low level alcohol consumption on a daily basis will decrease by 0.01711 if their parents are together rather than being apart.

The high intake of alcohol increases by log odds of 0.9628 when the student's relationship status move from being single to committed.

The log odds of being in high level alcohol consumption is increased by 31.3459 when there is a transition from no internet usage to internet usage.

The study time has a negative impact on the alcohol consumption because as the model states that the more time they spend on studies, less time they put themselves off from the habit of consuming alcohol. 

A one unit increase in variable grade is associated with the decrease in the log odds of being in high level versus very low level alcohol consumption. 

The time covered to reach school and back to home has an positive effect in the log odds of being in high versus very low level alcohol consumption. 


2. Does parent cohabitation, internet usage, study time, travel time and grade have any effect on consuming alcohol during weekends?

Using the results obtained from the model, the above question can be answered.

```{r,echo=F,message=F,include=F}
#Weekend Alcohol
library(nnet)
model1=multinom(weekend_alcohol~Pstatus+romantic+internet+study_time+grade+travel_time,data = Alcohol_consumption)
```
```{r,echo=F,message=F}
WA=summary(model1)
WA
pre_valWA=WA$coefficients/WA$standard.errors
probs_WA= (1- pnorm(abs(pre_valWA),0,1))*2
WA_C= exp(coefficients(model1))
```


The log odds of being in high level versus in very low level alcohol consumption on weekend basis will decrease by 0.0978 if their parents are together rather than being apart.

The high intake of alcohol increases by log odds of 0.0969 when the student's relationship status move from being single to committed. 

The log odds of being in high level alcohol consumption is decrease by 0.03610508  when there is a transition from no internet usage to internet usage.

The amount of time they invest on studies reduces their rate of alcohol consumption. 

A unit increase in grade of the academic associated with the decrease in level of alcohol consumption by 1.6649.(Poor Performance in their academics increases the log odds of higher alcohol consumption)

## Conclusion

Though a number of investigations have studied the associations between alcohol consumption and years of schooling. Only few factors are known about the impact of adolescent drinking in the present time and quality of learning for those who remain in school.

The analysis proves that the alcohol consumption either on a regular basis or weekends has a negative impact on academic performances of the students. Here, we also analysed their habit of consuming alcohol depends on the factors like parent cohabitation, study time, travel time,internet usage and romantic relationships. So,this shows that, the combined effect of all these factors affects the academic performaces of a student. 

## References
https://www.kaggle.com/uciml/student-alcohol-consumption


https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3026599


https://www.niaaa.nih.gov/alcohol-health/overview-alcohol-consumption


https://www.jstage.jst.go.jp/article/kenkokyoiku/19/2/19_135/_pdf


https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
