---
output:
  pdf_document: default
  html_document: default
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

---
title: "Sampling Populations"
author: 'NCSU: Nicolas Cardenas & Gustavo Machado'
subtitle: 'practical examples of sampling populations in veterinary medicine using R' 
date: "`r Sys.Date()`"
output:
  pdf_document: default
  html_document: default
---

## Set some packages and data in R 

```{r, echo=FALSE, warning=F, message=FALSE}
#load the packages 
if(!require(tidyverse)){install.packages("tidyverse")}; library(tidyverse)
if(!require(epiR)){install.packages("epiR")}; library(epiR)
if(!require(SDaA)){install.packages("SDaA")}; library(SDaA)
if(!require(reshape2)){install.packages("reshape2")}; library(reshape2)
if(!require(sampler)){install.packages("sampler")}; library(sampler)
if(!require(readr)){install.packages("readr")}; library(readr)
if(!require(plot3D)){install.packages("plot3D")}; library(plot3D)
# load the example datababse 
clinical_mastitis_cows <- read_csv("C:/Users/ncesped/Downloads/Clinical_Mastitis_cows_version2/clinical_mastitis_cows.csv")

```
# Simple random sampling

We will take a sample of **N** from a list of target invidious:
* I wanted to sample from a list of friend and calculate who will provide me with my daily cookie? 
My options are the following: Felipe, Jason, Abby, Gustavo and Kelsey.

> I can run one time random sample selectin once.
 
```{r}
# Options to get my cookie  
N <- c("Felipe",
       "Jason", 
       "Abby",
       "Gustavo",
       "Kelsey" )

# Calculate the sample 
sample(N,              # total of the population 
       1,              # one person 
       replace = F)    # sampling without replacement 
```
you get back luck, lets gonna simulate 1000 times 

```{r}
# simple random sample repeated 1000 times 
result <- sample(N,       # total of the population 
          1000,           # one person 
          replace = T)    # sample without replacement

# show a table with the results 
sort(table(result))

```

# Sample size

Here we will calculate the number of animals requited to estimate a prevalence of a disease in a specific population.
For this example the expected prevalence for this area is **15%**. And, our question is How many cattle need to be sampled and tested using the **95%** of confidence interval?  the total herds is a  population `N` of 1000 animals. 


```{r}
size <- rsampcalc(N=1000, e=3, ci=95, p=0.15)
print(size)

```

# Stratified random sampling
For this example we are going to use the `Albania` dataset containing 2017 Albania election that is previously installed in R. First, we are going to explore in detail the variable `qarku` that means county or location to see how many records are in each region.


```{r}
sort(table(albania$qarku))
```



```{r}
# Calulate the general sample size 
size <- rsampcalc(nrow(albania), e=3, ci=95, p=0.15)
# stratify the data by the variable qarku
stratifiedsample <- ssamp(albania, size, qarku)
sort(table(stratifiedsample$qarku))


```

# Relation among  population size adn prevalence 

In the next analysis we are going to simulate 1000 sampling designs. For this we are going to consider an initial prevalence of 1% and successively increase it to 100%, in this same way we are going to consider that populations from 10 animals to 100 animals. In the next plot the color reflects the size of the sample where warm colors represent larger sample sizes.


```{r}
#set the number of samples 
N <- 1000
myprevalence <- runif(n=N, min=0.01, max=1) # prevalence within 1 to 100%
mypopulation<- runif(n=N, min=10, max=1000) # population withn 10 to 1000

mysamplesize <- c()
for (i in 1:length(mypopulation)){
  aux <- rsampcalc(mypopulation[i],
                   e=3, ci=95,
                   p=myprevalence[i])
  mysamplesize <- rbind(mysamplesize, aux)
}
#plot the results 
mydata <- tibble(myprevalence, mypopulation, mysamplesize= as.numeric(mysamplesize))
scatter3D(mydata$myprevalence,
          mydata$mysamplesize, 
          mydata$mypopulation,
          bty = "g", pch = 18,
          lwd = 2, alpha = 0.5,
          expand =0.2,
          phi = 20,
          colvar = mysamplesize,
          ticktype = "detailed",
          type = "h",
          xlab = "Prevalence", ylab = "Sample size", zlab = "Population")

```

# Cluster sampling
An aid project has distributed cook stoves in a single province in a resource-poor country. At the end of three years, the donors would like to know what proportion of households are still using their donated stove. A cross-sectional study is planned where villages in a province will be sampled and all households (approximately `75` per village) will be visited to determine if the donated stove is still in use. A pilot study of the prevalence of stove usage in five villages showed that `0.46 `of householders were still using their stove and the intracluster correlation coefficient (ICC) for stove use within villages is in the order of `0.20`. If the donor wanted to be 95% confident that the survey estimate of stove usage was within` 10% `of the true population value, how many villages (clusters) need to be sampled?


```{r}
epi.ssclus1estb(b = 75,                 # the number of individual listing units in each cluster to be sampled
                Py = 0.46,              # an estimate of the unknown population proportion is this case 46%
                epsilon = 0.10,         # the maxi difference between the estimate and the unknown population value
                error = "relative",     # type of error to be used 
                rho = 0.20,             # the intra-cluster correlation
                conf.level = 0.95)$n.psu# IC95%
```


# Challenge 
Data for: Clinical Mastitis in Cows based on Udder Parameter using Internet of Things (IoT) from [this study](https://github.com/machado-lab/CBS-595-Special-topics-in-disease-epidemiology/blob/main/CBS_595_epidemiology/Exploratory%20data%20analysis/Exploratory%20data%20analysis_lecture.pdf), each represent one animal thus we have a population of n = `r nrow(clinical_mastitis_cows %>% filter(Day == max(Day)))`.

First prepare the data to be analyze, here we will consider the results at `Day = 6`, and we will stratify by the variable `Address`. Then, calculate the number of animals requited to estimate a prevalence of _mastitis_ a disease this population with a tolerable margin of error of `3`.For this exercise assume that your expected prevalence for this area **20%**. How many cattle need to be sampled and tested using interval confidence of **95%** ?

> the next code filter the data by tyhe day = 6 

```{r, warning=F, message=FALSE}
#prepare data for analysis
clinical_mastitis_cows <- clinical_mastitis_cows %>% # indicates the  database
  filter(Day == max(Day))                             # filter by 6 day 

```



# references 

[Sample Size Estimation in Veterinary Epidemiologic Research](https://www.frontiersin.org/articles/10.3389/fvets.2020.539573/full)
[Practical Issues in Calculating the Sample Size for Prevalence Studies](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.504.2129&rep=rep1&type=pdf)
