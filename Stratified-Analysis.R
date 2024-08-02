library(dplyr)
library(survey);library(sampling)

# input table 3.8 data
worker <- c(1:40)
exposure <- as.character(c(3,3,2,2,3,1,1,1,3,3,1,3,2,3,3,3,3,3,3,2,
                           2,1,3,2,3,3,3,3,3,1,3,3,3,3,3,3,3,3,3,3))
fvc <- as.numeric(c(81,64,85,91,60,97,82,99,96,91,71,88,84,
                    85,77,76,62,67,91,99,70,64,72,72,95,96,
                    62,67,95,87,84,89,89,65,67,69,80,98,65,84))
N <- 1200
n <- 40

data1 <- data.frame(worker,exposure,fvc)
data1$exposure <- factor(data1$exposure, levels=c("1","2","3"));data1

data1 <- data1 %>% 
  mutate(N_h= case_when(exposure==1 ~ 180,
                        exposure==2 ~ 180,
                        exposure==3 ~ 840)) %>%
  mutate(n_h= case_when(exposure==1 ~ 6,
                        exposure==2 ~ 6,
                        exposure==3 ~ 28)) %>%
  mutate(wt= N_h/n_h) %>%
  mutate(wt_h=N_h/n_h) %>%
  mutate(N=N)
data1



# 6.2

# set survey design (Stratified)
design1 <- svydesign(id=~worker,
                     strata=~exposure,
                     weights=~wt_h,
                     data=data1,
                     fpc=~N_h)

# part b) find population mean estimate (str)
svymean(~data1$fvc,
        design=design1,
        df=~degf(design1),
        fpc=~N_h)

# part c) find 95% CI for above value
## Stratified Random Sampling
confint(svymean(~fvc,design=design1),
        df=degf(design1),
        fpc=~N_h,
        level=0.95)

## Simple Random Sampling
design2 <- svydesign(id=~worker, 
                     strata=~exposure, 
                     weights=~wt, 
                     data=data1, 
                     fpc=~N)

svymean(~fvc,design=design2)
confint(svymean(~fvc,design=design2),
        df=degf(design2),
        fpc=~N_h,
        level=0.95)



# 6.4

## Set Survey Design: Simple Random Sampling
design2 <- svydesign(id=~worker, 
                     strata=~exposure, 
                     weights=~wt, 
                     data=data1, 
                     fpc=~N)

# part a) compute 90% CI for population mean fvc
svymean(~data1$fvc,
        design=design2,
        df=degf(design2),
        fpc=~N,
        level=.9)

confint(svymean(~fvc,design=design2),
        df=degf(design2), level=0.9)
data1


# part b) Post stratify
## find means and SD for the stratum
aggregate(fvc~exposure, data=data1, FUN=function(x) c(Mean=mean(x), SD=sd(x)))

## create data frame from calculations
psmean <- data.frame(exposure=c(1,2,3),
                     mean=c(83.33333,83.5,79.10714),
                     sd=c(13.94230,11.07700,12.56196))
psmean$exposure <- factor(psmean$exposure, levels=c("1","2","3"))

## given pop size
popsize <- data.frame(exposure=c(1,2,3), pop= c(1000,100,100))
popsize$exposure <- factor(popsize$exposure, levels=c("1","2","3"))

dataps <- data1 %>% 
  select(exposure,N,n_h) %>% 
  distinct(exposure,n_h) %>% 
  full_join(psmean, join_by(exposure))%>%
  full_join(popsize, join_by(exposure))%>%
  mutate(var_h=sd^2)

## calculate mean for post stratification
mean_pstr= sum((dataps$pop/N)*dataps$mean)
mean_pstr

## calculate var for post stratification
var_pstr=((N-n)/(n*N))*
  (sum((dataps$pop/N)*dataps$var_h)) + 
  (1/n^2)*(sum(dataps$var_h*((N-dataps$n_h)/N)))
var_pstr=((N-n)/(n*N))*
  (sum((dataps$pop/N)*dataps$var_h)) + 
  (1/n^2)*(sum(dataps$var_h*((N-dataps$pop)/N)))
var_pstr

## estimate se with sd (square root of variance)
SE_pstr <- sqrt(var_pstr)
SE_pstr

## using t-table, t_crit= 1.645 for .95 2-tail prob
CI_ps_L <- mean_pstr - (1.645*SE_pstr)
CI_ps_U <- mean_pstr + (1.645*SE_pstr)

print(paste("The 90% CI for Population Poststratification Mean is:(",CI_ps_L,",",CI_ps_U,")"))



# set survey design (SRS)
design3 <- svydesign(id=~worker,
                     weights=~wt, 
                     fpc=~N,
                     data=data1)

popf <- data.frame(exposure=c("3","2","1"), 
                   Freq=c(100,100,1000))
poststrat<- postStratify(design3,~exposure,popf)
summary(poststrat)
poststrat
svymean(~data1$fvc,poststrat,df=degf(poststrat))
svyby(~fvc,~exposure,design3,svymean)

SE(svymean(~data1$fvc, poststrat, df=degf(poststrat)), svymean, level=.90)
confint(svymean(~data1$fvc,poststrat), level=0.90)
confint(svymean(~data1$fvc,poststrat), "fvc", level=0.90)

