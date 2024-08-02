library(survey)
library(data.table)
library(dplyr)

#input all valid page numbers to sample
pages<- c(12:27,30:41,44:55,58:91,94:101,104:115,118:145,148:159,
          162:169,172:191,194:217,220:251,254:277,280:301,304:323)
str(pages)
N <- 284
n <- 20
fpc = sqrt((N-n)/(N-1))


#####                              PART A                                 #####

#randomly select 20 of the valid pages
pages_samp <- sample(pages, replace=F, size=n)
pages_samp = data.frame(c(111,79,156,159,98,250,31,125,86,207,
                          113,248,114,231,151,189,122,71,254,83))

#collect data
dataA <- data.frame(page=c(111,79,156,159,98,250,31,125,86,207,
                           113,248,114,231,151,189,122,71,254,83),
                    x=c(17,24,19,17,28,14,32,28,17,47,
                        36,14,15,24,29,19,27,31,31,35),
                    y=c(5,9,13,6,8,5,13,6,4,10,16,0,7,3,12,6,6,13,10,8),
                    v=c(3,4,1,6,3,1,7,1,3,6,3,2,3,0,5,5,3,5,8,7));dataA

#add variable for sampling weights, add population size
dataA <- dataA %>% mutate(weights=(N/n)) %>% mutate(N=N)
dataA


#create design object
designA<- svydesign(id=~page, data=dataA, fpc=~N)

#find population total x with its SE and 95% CI
sum(dataA$x) # total sample x
var(dataA$x) # sample variance of x
sqrt(((N-1)/N)*var(dataA$x)) #find estimate for sigma of x

svytotal(~x,designA)
SE(svytotal(~x,designA))
confint(svytotal(~x,designA), level=0.95)

#find population total y with its SE and 95% CI
sum(dataA$y) # total sample y
var(dataA$y) # sample variance of y
sqrt(((N-1)/N)*var(dataA$y)) #find estimate for sigma of y

svytotal(~y,designA)
SE(svytotal(~y,designA))
confint(svytotal(~y,designA), level=0.95)


#find estimate for population proportion of words that I know
p_y <- sum(dataA$y)/sum(dataA$x)
p_y
#does this p_y estimate total population y?
p_y * 7156.8

# Find SE for p_y and its confidence interval
SE_py <- sqrt((p_y*(1-p_y))/504)*fpc
SE_py
CI_L = p_y - (1.96*SE_py)
CI_U = p_y + (1.96*SE_py)
print(paste('95% CI for p_y:(',CI_L,',',CI_U,')'))

#collect number of words starting with vowels
dataA <- dataA %>% mutate(v=c(3,3,0,6,3,1,5,1,3,2,0,2,3,0,3,5,3,5,8,7))

#find population total v with its SE and 95% CI
sum(dataA$v) # total sample v
var(dataA$v) # sample variance of v
sqrt(((N-1)/N)*var(dataA$v)) #find estimate for sigma of v

svytotal(~v,designA) # V'
SE(svytotal(~v,designA)) # SE(V')
confint(svytotal(~v,designA), level=0.95) # 95% CI for V'


#####                              PART B                                 #####

#randomly select 20 of the valid pages
pages_samp <- sample(pages, replace=F, size=n)
pages_samp2 = data.frame(c(69,289,32,81,304,129,275,133,166,137,
                          130,61,224,194,75,154,286,58,264,234))


#collect data 1 = vowel; 2 = s,t,m,n,b,d; 3 = x,y,z,q; 4 = all else
N <- 284
n <- 20

x1=c(3,6,4,3,2,6,5,5,4,4,3,6,1,6,3,1,6,7,3,3)
y1=c(0,3,3,0,1,1,1,2,2,0,1,1,1,2,1,0,5,1,0,2)

x2=c(13,6,6,8,12,15,10,5,8,13,8,13,4,12,11,4,11,7,5,9)
y2=c(2,1,1,1,9,7,4,2,1,3,2,6,2,5,4,3,7,2,3,6)

x3=c(0,1,1,0,0,0,0,0,0,2,1,1,1,0,0,0,0,0,0,0)
y3=c(0,1,1,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,0)

x4=c(17,8,14,11,13,17,10,13,9,17,6,15,18,14,20,19,13,23,3,14)
y4=c(3,0,8,0,13,8,3,5,3,9,2,1,6,4,4,13,10,17,2,3)

dataB <- data.frame(page=c(69,289,32,81,304,129,275,133,166,137,
                           130,61,224,194,75,154,286,58,264,234),
                    strata=c(rep(1,times=20),
                             rep(2,times=20),
                             rep(3,times=20),
                             rep(4,times=20)),
                    x=c(x1,x2,x3,x4),
                    y=c(y1,y2,y3,y4))

aggregate(x ~ page, dataB, sum)
aggregate(y ~ page, dataB, sum)

aggregate(x ~ strata, dataB, mean)
aggregate(y ~ strata, dataB, mean)
aggregate(x ~ strata, dataB, sum)

aggregate(x ~ strata, dataB, sd)
aggregate(y ~ strata, dataB, sd)

#add variable for sampling weights, add population size

dataB <- dataB %>%
  mutate(weights=N/n) %>%
  mutate(N=N) %>%
  mutate(p_y = y/x) %>%
  mutate(p_yh = sum(p_y)/n)

#create design object
designB<- svydesign(id=~page, strata=~strata, nest=T, weights=~weights, 
                    data=dataB, fpc=~N)

#find population total x with its SE and 95% CI
svytotal(~x,designB)
svyby(~x, ~strata, svytotal, design=designB)
SE(svytotal(~x,designB))

confint(svytotal(~x,designB), level=0.95)
confint(svyby(~x, ~strata, svytotal, design=designB), level=0.95)

#find population total y with its SE and 95% CI
svytotal(~y,designB)
svyby(~y, ~strata, svytotal, design=designB)
SE(svytotal(~y,designB))

confint(svytotal(~y,designB), level=0.95)
confint(svyby(~y, ~strata, svytotal, design=designB), level=0.95)


#find estimate for population proportion of words that I know
p_y <- sum(dataB$y)/sum(dataB$x)
p_y
# Calculate proportions
sum_y_by_stratum <- svyby(~y, ~strata, svytotal, design = designB)
sum_x_by_stratum <- svyby(~x, ~strata, svytotal, design = designB)
sum_x <- svytotal(~x,designB)
p_yh_by_stratum <- sum_y_by_stratum / sum_x_by_stratum
p_yh_by_stratum

p_str <- sum(sum_x_by_stratum$x*p_yh_by_stratum$y)/7696.4
p_str #same as calc above

#does this p_y estimate total population y?
p_y * 7696.4

# Find SE for p_y and its confidence interval
# Create the contingency table
table_xy <- svytable(~x + y + strata, design = designB)
table_xy


SE_py <- sqrt((p_y*(1-p_y))/sum(dataB$x))*fpc
SE_py
CI_L = p_y - (1.96*SE_py)
CI_U = p_y + (1.96*SE_py)
print(paste('95% CI for p_y:(',CI_L,',',CI_U,')'))

# find population total v with its SE and 95% CI
## strata 1 = words starting with vowels
svyby(~x, ~strata, svytotal, design=designB) # strata 1
SE(svyby(~x, ~strata, svytotal, design=designB)) # strata 1
confint(svyby(~x, ~strata, svytotal, design=designB), level=0.95)
