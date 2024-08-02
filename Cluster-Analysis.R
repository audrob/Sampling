library(dplyr)
library(survey);library(sampling)
  
###                                 9.4                                     ###
# input data
df1 <- data.frame(Hospital = c(1:10),
                  Patients = c(560,190,260,370,190,130,170,170,60,110),
                  Dead     = c(4,4,2,4,4,0,9,2,0,1),
                  Beds     = c(824,312,329,648,358,252,256,263,138,150))

# Population Clusters (33 Hospitals in the population)
M <- 33

# Inflate Patients and Dead
df1i <- df1 %>%
  mutate(wt = M/10) %>% 
  mutate(M = M) %>%
  mutate(Patients_infl = Patients*7217) %>%
  mutate(Dead_infl = Dead*7217)

# set survey design (1-Stage Cluster)
design1 <- svydesign(id=~Hospital,
                     weights=~wt,
                     fpc=~M,
                     data=df1i)

# part a) calculate total patients and 95% CI using ratio estimation
svyratio(numerator=~Patients_infl,
         denominator=~Beds,
         fpc=~M,
         design=design1)
# 95% CI
confint(svyratio(numerator=~Patients_infl,
                 denominator=~Beds,
                 fpc=~M,
                 design=design1),
        level = 0.95, df=9)

# part b) calculate total dead and 95% CI using ratio estimation
svyratio(numerator=~Dead_infl,
         denominator=~Beds,
         fpc=~M,
         design=design1)
# 95% CI
confint(svyratio(numerator=~Dead_infl,
                 denominator=~Beds,
                 fpc=~M,
                 design=design1),
        level = 0.95, df=9)

###                                 10.4                                    ###
# input data for selected clusted and listing units
df2 <- data.frame(Week = rep(c(2,6,8,10),each=3),
                  Visitors = c(105,111,111,
                               250,209,180,
                               315,302,395,
                               300,206,200),
                  Injuries = c(0,0,0,
                               2,0,1,
                               3,2,4,
                               0,2,0))

# create weights
df2 <- df2 %>%
  mutate(wt=(1/((4/10)*(3/6))))


# set survey design (2-Stage Cluster)
design2 <- svydesign(id=~Week,
                     weights=~wt,
                     data=df2)

# part a) estimate total visitors and its 95% CI

svytotal(~Visitors,design=design2)

#95% CI
confint(svytotal(~Visitors,design=design2),
        level=0.95,
        df=3) # m-1 = 3

# part b) estimate total injuries and its 95% CI

svytotal(~Injuries,design=design2)

#95% CI
confint(svytotal(~Injuries,design=design2),
        level=0.95,
        df=3) # m-1 = 3

# part c) estimate total injuries and visitors per week 
  # and their 95% CI
df2 <- df2 %>%
  mutate(Weekly_Visitors = Visitors/10) %>%
  mutate(Weekly_Injuries = Injuries/10)

# set survey design (2-Stage Cluster)
design2 <- svydesign(id=~Week,
                     weights=~wt,
                     data=df2)

# Total Weekly Visitors
svytotal(~Weekly_Visitors,design=design2)

#95% CI
confint(svytotal(~Weekly_Visitors,design=design2),
        level=0.95,
        df=3) # m-1 = 3

# Total Weekly Injuries
svytotal(~Weekly_Injuries,design=design2)

#95% CI
confint(svytotal(~Weekly_Injuries,design=design2),
        level=0.95,
        df=3) # m-1 = 3

# part d) estimate total injuries and visitors per day
# and their 95% CI
df2 <- df2 %>%
  mutate(Daily_Visitors = Visitors/60) %>%
  mutate(Daily_Injuries = Injuries/60)

# set survey design (2-Stage Cluster)
design2 <- svydesign(id=~Week,
                     weights=~wt,
                     data=df2)

# Total Daily Visitors
svytotal(~Daily_Visitors,design=design2)

#95% CI
confint(svytotal(~Daily_Visitors,design=design2),
        level=0.95,
        df=3) # m-1 = 3

# Total Daily Injuries
svytotal(~Daily_Injuries,design=design2)

#95% CI
confint(svytotal(~Daily_Injuries,design=design2),
        level=0.95,
        df=3) # m-1 = 3

# part e) calculate total injuries per visitor and 95% CI using ratio estimation
svyratio(numerator=~Injuries,
         denominator=~Visitors,
         design=design2)
# 95% CI
confint(svyratio(numerator=~Injuries,
                 denominator=~Visitors,
                 design=design2),
        level = 0.95, df=3) #m-1 = 3


###                                 10.6                                    ###
#Calculate ICC for number of visitors
df3 <- data.frame(Week = rep(c(1:10),times=6),
                  Visitors = c(200,120,310,200,170,250,380,495,206,308,
                               150,105,200,107,160,237,378,400,200,300,
                               130,111,180,101,130,209,325,315,108,293,
                               140,103,130,98 ,121,212,330,302,95 ,206,
                               150,111,125,103,107,231,306,350,107,200,
                               190,130,208,137,114,180,331,395,190,300))
df3$Week <- as.factor(df3$Week)

# total variance
total_var <- (((sd(df3$Visitors)^2) *59) /60)


# Find between variance
Week_Totals <- df3 %>%
  group_by(Week) %>%
  summarise(Total_Wk_Visitors = sum(Visitors))

Week_Totals 

between_var <- (((sd(Week_Totals$Total_Wk_Visitors)^2)*9)/10)

#Calculate ICC 
ICC <- ((1/6)*between_var - total_var) / (5*total_var)
ICC
