
# Import data file
mydata <- read.csv("path\\cbecs2018.csv")

# Import necessary libs
library(survey)
library(spatstat)
library(dplyr)
library(ggplot2)

# Set up final weight and replicate weights for sample
wts <- mydata$FINALWT
rep_wts <- mydata[, grepl("^FINALWT", names(mydata))]
rep_wts$FINALWT <- NULL


## without replicate weights
design <- svydesign(ids=~PUBID, weights=wts, mse=TRUE, data=mydata)


###                       C: Histogram & Summary                         ###

# Histogram 

## histogram for sqft
svyhist(~SQFT, design, breaks=100, 
        main="Histogram of Weighted Building Square Footage",
        col="grey")


## histogram for ELCNS
svyhist(~ELCNS, design, breaks=100, 
        main="Histogram of Weighted Building Energy Consumption",
        col="grey")

# Summary Stats

#SQFT
svymean(~SQFT, design)
svyvar(~SQFT, design)

confint(svymean(~SQFT, design))

#ELCNS
svymean(~ELCNS, design, na.rm=T)
svyvar(~ELCNS, design, na.rm=T)
confint(svymean(~ELCNS, design, na.rm=T))


###                        F: Variance estimates                      ###
# Survey design object
designr <- svrepdesign(weights=wts, repweights=rep_wts,
                      type="JK2", mse=TRUE, data=mydata)
 
svymean(~SQFT,designr)
svymean(~SQFT, designr, return.replicates=T)

designr <- svrepdesign(weights=wts, repweights=rep_wts,
                       type="JK2", mse=TRUE, data=mydata)
svymean(~ELCNS, designr, na.rm=T, return.replicates=T)
svytotal(~ELCNS, designr, na.rm=T, return.replicates=T)

designr <- as.svrepdesign(
  design, type="JK1")


designr <- as.svrepdesign(
  design, type="JK1", scale=1, rscales=rep(1,150)
  )

svymean(~SQFT,designr)
svymean(~SQFT, designr, return.replicates=T)

svyby(~SQFT, ~REGION, designr, svymean, return.replicates=T)


svymean(~ELCNS, designr, na.rm=T)
svymean(~ELCNS, designr, na.rm=T, return.replicates)

