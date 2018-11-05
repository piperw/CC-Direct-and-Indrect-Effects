#################################################### 
# Script to analyze data using an SEM              #
# Created by Piper                                 #
# Created on 11/2/2017                             #
# Edited on 10/30/2018                             #
####################################################

# clear workspace
rm(list=ls())

# run detide and emersion calculations
source('Scripts/RegressionsET.R')

# load packages and functions
library("lavaan")
library('bbmle')
source("Functions/plot_jpeg.R")
source("Functions/calc.predict.eqns.r")
source("Functions/viz.miss.links.lv.R")
source("Functions/lavaan.graph.r")
source("Functions/lavaan.model.graph.R")

# create new dataframe and clean up
SEMdata <- merge(MaxData18, MinData, by="Site", all.y = TRUE)
colnames(SEMdata)
SEMdata[,4:25] <- NULL

names(SEMdata) <- c("Site", "Mean.max", "se.max", "Mean.min", "se.min", "Stars", "MeanStar", "se.meanstar", "StarDens", "MinStar", "MaxStar", "PA", "Mean", "P90", "MonAvg", "MMMax", "MDMax", "DRange", "MRange", "PC1", "PC2", "iMean", "i90P", "iMax", "iRange", "iPC1", "iPC2")

# Remove NAs
SEM10 <- SEMdata[-which(is.na(SEMdata$MeanStar)), ]
SEM10 <- SEM10[-which(SEM10$Site == "PP"), ]

# Check normality
max <- lm(Mean.max ~ PC1 + PC2, data = SEM10)
qqnorm(resid(max))
qqline(resid(max))
hist(resid(max))
anova(max)

min <- lm(Mean.min ~ PC1 + PC2, data = SEM10)
qqnorm(resid(min))
qqline(resid(min))
hist(resid(min))
shapiro.test(resid(min))
anova(min)

star <- lm(MeanStar ~ PC1 + PC2, data = SEM10)
qqnorm(resid(star))
qqline(resid(star))
hist(resid(star))
shapiro.test(resid(star))
anova(star)


# Direct Model
direct <- 'Mean.max ~ PC1 + PC2
Mean.min ~ PC1 + PC2
Mean.min ~ iMean
Mean.min ~ MeanStar
iMean ~~ PC1 + 0*PC2'




lavaan.model.graph(direct)

direct.est<-sem(direct, data=SEM10, fixed.x = FALSE)
varTable(direct.est)
summary(direct.est, fit.measures = TRUE, standardized = TRUE)

#look at results
lavaan.graph(direct.est, coeff=TRUE ,dpi=200)
#The Chi-square pvalue suggests that ourmodel does not fit well
#Let's use the model residuals to look for missing paths

parameterEstimates(direct.est, standardized=TRUE)
residuals(direct.est, type = "cor")$cor
modificationIndices(direct.est, sort.=TRUE, minimum.value=3)



#write lavaan model statement
indirect <- 'Mean.max ~ PC1 + PC2
Mean.min ~ c*(PC1)
Mean.min ~ c*(PC2)
MeanStar ~ a*iMean
Mean.min ~ b*MeanStar
iMean ~~ PC1 + 0*PC2
ab := a*b
total := c + (a*b)'



lavaan.model.graph(indirect)
ind.est<-sem(indirect, data=SEM10, fixed.x = FALSE)
varTable(ind.est)
summary(ind.est, fit.measures = TRUE, standardized = TRUE)
#look at results
lavaan.graph(ind.est,coeff=TRUE ,dpi=200)
#The Chi-square pvalue suggests that ourmodel does not fit well
#Let's use the model residuals to look for missing paths
parameterEstimates(ind.est, standardized=TRUE)
residuals(ind.est, type = "cor")$cor
modificationIndices(ind.est, sort.=TRUE, minimum.value=3)




MuMIn::AICc(direct.est, ind.est)
bbmle::AICctab(direct.est, ind.est)

