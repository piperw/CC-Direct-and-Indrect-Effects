#################################################### 
# Script to analyze data                           #
# Created by Piper                                 #
# Edited on 5/24/2017                              #
# Commented out 10/9/2018                          #
####################################################

# clear workspace
rm(list=ls())

# run detide and emersion calculations
source('Scripts/Emersion.R')

# load libraries
library(nlme)
library(lme4)
library(boot)
library(car)
library(MuMIn)
library(lmerTest)
library(sjPlot)
library(sjmisc)
library(effects)
library(multcomp)
library(lattice)
library(betareg)
library(ggplot2)
library(ggbiplot)
library(FactoMineR)
library(factoextra)
library(ggthemes)

# Temp metrics and latitude----
colnames(SiteMetrics)
paramsy<-colnames(SiteMetrics)[c(14:15,17:18,20,22:23)]


# plot temperature by lat
png(paste('Plots/TempxLat.png'), width = 800, height = 800)
par(mfrow = c(3,3))
for (i in 1:length(paramsy)){ #loop over different temp metrics
  
  #model
  model <- lm(SiteMetrics[,paramsy[i]] ~ SiteMetrics$Latitude, data = SiteMetrics)
  
  plot(SiteMetrics[,paramsy[i]] ~ SiteMetrics$Latitude, data = SiteMetrics, main = paramsy[i], xlab = 'Latitude', ylab = 'Degrees C', cex = 1.5, cex.lab = 1.5)
  
  #CI lines
  if(anova(model)$'Pr(>F)' [1] <= 0.05){
    new.Y<-predict(model, se = TRUE, interval = 'confidence') 
    #upper and lower CI
    x<-SiteMetrics$Latitude
    x.ind<-order(x)
    lines(x[x.ind], new.Y$fit[x.ind,1])
    lines(x[x.ind], new.Y$fit[x.ind,2], col = 'blue', lty = 2)
    lines(x[x.ind], new.Y$fit[x.ind,3], col = 'blue', lty = 2)
    R2 <- summary(model)$'r.squared'
    legend('topright',legend= paste('R2 = ',round(R2,2)), bty='n')
  }
}
dev.off()


# PCAs----
# scale the data and run PCA

PCA <- prcomp(SiteMetrics[c(14:15,17:18,20,22:23)], scale = TRUE, center = TRUE)

# flip axis of PC2 so its more intuitive
PCA$x[,2]<- (-PCA$x[,2])

# Proportion of data explained by each axis
summary(PCA)

# plot
png('Plots/PCA.png')
fviz_pca_biplot(PCA, xlab = "PC1 (86.7%)", ylab = "PC2 (11.8%)", repel = TRUE, col.var = "black", col.ind = "gray", title = " ", label = "var") + theme_base()
dev.off()


# individual point scores 
PCAFrame<-data.frame(PCA$x[,1:2],SiteMetrics$Site) # we want to make sure the sites line up correctly
colnames(PCAFrame)[3]<-'Site'
SiteMetrics <- merge(SiteMetrics,PCAFrame)



# Summarize mussel and pisaster data----
MusselMin <- VerticalMin[which(VerticalMin$Species == "Mytilus"),]
MusselMax <- VerticalMax[which(VerticalMax$Species == "Mytilus"),]
Pisaster <- subset(Stars, Species == "Pisaster")

#Sea star summary data
StarData <- ddply(Pisaster, c("Site"), summarize,
                   Stars = sum(!is.na(ET)),
                   MeanStar = mean(ET),
                   se.mean = sd(ET)/sqrt(840),
                   StarDens = Stars/150,
                   MinStar = min(ET),
                   MaxStar = max(ET))
                  

#Summary for mussels
SumMax <- ddply(MusselMax, c("Site"), summarize,
                   Mean.max = mean(ET, na.rm = TRUE), #average max
                   se.max = sd(ET, na.rm = TRUE)/sqrt(10))

SumMin <- ddply(MusselMin, c("Site"), summarize,
                   Mean.min = mean(ET, na.rm = TRUE), #average min
                   se.min = sd(ET, na.rm = TRUE)/sqrt(10))



# Merge temp metrics with summary data
MaxData<- merge(SumMax, StarData, by="Site", all.x = TRUE)
MaxData$StarDens[is.na(MaxData$StarDens)] <- 0
MaxData$Stars[is.na(MaxData$Stars)] <- 0
MaxData$PA <- ifelse(MaxData$StarDens > 0, 1, 0)
MaxData <- merge(MaxData, SiteMetrics, by="Site", all.x = TRUE)


MinData<- merge(SumMin, StarData, by="Site", all.x = TRUE)
MinData$StarDens[is.na(MinData$StarDens)] <- 0
MinData$Stars[is.na(MinData$Stars)] <- 0
MinData$PA <- ifelse(MinData$StarDens > 0, 1, 0)
MinData <- merge(MinData, SiteMetrics, by="Site", all.x = TRUE)



# check data
colnames(MaxData)
MaxData[,c(11:22,25,28,30)] <- NULL
colnames(MaxData)

colnames(MinData)
MinData[,c(11:22,25,28,30)] <- NULL
colnames(MinData)


# Remove extras
rm(SiteMetrics, PCA, StarData, MusselMax, MusselMin, VerticalMax, VerticalMin, Stars, SumMax, SumMin, i, model, new.Y, paramsy, R2, x, x.ind, Pisaster, PCAFrame)


# Temp x Variables plots----
paramsx<-colnames(MaxData)[c(11:19)]

png(paste('Plots/MaxMetrics.png'), width = 1200, height = 1200)
par(mfrow = c(3,3))
for (i in 1:length(paramsx)){ #loop over different temp metrics
  
  #model
  model <- lm(MaxData$Mean.max ~ MaxData[,paramsx[i]])
  
  plot(MaxData$Mean.max ~ MaxData[,paramsx[i]], data = MaxData, main = paramsx[i], xlab = 'Degrees C', ylab = 'Mean Max Exposure Time (%)', cex = 1.5, cex.lab = 1.5)
  
  #CI lines
  if(anova(model)$'Pr(>F)' [1] <= 0.05){
    new.Y<-predict(model, se = TRUE, interval = 'confidence') 
    
    #upper and lower CI
    x <- MaxData[,paramsx[i]]
    x.ind<-order(x)
    lines(x[x.ind], new.Y$fit[x.ind,1])
    lines(x[x.ind], new.Y$fit[x.ind,2], col = 'gray', lty = 2)
    lines(x[x.ind], new.Y$fit[x.ind,3], col = 'gray', lty = 2)
    R2 <- summary(model)$'r.squared'
    legend('topright',legend= paste('R2 = ',round(R2,2)), bty='n')
  }
}
dev.off()


# Plot  min

paramsx<-colnames(MinData)[c(11:19)]

png(paste('Plots/MinMetrics.png'), width = 1200, height = 1200)
par(mfrow = c(3,3))
for (i in 1:length(paramsx)){ #loop over different temp metrics
  
  #model
  model <- lm(MinData$Mean.min ~ MinData[,paramsx[i]])
  
  plot(MinData$Mean.min ~ MinData[,paramsx[i]], data = MinData, main = paramsx[i], xlab = 'Degrees C', ylab = 'Mean Min Exposure Time (%)', cex = 1.5, cex.lab = 1.5)
  
  #CI lines
  if(anova(model)$'Pr(>F)' [1] <= 0.05){
    new.Y<-predict(model, se = TRUE, interval = 'confidence') 
    
    #upper and lower CI
    x <- MinData[,paramsx[i]]
    x.ind<-order(x)
    lines(x[x.ind], new.Y$fit[x.ind,1])
    lines(x[x.ind], new.Y$fit[x.ind,2], col = 'gray', lty = 2)
    lines(x[x.ind], new.Y$fit[x.ind,3], col = 'gray', lty = 2)
    R2 <- summary(model)$'r.squared'
    legend('topright',legend= paste('R2 = ',round(R2,2)), bty='n')
  }
}
dev.off()


# Look at star metrics as function of temperature

paramsx<-colnames(MinData)[c(11:19)]
paramsy<-colnames(MinData)[c(5, 9)]


# plot star metrics x temps
png(paste('Plots/StarMetrics.png'), width = 1200, height = 1600)
par(mfrow = c(6,3))
for (i in 1:length(paramsx)){ #loop over different temp metrics
  for (j in 1:length(paramsy)){ #loop over different star metrics
  
  #model
  model <- lm(MinData[,paramsy[j]] ~ MinData[,paramsx[i]], data = MinData)
  
  plot(MinData[,paramsy[j]] ~ MinData[,paramsx[i]], data = MinData, main = paste(paramsy[j], 'by', paramsx[i]), xlab = 'Degrees C', ylab = 'Exposure Time (%)', cex = 1.5, cex.lab = 1.5)
  
  #CI lines
  if(anova(model)$'Pr(>F)' [1] <= 0.05){
    new.Y<-predict(model, se = TRUE, interval = 'confidence') 
    #upper and lower CI
    x<-MinData[,paramsx[i]]
    x.ind<-order(x)
    lines(x[x.ind], new.Y$fit[x.ind,1])
    lines(x[x.ind], new.Y$fit[x.ind,2], col = 'gray', lty = 2)
    lines(x[x.ind], new.Y$fit[x.ind,3], col = 'gray', lty = 2)
    R2 <- summary(model)$'r.squared'
    legend('topright',legend= paste('R2 = ',round(R2,2)), bty='n')
  }
  }
}
dev.off()



## Model with all 20 sites----

# Max ~ Temp
model.max <- lm(Mean.max ~ PC1 + PC2, data = MaxData)

  qqnorm(resid(model.max))
  qqline(resid(model.max))
  hist(resid(model.max))
  shapiro.test(resid(model.max))
  summary(model.max)
  plot(MaxData$Mean, MaxData$Mean.max)


png(paste('Plots/Max20.png'), width = 800, height = 600, res = 100)
  par(mar=c(5.1,5.1,2.1,2.1)) #margins = bottom, left, top, right
    
  #model
    model <- lm(MaxData$Mean.max ~ MaxData$Mean, data = MaxData)
    
    plot(MaxData$Mean.max ~ MaxData$Mean, data = MaxData, xlab = expression(paste('Annual Mean Temperature (', degree, 'C)')), ylab = 'Mussel Upper Limits (% Exposure Time)', pch=ifelse((MaxData$Mean.max < 80 & MaxData$Mean < 13), 1, 19), ylim = c(50,100), cex = 1.5, cex.lab = 1.5)

  dev.off()
  
  

# Try transformations

mod.max1 <- lm(sqrt(Mean.max) ~ PC1 + PC2, data = MaxData)
qqnorm(resid(mod.max1))
qqline(resid(mod.max1))
hist(resid(mod.max1))

mod.max2 <- lm(log(Mean.max) ~ PC1 + PC2, data = MaxData)
qqnorm(resid(mod.max2))
qqline(resid(mod.max2))
hist(resid(mod.max2))



# Remove CA, PP and re-run models----

MaxData18 <- MaxData[-which(MaxData$Site == c("CA", "PP")), ]

model.max18 <- lm(Mean.max ~ PC1 + PC2, data = MaxData18)


qqnorm(resid(model.max18))
qqline(resid(model.max18))
hist(resid(model.max18))
shapiro.test(resid(model.max18))
plot(MaxData18$Mean, MaxData18$Mean.max)
anova(model.max18) 
summary(model.max18)



png(paste('Plots/Max18.png'), width = 800, height = 600, res = 100)
par(mar=c(5.1,5.1,2.1,2.1)) #margins = bottom, left, top, right

#model
model <- lm(MaxData18$Mean.max ~ MaxData18$Mean, data = MaxData18)

plot(MaxData18$Mean.max ~ MaxData18$Mean, data = MaxData18, xlab = expression(paste('Mean Annual Temperature(', degree, 'C)')), ylab = 'Mussel Upper Limits (% Exposure Time)', pch = 19, cex = 1.5, cex.lab = 1.5)


#CI lines
if(anova(model)$'Pr(>F)' [1] <= 0.05){
  new.Y<-predict(model, se = TRUE, interval = 'confidence') 
  
  #upper and lower CI
  x <- MaxData18$Mean
  x.ind<-order(x)
  lines(x[x.ind], new.Y$fit[x.ind,1])
  lines(x[x.ind], new.Y$fit[x.ind,2], col = 'gray', lty = 2)
  lines(x[x.ind], new.Y$fit[x.ind,3], col = 'gray', lty = 2)
  R2 <- round(summary(model)$'r.squared',2)
  legend('topright', legend = expression(paste('R'^{2}, ' = 0.67, F = 14.96')), bty ='n')
}
dev.off()




# Min ~ Temp----
model.min <- lm(Mean.min ~ PC1 + PC2, data = MinData)
qqnorm(resid(model.min))
qqline(resid(model.min))
hist(resid(model.min))
shapiro.test(resid(model.min))
anova(model.min) 
summary(model.min) 



# Stars ~ Temp----
model.stars <- lm(MeanStar ~ PC1 + PC2, data = MinData)
qqnorm(resid(model.stars))
qqline(resid(model.stars))
hist(resid(model.stars))
shapiro.test(resid(model.stars))
anova(model.stars) 

plot(MinData$Mean, MinData$MeanStar)


#model


png(paste('Plots/Stars.png'), width = 800, height = 600, res = 100)
par(mar=c(5.1,5.1,2.1,2.1))

model <- lm(MinData$Mean.min[!is.na(MinData$MeanStar)] ~ MinData$MeanStar[!is.na(MinData$MeanStar)], data = MinData)

  plot(MinData$Mean.min[!is.na(MinData$MeanStar)] ~ MinData$MeanStar[!is.na(MinData$MeanStar)], data = MinData, xlab = 'Mean Sea Star Tide Height (% Exposure Time)', ylab = 'Mussel Lower Limits (% Exposure Time)', pch = 19, ylim = c(0, 85), cex = 1.5, cex.lab = 1.5)

#CI lines
  new.Y<-predict(model, se = TRUE, interval = 'confidence') 
  
  #upper and lower CI
  x <- MinData$MeanStar[!is.na(MinData$MeanStar)]
  x.ind<-order(na.exclude(x))
  lines(x[x.ind], new.Y$fit[x.ind,1])
  lines(x[x.ind], new.Y$fit[x.ind,2], col = 'gray', lty = 2)
  lines(x[x.ind], new.Y$fit[x.ind,3], col = 'gray', lty = 2)
  R2 <- round(summary(model)$'r.squared',2)
  legend('topright', legend = expression(paste('R'^{2}, ' = 0.55, F = 11.14')), bty ='n')
dev.off()


model.maxstars <- lm(MaxStar ~ PC1 + PC2, data = MinData)
qqnorm(resid(model.maxstars))
qqline(resid(model.maxstars))
hist(resid(model.maxstars))
shapiro.test(resid(model.maxstars))
anova(model.maxstars) 

plot(MinData$Mean, MinData$MaxStar)
  

paramsy<-colnames(MinData)[c(5,9)]

png(paste('Plots/Star*Temp.png'), width = 1200, height = 400)
par(mfrow = c(1,2))
for (i in 1:length(paramsy)){ #loop over different temp metrics
  
  #model
  model <- lm(MinData[,paramsy[i]] ~ MinData$PC1 + MinData$PC2)
  
  plot(MinData[,paramsy[i]] ~ MinData$PC1, data = MinData, main = paramsy[i], xlab = 'Degrees C', ylab = 'Exposure Time (%)', cex = 1.5, cex.lab = 1.5)
  
  #CI lines
  if(anova(model)$'Pr(>F)' [1] <= 0.05){
    new.Y<-predict(model, se = TRUE, interval = 'confidence') 
    
    #upper and lower CI
    x <- MinData19[,paramsy[i]]
    x.ind<-order(x)
    lines(x[x.ind], new.Y$fit[x.ind,1])
    lines(x[x.ind], new.Y$fit[x.ind,2], col = 'gray', lty = 2)
    lines(x[x.ind], new.Y$fit[x.ind,3], col = 'gray', lty = 2)
    R2 <- summary(model)$'r.squared'
    legend('topright',legend= paste('R2 = ',round(R2,2)), bty='n')
  }
}
dev.off()




# Minimum ~ Mean Predators----

mod.p <- lm(Mean.min ~ MeanStar, data = MinData)  
qqnorm(resid(mod.p))
qqline(resid(mod.p))
hist(resid(mod.p))
shapiro.test(resid(mod.p))
anova(mod.p)
summary(mod.p)

plot(Mean.min ~ MeanStar, data = MinData)



# Transformations
sqrt.modp <- lm(sqrt(Mean.min) ~ sqrt(MeanStar), data = MinData)
qqnorm(resid(sqrt.modp))
qqline(resid(sqrt.modp))
hist(resid(sqrt.modp))
shapiro.test(resid(sqrt.modp))
anova(sqrt.modp)

plot(sqrt(Mean.min) ~ sqrt(MaxStar), data = MinData)


log.modp <- lm(log(Mean.min) ~ MeanStar, data = MinData)
qqnorm(resid(log.modp))
qqline(resid(log.modp))
hist(resid(log.modp))
shapiro.test(resid(log.modp))
anova(log.modp)


# Minimum ~ Max Predators----

mod.pmax <- lm(Mean.max ~ MaxStar, data = MaxData)  
qqnorm(resid(mod.pmax))
qqline(resid(mod.pmax))
hist(resid(mod.pmax))
shapiro.test(resid(mod.pmax))
anova(mod.pmax)

plot(Mean.min ~ MaxStar, data = MinData)



# Transformations
sqrt.modpmax <- lm(sqrt(Mean.min) ~ sqrt(MaxStar), data = MinData)
qqnorm(resid(sqrt.modpmax))
qqline(resid(sqrt.modpmax))
hist(resid(sqrt.modpmax))
shapiro.test(resid(sqrt.modpmax))
anova(sqrt.modpmax)

plot(sqrt(Mean.min) ~ sqrt(MaxStar), data = MinData)

log.modpmax <- lm(log(Mean.min) ~ MaxStar, data = MinData)
qqnorm(resid(log.modpmax))
qqline(resid(log.modpmax))
hist(resid(log.modpmax))
shapiro.test(log(MinData$Mean.min))
anova(log.modpmax)

plot(log(Mean.min) ~ log(MaxStar), data = MinData)
summary(mod.p)


#t-test differences----
mean(MinData$Mean.min[MinData$StarDens == 0])
mean(MinData$Mean.min[MinData$StarDens > 0])
StarDif <- t.test(MinData$Mean.min[MinData$StarDens == 0], MinData$Mean.min[MinData$StarDens > 0])
StarDif <- t.test(Mean.min ~ PA, MinData)
StarDif



# Short term temps----

# scale the data and run PCA
iPCA <- prcomp(iTempMetrics[,2:5], scale = TRUE, center = TRUE)

# flip axis of PC2 so its more intuitive
iPCA$x[,2]<- (-iPCA$x[,2])

# Proportion of data explained by each axis
summary(iPCA)

# plot
png('Plots/iPCA.png')
fviz_pca_biplot(iPCA, xlab = "PC1 (73.7%)", ylab = "PC2 (23.5%)", repel = TRUE, col.var = "black", col.ind = "gray", title = " ", label = "var") + theme_base()
dev.off()


# individual point scores 
iPCAFrame<-data.frame(iPCA$x[,1:2],iTempMetrics$Site) # we want to make sure the sites line up correctly
colnames(iPCAFrame)[1]<-'iPC1'
colnames(iPCAFrame)[2]<-'iPC2'
colnames(iPCAFrame)[3]<-'Site'
iTempMetrics <- merge(iTempMetrics,iPCAFrame)


# merge datasets

MaxData18 <- merge(MaxData18, iTempMetrics)
colnames(MaxData18)

MinData <- merge(MinData, iTempMetrics)
colnames(MinData)


# iTemp Models----

# max ~ ST temp
iMax <- lm(MaxData18$Mean.max ~ MaxData18$iPC1 + MaxData18$iPC2, data = MaxData18)
qqnorm(resid(iMax))
qqline(resid(iMax))
hist(resid(iMax))
shapiro.test(resid(iMax))
anova(iMax)

max <- lm(Mean.max ~ iMean, data = MaxData18)
qqnorm(resid(max))
qqline(resid(max))
hist(resid(max))
shapiro.test(resid(max))
anova(max)


max1 <- lm(Mean.max ~ MeanStar, data = MaxData18)
qqnorm(resid(max1))
qqline(resid(max1))
hist(resid(max1))
shapiro.test(resid(max1))
anova(max1)



  # transformations
    sqrt.iMax <- lm(sqrt(MaxData18$Mean.max) ~ MaxData18$iPC1 + MaxData18$iPC2, data = MaxData18)
    qqnorm(resid(sqrt.iMax))
    qqline(resid(sqrt.iMax))
    hist(resid(sqrt.iMax))
    shapiro.test(resid(sqrt.iMax))
    anova(sqrt.iMax)

    log.iMax <- lm(log(MaxData18$Mean.max) ~ MaxData18$iPC1 + MaxData18$iPC2, data = MaxData18)
    qqnorm(resid(log.iMax))
    qqline(resid(log.iMax))
    hist(resid(log.iMax))
    shapiro.test(resid(log.iMax))
    anova(log.iMax)


# min ~  ST temp
iMin <- lm(MinData$Mean.min ~ MinData$iPC1 + MinData$iPC2, data = MinData)
    qqnorm(resid(iMin))
    qqline(resid(iMin))
    hist(resid(iMin))
    shapiro.test(resid(iMin))
    anova(iMin)
    summary(iMin)
    plot(Mean.min ~ iMean, data = MinData)
    
    
    # transformations
    sqrt.iMin <- lm(sqrt(MinData$Mean.min) ~ MinData$iPC1 + MinData$iPC2, data = MinData)
    qqnorm(resid(sqrt.iMin))
    qqline(resid(sqrt.iMin))
    hist(resid(sqrt.iMin))
    shapiro.test(resid(sqrt.iMin))
    anova(sqrt.iMin)
    
    log.iMin <- lm(log(MinData$Mean.min) ~ MinData$iPC1 + MinData$iPC2, data = MinData)
    qqnorm(resid(log.iMin))
    qqline(resid(log.iMin))
    hist(resid(log.iMin))
    shapiro.test(resid(log.iMin))
    anova(log.iMin)    
    
    
# mean stars ~ iTemp
iModp <- lm(MinData$MeanStar ~ MinData$iMean, data = MinData)
qqnorm(resid(iModp))
qqline(resid(iModp))
hist(resid(iModp))
shapiro.test(resid(iModp))
anova(iModp)

plot(MeanStar ~ iMean, data = MinData)

star <- lm(MeanStar ~ iMean, data = MinData)
qqnorm(resid(star))
qqline(resid(star))
hist(resid(star))
shapiro.test(resid(star))
anova(star)



#model
model <- lm(MinData$MeanStar[!is.na(MinData$MeanStar)] ~ MinData$iMean[!is.na(MinData$MeanStar)], data = MinData)


png(paste('Plots/iStars.png'), width = 800, height = 600, res = 100)
par(mar=c(5.1,5.1,2.1,2.1)) #margins = bottom, left, top, right

plot(MinData$MeanStar[!is.na(MinData$MeanStar)] ~ MinData$iMean[!is.na(MinData$MeanStar)], data = MinData, xlab = expression(paste('Mean Short-Term Temperature (', degree, 'C)')), ylab = 'Mean Sea Star Tide Height (% Exposure Time)', pch = 19, cex = 1.5, cex.lab = 1.5)

  new.Y<-predict(model, se = TRUE, interval = 'confidence') 
  
  #upper and lower CI
  x <- MinData$iMean[!is.na(MinData$MeanStar)]
  x.ind<-order(x)
  lines(x[x.ind], new.Y$fit[x.ind,1])
  lines(x[x.ind], new.Y$fit[x.ind,2], col = 'gray', lty = 2)
  lines(x[x.ind], new.Y$fit[x.ind,3], col = 'gray', lty = 2)
  R2 <- round(summary(model)$'r.squared',2)
  legend('topright', legend = expression(paste('R'^{2}, ' = 0.47, F = 7.95')), bty ='n')

dev.off()




# transformations
sqrt.iModp <- lm(sqrt(MinData$MeanStar) ~ MinData$iPC1 + MinData$iPC2, data = MinData)
qqnorm(resid(sqrt.iModp))
qqline(resid(sqrt.iModp))
hist(resid(sqrt.iModp))
shapiro.test(resid(sqrt.iModp))
anova(sqrt.iModp)


log.iModp <- lm(log(MinData$MeanStar) ~ MinData$iPC1 + MinData$iPC2, data = MinData)
qqnorm(resid(log.iModp))
qqline(resid(log.iModp))
hist(resid(log.iModp))
shapiro.test(resid(log.iModp))
anova(log.iModp)


# max stars ~ iTemp
iMaxP <- lm(MinData$MaxStar ~ MinData$iPC1 + MinData$iPC2, data = MinData)
qqnorm(resid(iMaxP))
qqline(resid(iMaxP))
hist(resid(iMaxP))
shapiro.test(resid(iMaxP))
anova(iMaxP)

plot(MaxStar ~ iMean, data = MinData)

StarData <- MinData[-which(MinData$StarDens == "0"), ]
exp.iMaxP <- lm(poly(MaxStar, 2) ~ iMean, data = StarData)
qqnorm(resid(exp.iMaxP))
qqline(resid(exp.iMaxP))
hist(resid(exp.iMaxP))
shapiro.test(resid(exp.iMaxP))
anova(exp.iMaxP)


sqrt.iMaxP <- lm(sqrt(MinData$MaxStar) ~ MinData$iPC1 + MinData$iPC2, data = MinData)
qqnorm(resid(sqrt.iMaxP))
qqline(resid(sqrt.iMaxP))
hist(resid(sqrt.iMaxP))
shapiro.test(resid(sqrt.iMaxP))
anova(sqrt.iMaxP)


log.iMaxP <- lm(log(MinData$MaxStar) ~ MinData$iPC1 + MinData$iPC2, data = MinData)
qqnorm(resid(log.iMaxP))
qqline(resid(log.iMaxP))
hist(resid(log.iMaxP))
shapiro.test(resid(log.iMaxP))
anova(log.iMaxP)


plot(log(MinData$MaxStar) ~ MinData$iMean)



png(paste('Plots/NS.png'), width = 1200, height = 1000, res = 100)
par(mar=c(5.1,5.1,2.1,2.1), mfrow = c(2,2)) #margins = bottom, left, top, right

#models
plot(MinData$Mean.min ~ MinData$Mean, data = MinData, xlab = expression(paste('Mean Annual Temperature(', degree, 'C)')), ylab = 'Mussel Lower Limits (% Exposure Time)', pch = 19, cex = 1.5, cex.lab = 1.5)
  legend('topright',legend= 'R2 = 0.04, F = 0.34', bty='n')
  
plot(MinData$Mean.min ~ MinData$iMean, data = MinData, xlab = expression(paste('Mean Short-Term Temperature(', degree, 'C)')), ylab = 'Mussel Lower Limits (% Exposure Time)', pch = 19, cex = 1.5, cex.lab = 1.5)
  legend('topright',legend= 'R2 = 0.24, F = 2.75', bty='n')
  
plot(MinData$MeanStar ~ MinData$Mean, data = MinData, xlab = expression(paste('Mean Short-Term Temperature(', degree, 'C)')), ylab = 'Mean Sea Star Tide Height (% Exposure Time)', pch = 19, cex = 1.5, cex.lab = 1.5)
  legend('topright',legend= 'R2 = 0.09, F = 0.40', bty='n')         

dev.off()



rm(iPCAFrame, iTempMetrics, MaxData19, i, iPCA, j, mod.max1, mod.max2, model, model.max, model.max18, model.max19, model.min, model.stars, new.Y, paramsx, paramsy, R2, StarDif, StarPA, x, x.ind, exp.iMaxP, iMax, iMaxP, iMin,  log.iMax, log.iMaxP, log.iMin, log.modp, log.modpmax, max, mod.p, mod.pmax, model.maxstars, sqrt.iMax, sqrt.iMaxP, sqrt.iMin, sqrt.modp, sqrt.modpmax, star, iModp, log.iModp, sqrt.iModp)
