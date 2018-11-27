
#### process leaf P and rainfall data for EucFACE

### source scripts
source("prepare.R")

### read in rainfall data
rainDF <- prepare_rainfall_data()
rainDF$Date <- as.Date(rainDF$Month)

### read in leaf P data
pDF <- read.csv("data/GreenLeaf_RingMeans_acrossYears-Mingkai.csv")
pDF$Yearf <- as.factor(pDF$Year)
pDF$Trt <- pDF$CO2.treat
    
### Analyse the variable model
## model 1: no interaction, year as factor, ring random factor, include covariate
modelt1 <- lmer(Pm.mean~Trt + face + (1|Ring),data=pDF)

## anova
m1.anova <- Anova(modelt1, test="F")

## Check ele - amb diff
summ1 <- summary(glht(modelt1, linfct = mcp(Trt = "Tukey")))

## average effect size
eff.size1 <- coef(modelt1)[[1]][1,2]

## predict
pDF$predicted <- predict(modelt1, newdata=pDF)


p<- ggplot(modelt1,aes(face, Pm.mean, col=Trt)) + 
    geom_line(aes(y=predicted, col=as.factor(Trt)), size=0.8) +
    theme_bw()
plot(p)


with(pDF, plot(Pm.mean~face))
