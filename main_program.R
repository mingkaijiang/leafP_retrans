
#### process leaf P and rainfall data for EucFACE

### source scripts
source("prepare.R")

### read in rainfall data
rainDF <- prepare_rainfall_data()
rainDF$Date <- as.Date(rainDF$Month)

### add pre-2013 data
val <- c(95.7, 43.2, 52, 41.2, 28.6, 25, 86.2, 39.8, 142.4, 77.2,
         161.3, 188.6, 168.6, 116.4, 13.7, 74.3, 11.5)
preDF <- data.frame(NA, val, NA)
colnames(preDF) <- c("Month", "Rain_mm_Tot", "Date")
preDF$Month <- c("2011-03-01", "2011-04-01", "2011-05-01", "2011-06-01",
                 "2011-07-01", "2011-08-01", "2011-09-01", "2011-10-01",
                 "2011-11-01", "2011-12-01",
                 "2012-01-01", "2012-02-02", "2012-03-01", "2012-04-01", 
                 "2012-05-01", "2012-06-01", "2012-07-01")

preDF$Date <- as.Date(preDF$Month)

rainDF <- rbind(preDF, rainDF)

rainDF$Month <- month(rainDF$Date)
rainDF$Year <- year(rainDF$Date)

### read in leaf P data
pDF <- read.csv("data/GreenLeaf_RingMeans_acrossYears-Mingkai.csv")
pDF$Yearf <- as.factor(pDF$Year)
pDF$Trt <- pDF$CO2.treat
testDF <- pDF[,c("entry", "Year", "Ring", "Nm.mean", "Pm.mean")]

### add rainfall information
yr.list <- unique(pDF$Year)
for (i in yr.list) {
    rain <- rainDF$Rain_mm_Tot[rainDF$Year==i&rainDF$Month==2]
    testDF$R1[testDF$Year==i] <- rep(rain,6)
    
    rain <- sum(rainDF$Rain_mm_Tot[rainDF$Year==i&rainDF$Month%in%c(1,2)])
    testDF$R2[testDF$Year==i] <- rep(rain,6)
    
    rain <- sum(rainDF$Rain_mm_Tot[rainDF$Year==i&rainDF$Month%in%c(1,2)],
                rainDF$Rain_mm_Tot[rainDF$Year==i-1&rainDF$Month%in%c(12)])
    testDF$R3[testDF$Year==i] <- rep(rain,6)
    
    rain <- sum(rainDF$Rain_mm_Tot[rainDF$Year==i&rainDF$Month%in%c(1,2)],
                rainDF$Rain_mm_Tot[rainDF$Year==i-1&rainDF$Month%in%c(11,12)])
    testDF$R4[testDF$Year==i] <- rep(rain,6)
    
    rain <- sum(rainDF$Rain_mm_Tot[rainDF$Year==i&rainDF$Month%in%c(1,2)],
                rainDF$Rain_mm_Tot[rainDF$Year==i-1&rainDF$Month%in%c(10,11,12)])
    testDF$R5[testDF$Year==i] <- rep(rain,6)
    
    rain <- sum(rainDF$Rain_mm_Tot[rainDF$Year==i&rainDF$Month%in%c(1,2)],
                rainDF$Rain_mm_Tot[rainDF$Year==i-1&rainDF$Month%in%c(9,10,11,12)])
    testDF$R6[testDF$Year==i] <- rep(rain,6)
    
    rain <- sum(rainDF$Rain_mm_Tot[rainDF$Year==i&rainDF$Month%in%c(1,2)],
                rainDF$Rain_mm_Tot[rainDF$Year==i-1&rainDF$Month%in%c(8,9,10,11,12)])
    testDF$R7[testDF$Year==i] <- rep(rain,6)
    
    rain <- sum(rainDF$Rain_mm_Tot[rainDF$Year==i&rainDF$Month%in%c(1,2)],
                rainDF$Rain_mm_Tot[rainDF$Year==i-1&rainDF$Month%in%c(7,8,9,10,11,12)])
    testDF$R8[testDF$Year==i] <- rep(rain,6)
    
    rain <- sum(rainDF$Rain_mm_Tot[rainDF$Year==i&rainDF$Month%in%c(1,2)],
                rainDF$Rain_mm_Tot[rainDF$Year==i-1&rainDF$Month%in%c(6,7,8,9,10,11,12)])
    testDF$R9[testDF$Year==i] <- rep(rain,6)
    
    rain <- sum(rainDF$Rain_mm_Tot[rainDF$Year==i&rainDF$Month%in%c(1,2)],
                rainDF$Rain_mm_Tot[rainDF$Year==i-1&rainDF$Month%in%c(5,6,7,8,9,10,11,12)])
    testDF$R10[testDF$Year==i] <- rep(rain,6)
    
    rain <- sum(rainDF$Rain_mm_Tot[rainDF$Year==i&rainDF$Month%in%c(1,2)],
                rainDF$Rain_mm_Tot[rainDF$Year==i-1&rainDF$Month%in%c(4,5,6,7,8,9,10,11,12)])
    testDF$R11[testDF$Year==i] <- rep(rain,6)
    
    rain <- sum(rainDF$Rain_mm_Tot[rainDF$Year==i&rainDF$Month%in%c(1,2)],
                rainDF$Rain_mm_Tot[rainDF$Year==i-1&rainDF$Month%in%c(3,4,5,6,7,8,9,10,11,12)])
    testDF$R12[testDF$Year==i] <- rep(rain,6)
}

    
### Analyse the variable model
mod.n.1 <- lm(Nm.mean~R1, data=testDF)
mod.n.2 <- lm(Nm.mean~R2, data=testDF)
mod.n.3 <- lm(Nm.mean~R3, data=testDF)
mod.n.4 <- lm(Nm.mean~R4, data=testDF)
mod.n.5 <- lm(Nm.mean~R5, data=testDF)
mod.n.6 <- lm(Nm.mean~R6, data=testDF)
mod.n.7 <- lm(Nm.mean~R7, data=testDF)
mod.n.8 <- lm(Nm.mean~R8, data=testDF)
mod.n.9 <- lm(Nm.mean~R9, data=testDF)
mod.n.10 <- lm(Nm.mean~R10, data=testDF)
mod.n.11 <- lm(Nm.mean~R11, data=testDF)
mod.n.12 <- lm(Nm.mean~R12, data=testDF)

mod.p.1 <- lm(Pm.mean~R1, data=testDF)
mod.p.2 <- lm(Pm.mean~R2, data=testDF)
mod.p.3 <- lm(Pm.mean~R3, data=testDF)
mod.p.4 <- lm(Pm.mean~R4, data=testDF)
mod.p.5 <- lm(Pm.mean~R5, data=testDF)
mod.p.6 <- lm(Pm.mean~R6, data=testDF)
mod.p.7 <- lm(Pm.mean~R7, data=testDF)
mod.p.8 <- lm(Pm.mean~R8, data=testDF)
mod.p.9 <- lm(Pm.mean~R9, data=testDF)
mod.p.10 <- lm(Pm.mean~R10, data=testDF)
mod.p.11 <- lm(Pm.mean~R11, data=testDF)
mod.p.12 <- lm(Pm.mean~R12, data=testDF)

## r squared
rsq.n.1 <- summary(mod.n.1)$r.squared
rsq.n.2 <- summary(mod.n.2)$r.squared
rsq.n.3 <- summary(mod.n.3)$r.squared
rsq.n.4 <- summary(mod.n.4)$r.squared
rsq.n.5 <- summary(mod.n.5)$r.squared
rsq.n.6 <- summary(mod.n.6)$r.squared
rsq.n.7 <- summary(mod.n.7)$r.squared
rsq.n.8 <- summary(mod.n.8)$r.squared
rsq.n.9 <- summary(mod.n.9)$r.squared
rsq.n.10 <- summary(mod.n.10)$r.squared
rsq.n.11 <- summary(mod.n.11)$r.squared
rsq.n.12 <- summary(mod.n.12)$r.squared

rsq.p.1 <- summary(mod.p.1)$r.squared
rsq.p.2 <- summary(mod.p.2)$r.squared
rsq.p.3 <- summary(mod.p.3)$r.squared
rsq.p.4 <- summary(mod.p.4)$r.squared
rsq.p.5 <- summary(mod.p.5)$r.squared
rsq.p.6 <- summary(mod.p.6)$r.squared
rsq.p.7 <- summary(mod.p.7)$r.squared
rsq.p.8 <- summary(mod.p.8)$r.squared
rsq.p.9 <- summary(mod.p.9)$r.squared
rsq.p.10 <- summary(mod.p.10)$r.squared
rsq.p.11 <- summary(mod.p.11)$r.squared
rsq.p.12 <- summary(mod.p.12)$r.squared

## p values
pv.n.1 <- anova(mod.n.1)$'Pr(>F)'[1]
pv.n.2 <- anova(mod.n.2)$'Pr(>F)'[1]
pv.n.3 <- anova(mod.n.3)$'Pr(>F)'[1]
pv.n.4 <- anova(mod.n.4)$'Pr(>F)'[1]
pv.n.5 <- anova(mod.n.5)$'Pr(>F)'[1]
pv.n.6 <- anova(mod.n.6)$'Pr(>F)'[1]
pv.n.7 <- anova(mod.n.7)$'Pr(>F)'[1]
pv.n.8 <- anova(mod.n.8)$'Pr(>F)'[1]
pv.n.9 <- anova(mod.n.9)$'Pr(>F)'[1]
pv.n.10 <- anova(mod.n.10)$'Pr(>F)'[1]
pv.n.11 <- anova(mod.n.11)$'Pr(>F)'[1]
pv.n.12 <- anova(mod.n.12)$'Pr(>F)'[1]

pv.p.1 <- anova(mod.p.1)$'Pr(>F)'[1]
pv.p.2 <- anova(mod.p.2)$'Pr(>F)'[1]
pv.p.3 <- anova(mod.p.3)$'Pr(>F)'[1]
pv.p.4 <- anova(mod.p.4)$'Pr(>F)'[1]
pv.p.5 <- anova(mod.p.5)$'Pr(>F)'[1]
pv.p.6 <- anova(mod.p.6)$'Pr(>F)'[1]
pv.p.7 <- anova(mod.p.7)$'Pr(>F)'[1]
pv.p.8 <- anova(mod.p.8)$'Pr(>F)'[1]
pv.p.9 <- anova(mod.p.9)$'Pr(>F)'[1]
pv.p.10 <- anova(mod.p.10)$'Pr(>F)'[1]
pv.p.11 <- anova(mod.p.11)$'Pr(>F)'[1]
pv.p.12 <- anova(mod.p.12)$'Pr(>F)'[1]


## combine r square and p values
checkDF <- data.frame(rep(c(1:12), 2), NA, NA, NA)
colnames(checkDF) <- c("Antecedent_Month", "Rsquared", "Pvalue", "Nutrient")
checkDF$Nutrient <- rep(c("N", "P"), each=12)

### assign
checkDF$Rsquared[checkDF$Antecedent_Month==1&checkDF$Nutrient=="N"] <- rsq.n.1
checkDF$Rsquared[checkDF$Antecedent_Month==2&checkDF$Nutrient=="N"] <- rsq.n.2
checkDF$Rsquared[checkDF$Antecedent_Month==3&checkDF$Nutrient=="N"] <- rsq.n.3
checkDF$Rsquared[checkDF$Antecedent_Month==4&checkDF$Nutrient=="N"] <- rsq.n.4
checkDF$Rsquared[checkDF$Antecedent_Month==5&checkDF$Nutrient=="N"] <- rsq.n.5
checkDF$Rsquared[checkDF$Antecedent_Month==6&checkDF$Nutrient=="N"] <- rsq.n.6
checkDF$Rsquared[checkDF$Antecedent_Month==7&checkDF$Nutrient=="N"] <- rsq.n.7
checkDF$Rsquared[checkDF$Antecedent_Month==8&checkDF$Nutrient=="N"] <- rsq.n.8
checkDF$Rsquared[checkDF$Antecedent_Month==9&checkDF$Nutrient=="N"] <- rsq.n.9
checkDF$Rsquared[checkDF$Antecedent_Month==10&checkDF$Nutrient=="N"] <- rsq.n.10
checkDF$Rsquared[checkDF$Antecedent_Month==11&checkDF$Nutrient=="N"] <- rsq.n.11
checkDF$Rsquared[checkDF$Antecedent_Month==12&checkDF$Nutrient=="N"] <- rsq.n.12

checkDF$Rsquared[checkDF$Antecedent_Month==1&checkDF$Nutrient=="P"] <- rsq.p.1
checkDF$Rsquared[checkDF$Antecedent_Month==2&checkDF$Nutrient=="P"] <- rsq.p.2
checkDF$Rsquared[checkDF$Antecedent_Month==3&checkDF$Nutrient=="P"] <- rsq.p.3
checkDF$Rsquared[checkDF$Antecedent_Month==4&checkDF$Nutrient=="P"] <- rsq.p.4
checkDF$Rsquared[checkDF$Antecedent_Month==5&checkDF$Nutrient=="P"] <- rsq.p.5
checkDF$Rsquared[checkDF$Antecedent_Month==6&checkDF$Nutrient=="P"] <- rsq.p.6
checkDF$Rsquared[checkDF$Antecedent_Month==7&checkDF$Nutrient=="P"] <- rsq.p.7
checkDF$Rsquared[checkDF$Antecedent_Month==8&checkDF$Nutrient=="P"] <- rsq.p.8
checkDF$Rsquared[checkDF$Antecedent_Month==9&checkDF$Nutrient=="P"] <- rsq.p.9
checkDF$Rsquared[checkDF$Antecedent_Month==10&checkDF$Nutrient=="P"] <- rsq.p.10
checkDF$Rsquared[checkDF$Antecedent_Month==11&checkDF$Nutrient=="P"] <- rsq.p.11
checkDF$Rsquared[checkDF$Antecedent_Month==12&checkDF$Nutrient=="P"] <- rsq.p.12

checkDF$Pvalue[checkDF$Antecedent_Month==1&checkDF$Nutrient=="N"] <- pv.n.1
checkDF$Pvalue[checkDF$Antecedent_Month==2&checkDF$Nutrient=="N"] <- pv.n.2
checkDF$Pvalue[checkDF$Antecedent_Month==3&checkDF$Nutrient=="N"] <- pv.n.3
checkDF$Pvalue[checkDF$Antecedent_Month==4&checkDF$Nutrient=="N"] <- pv.n.4
checkDF$Pvalue[checkDF$Antecedent_Month==5&checkDF$Nutrient=="N"] <- pv.n.5
checkDF$Pvalue[checkDF$Antecedent_Month==6&checkDF$Nutrient=="N"] <- pv.n.6
checkDF$Pvalue[checkDF$Antecedent_Month==7&checkDF$Nutrient=="N"] <- pv.n.7
checkDF$Pvalue[checkDF$Antecedent_Month==8&checkDF$Nutrient=="N"] <- pv.n.8
checkDF$Pvalue[checkDF$Antecedent_Month==9&checkDF$Nutrient=="N"] <- pv.n.9
checkDF$Pvalue[checkDF$Antecedent_Month==10&checkDF$Nutrient=="N"] <- pv.n.10
checkDF$Pvalue[checkDF$Antecedent_Month==11&checkDF$Nutrient=="N"] <- pv.n.11
checkDF$Pvalue[checkDF$Antecedent_Month==12&checkDF$Nutrient=="N"] <- pv.n.12

checkDF$Pvalue[checkDF$Antecedent_Month==1&checkDF$Nutrient=="P"] <- pv.p.1
checkDF$Pvalue[checkDF$Antecedent_Month==2&checkDF$Nutrient=="P"] <- pv.p.2
checkDF$Pvalue[checkDF$Antecedent_Month==3&checkDF$Nutrient=="P"] <- pv.p.3
checkDF$Pvalue[checkDF$Antecedent_Month==4&checkDF$Nutrient=="P"] <- pv.p.4
checkDF$Pvalue[checkDF$Antecedent_Month==5&checkDF$Nutrient=="P"] <- pv.p.5
checkDF$Pvalue[checkDF$Antecedent_Month==6&checkDF$Nutrient=="P"] <- pv.p.6
checkDF$Pvalue[checkDF$Antecedent_Month==7&checkDF$Nutrient=="P"] <- pv.p.7
checkDF$Pvalue[checkDF$Antecedent_Month==8&checkDF$Nutrient=="P"] <- pv.p.8
checkDF$Pvalue[checkDF$Antecedent_Month==9&checkDF$Nutrient=="P"] <- pv.p.9
checkDF$Pvalue[checkDF$Antecedent_Month==10&checkDF$Nutrient=="P"] <- pv.p.10
checkDF$Pvalue[checkDF$Antecedent_Month==11&checkDF$Nutrient=="P"] <- pv.p.11
checkDF$Pvalue[checkDF$Antecedent_Month==12&checkDF$Nutrient=="P"] <- pv.p.12

p<- ggplot(pDF,aes(Feb.annual, Nm.mean)) + 
    geom_point(data=pDF, aes(Feb.annual, Nm.mean))+
    geom_line(aes(y=predicted), size=0.8) +
    theme_bw()
plot(p)


with(pDF, plot(Pm.mean~face))
