### Performing gam modeling

require(scales)
library(mgcv)
library(visreg)
library(plyr)
library(ggplot2)

library(tidyverse)
library(magrittr)
library(nlme)
library(MuMIn)

## read input
myDF <- read.csv("data/Means_LeafAge_figS1.csv")

myDF$pos <- with(myDF, Pm.mean+Pm.std.error)
myDF$neg <- with(myDF, Pm.mean-Pm.std.error)

### gam
g <- gam(Pm.mean~s(LeafAge, k=15), data=myDF)

### plot
p <- ggplot(myDF, aes(x=LeafAge, y=Pm.mean)) +
    geom_point()+
    geom_errorbar(data=myDF, mapping=aes(ymax=pos, ymin=neg), 
                   width=0.6, size=0.4) +    
    geom_smooth(method=gam, formula = y~s(x, bs="ps"))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_line(color="grey"),
          legend.position="none")+
    ylab(expression(paste("Leaf ", P[m], " (mg ", g^-1, ")")))+
    xlab("Leaf Age (days)")

pdf("output/gam_figure.pdf")
plot(p)
dev.off()
