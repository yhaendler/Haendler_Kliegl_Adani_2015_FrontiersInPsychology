## Experiment: Nellie -- German object relatives with different kinds of embedded pronouns ##

## Published as:
# Haendler, Y., Kliegl, R. & Adani, F. (2015).
# Discourse accessibility constraints in children's processing of object relative clauses
# Frontiers in Pscyhology 6:860.

## Information about the data:
# Eyetracking data are cleaned: - excluded data points outside areas of interest (aoi).
#                               - excluded trials with more than 50% data loss.

## Data set is restricted to include only relevant time window: 
# from 3656ms (200ms after offset of the relative pronoun "den")
# until 6471ms (end of the silence period that followed the sentence)

rm(list=ls())

#setwd("C:/Users/yairh/Desktop/Experiments/Nellie")

# loading the data
load("nellie_children.RData")

#----------------------------------------------------------------------------------------
## Excluding 3 children for whom we don't have scores on the standardized tests
dat.c2 <- droplevels(dat.c[-which(dat.c$bueva==200), ])

#----------------------------------------------------------------------------------------
## Scores on standardized tests ##

# language
dat.c2$language <- (dat.c2$tsvk_3 + dat.c2$tsvk_5 + dat.c2$tsvk_6) / 3

# memory
dat.c2$memory <- (dat.c2$digit_fwrd + dat.c2$digit_bwrd) / 2

#----------------------------------------------------------------------------------------
## Dividing time into 50ms bins ## 

binsize <- 50
dat.c2$bin <- floor(dat.c2$ms/binsize)*binsize

#----------------------------------------------------------------------------------------
## Excluding fillers ##

dat.c2 <- droplevels(dat.c2[-which(dat.c2$condition=="filler"), ])

################################################################################################
### STATISTICS ###
################################################################################################

## Analysis procedure is based on: 
# Barr, D.J. (2008). 
# Analyzing 'visual world' eyetracking data using multilevel logistic regression.
# Journal of Memory and Language 59(4), 457-474.

# aggregating data - across items
stat <- droplevels( ddply(subset(dat.c2,condition!="OR+dem"), 
                          .(bin, id, condition, language, memory), 
                          summarize,
                          aoi.t=sum(aoi.target),
                          aoi.d=sum(aoi.distractor),
                          aoi.m=sum(aoi.middle)) )

# sum of looks to target
stat$y <- stat$aoi.t

# sum of looks to all aoi's
stat$N <- stat$aoi.t + stat$aoi.d + stat$aoi.m

# empirical logit
stat$elog <- log( (stat$y + .5) / (stat$N - stat$y + .5) )

# weights
stat$wts <- 1 / (stat$y+.5) + 1 / (stat$N - stat$y + .5)

# time variable in seconds
stat$sec <- stat$bin/1000

# centering time variable: around 4000ms (based on Grand Mean plot -- see below)
stat$sec.cent <- stat$sec - 4

# contrast for condition
library(MASS)
stat$condition <- factor(stat$condition, levels=c("OR+1pro", "OR+2DP", "OR+3pro"))
( contrasts(stat$condition) <- contr.sdif(3) )

# scaling standardized measures
stat$language_sc <- scale(stat$language, center=T)
stat$memory_sc <- scale(stat$memory, center=T)

# LMM
library(lme4)

m1 <- lmer(elog ~ poly(sec.cent,2)*condition*memory_sc*language_sc +  
             (1|id), data=stat, REML=FALSE, weights=1/wts)

print(summary(m1), corr=FALSE)

#----------------------------------------------------------------------------------------
## plotting the partial effects ##

# how many fixed effects
matrix(names(fixef(m1)))

# we need the remef function
source("C:/Users/yairh/Desktop/Experiments/remef.v0.6.10.R")

# eliminating individual differences from the dependent variable
stat$ia <- remef(m1, keep=TRUE, fix=1:36, ran=NULL)
# all 36 fixed effects are kept, because the four-way interactions were significant

# dividing the group between high and low scorers on the language and memory tests
stat$lang.div <- factor(ifelse(stat$language<median(stat$language),"low","high"))
stat$mem.div <- factor(ifelse(stat$memory<median(stat$memory),"low","high"))

# for the plot: time starts at zero
stat$time <- stat$sec.cent - min(stat$sec.cent)

# naming the levels of the factors in the plot
levels(stat$condition) <- c("1st-person\npronoun","full DP","3rd-person\npronoun")
levels(stat$lang.div) <- c("High\nlanguage\nscore","Low\nlanguage\nscore")
levels(stat$mem.div) <- c("High","Low")

# plotting
library(ggplot2)

plot <- ggplot(data=stat, geom="smooth", method=lm, formula=y~poly(x,2),
               aes(x=time, y=round(ia), linetype=mem.div, color=mem.div)) + 
  
  ylab("Adjusted proportion of target looks") + ylim(-3,3) + 
  xlab("Time (sec.)") + theme_bw() + 
  
  facet_grid(lang.div~condition) + 
  
  scale_colour_manual(values=c("#800000", "#2121D9"), name="Memory score") + 
  scale_linetype_manual(values=c(1,2), name="Memory score") + 
  
  geom_smooth(method=lm, size=2.5, formula=y~poly(x,2)) + 
  
  # line to mark the end of sentence
  geom_vline(aes(xintercept=4155/1000-3.95), size=1, linetype=2, col="black") +
  
  # Bigger axis titles
  theme(axis.title.x=element_text(size=26, angle=0),
        axis.title.y=element_text(size=26, angle=90)) + 
  
  # Bigger text on axes
  theme(axis.text.x=element_text(size=20, colour="black"),
        axis.text.y=element_text(size=20, colour="black")) + 
  
  # Bigger legend
  theme(legend.title=element_text(size=24),
        legend.text=element_text(size=22)) + 
  
  # Legend position
  theme(legend.justification=c(1,1), legend.position=c(1,1)) + 
  
  # Box around legend
  theme(legend.background=element_rect(fill="transparent"),
        legend.key.width = unit(2.5, "cm")) + 
  
  # Bigger text in facets
  theme(strip.text.x = element_text(size=20, angle=0),
        strip.text.y = element_text(size=20, angle=0)) + 
  
  # removing background grid
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank()) 

plot

#----------------------------------------------------------------------------------------
## Grand Mean plot ##

library(plyr)

##### STEP 1 #####
agg1 <- ddply(subset(dat.c2,condition!="OR+dem"), # excluding irrelevant condition
              .(bin), summarize,
              PLT = mean(aoi.target) / (mean(aoi.target)+mean(aoi.middle)+mean(aoi.distractor)) )

plot(agg1$PLT ~ agg1$bin) 
# onset of increase of PLT across conditions: at 4000ms
#----------------------------------------------------------------------------------------
