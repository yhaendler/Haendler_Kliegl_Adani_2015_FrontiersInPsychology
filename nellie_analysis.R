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

library(dplyr)

#----------------------------------------------------------------------------------------
## Excluding 3 children for whom we don't have scores on the standardized tests
dat.c2 <- dat.c %>%
  filter(bueva != 200) %>%
  droplevels()

#----------------------------------------------------------------------------------------
## Scores on standardized tests ##

dat.c2 <- dat.c2 %>%
  mutate(langauge = (tsvk_3 + tsvk_5 + tsvk_6) / 3,   # language test
         memory = (digit_fwrd + digit_bwrd) / 2       # memory test
         )

#----------------------------------------------------------------------------------------
## Dividing time into 50ms bins ## 

binsize <- 50
dat.c2$bin <- floor(dat.c2$ms/binsize)*binsize

#----------------------------------------------------------------------------------------
## Excluding fillers ##
dat.c2 <- dat.c2 %>%
  filter(condition != 'filler') %>%
  droplevels()

################################################################################################
### STATISTICS ###
################################################################################################

## Analysis procedure is based on: 
# Barr, D.J. (2008). 
# Analyzing 'visual world' eyetracking data using multilevel logistic regression.
# Journal of Memory and Language 59(4), 457-474.

# aggregating data - across items

stat <- dat.c2 %>%
  filter(condition != 'OR+dem') %>%
  droplevels() %>%
  group_by(bin, id, condition, langauge, memory),
  summarize(aoi.t = sum(aoi.target),
            aoi.d = sum(aoi.distractor),
            aoi.m = sum(aoi.middle))

stat <- stat %>%
  mutate(y = aoi.t,                 # sum of looks to target
         N = aoi.t + aoi.d + aoi.m  # sum of looks to all aoi's
         ) %>% 
  mutate(elog = log( (y + .5) / (N - y + .5) ), # empirical logit
         wts = 1 / (y+.5) + 1 / (N - y + .5),   # weights
         sec = bin / 1000                       # time in seconds
         ) %>%
  mutate(sec.cent = sec - 4                     # centering time variable: around 4000ms (based on Grand Mean plot -- see below)
         )

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

ggplot(data=stat, geom="smooth", method=lm, formula=y~poly(x,2),
       aes(x=time, y=round(ia), linetype=mem.div, color=mem.div)) + 
  
  ylab("Adjusted proportion of target looks") + ylim(-3,3) + 
  xlab("Time (sec.)") + theme_bw() + 
  
  facet_grid(lang.div~condition) + 
  
  scale_colour_manual(values=c("#800000", "#2121D9"), name="Memory score") + 
  scale_linetype_manual(values=c(1,2), name="Memory score") + 
  
  geom_smooth(method=lm, size=2.5, formula=y~poly(x,2)) + 
  
  # line to mark the end of sentence
  geom_vline(aes(xintercept=4155/1000-3.95), size=1, linetype=2, col="black") +
  
  theme(
    # Bigger axis titles  
    axis.title.x=element_text(size=26, angle=0),
    axis.title.y=element_text(size=26, angle=90),
    # Bigger text on axes
    axis.text.x=element_text(size=20, colour="black"),
    # Bigger legend
    legend.title=element_text(size=24),
    legend.text=element_text(size=22), 
    # Legend position
    legend.justification=c(1,1), legend.position=c(1,1),
    # Box around legend
    legend.background=element_rect(fill="transparent"),
    legend.key.width = unit(2.5, "cm"),
    # Bigger text in facets
    strip.text.x = element_text(size=20, angle=0),
    strip.text.y = element_text(size=20, angle=0),
    # removing background grid
    panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
    panel.background=element_blank()) 

#----------------------------------------------------------------------------------------
## Grand Mean plot ##

library(plyr)

##### STEP 1 #####
agg <- dat.c2 %>%
  filter(condition != 'OR+dem') %>%  # excluding the irrelevant condition
  group_by(bin) %>%
  summarize(PLT = mean(aoi.target) + ( mean(aoi.target) + mean(aoi.middle) + mean(aoi.distractor) )
            )

plot(agg$PLT ~ agg$bin) 
# onset of increase of PLT across conditions: at 4000ms
#----------------------------------------------------------------------------------------
