library(dplyr)
library(ggplot2)
library(broom)

wram_raw <- read.csv("one_platform_wram_raw.csv", header = TRUE)

wram_raw2 <- wram_raw %>%
  group_by(Animal, Treatment, Day) %>%
  mutate(avr_distance = mean(Distance)) %>%
  mutate(avr_latency = mean(Duration))
#this will create a new data.frame with average distance per animal per day, allocated to all 4 trials

wram_raw2 %>%
  summarise(Day, avr_distance, Treatment, distinct(avr_distance))
  ggplot(aes(x = Day, y = avr_distance, group = Treatment, colour = Treatment)) + geom_point() + geom_line()

wram_raw2 %>%
  ggplot(aes(x = Day, y = avr_distance, group = Treatment, colour = Treatment)) + geom_line() + geom_jitter()

wram_raw %>%
  ggplot(aes(x = Day, y = Distance, group = Treatment, colour = Treatment)) + geom_jitter() + geom_smooth(method = "lm")
#this makes a graph of all distance data, coloured by treatment(1 = SEA, 0 = SAL)

lm1 <- lm(Distance ~ Day + Treatment + Day:Treatment, data = wram_raw)
#linear regression of distance on Day, Treatment, and the interaction Day:Treatment)

lm2 <- lm(Distance ~ Day + Treatment + Sex + Day:Treatment, data = wram_raw)
#linear regression of distance on Day, Treatment, Sex, and the interaction Day:Treatment)

tidy(lm1)
#summary of lm1

??broom

summary(lm1)
#the bigger summary of lm1

head(augment(lm1))

tidy(lm2)
#summary of lm2

summary(lm2)
#the bigger summary of lm2

wram_raw2 %>%
  ggplot(aes(x = Day, y = avr_distance, group = Sex)) + geom_point() + geom_line()

anova(lm2)

wram_raw5 <- wram_raw %>%
  filter(Day != 1)

wram_raw5 %>%
  ggplot(aes(x = Day, y = Distance, group = Treatment, colour = Treatment)) + geom_jitter() + geom_smooth(method = "lm", se = FALSE)

lm6 <- lm(Distance ~ Day + Treatment + Sex + Day:Treatment, data = wram_raw5)
summary(lm6)

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

summarySE(wram_raw_female2)

install.packages(psych)

install.packages("pastecs")
library(pastecs)

avr_female_latency <- wram_raw_female2 %>%
  group_by(Day) %>%
  distinct(avr.latency) %>%
  stat.desc(avr_female_latency)


demo1 <- read.csv("http://www.ats.ucla.edu/stat/data/demo1.csv")
## Convert variables to factor
demo1 <- within(demo1, {
  group <- factor(group)
  time <- factor(time)
  id <- factor(id)
})

par(cex = .6)

with(demo1, interaction.plot(time, group, pulse,
                             ylim = c(5, 20), lty= c(1, 12), lwd = 3,
                             ylab = "mean of pulse", xlab = "time", trace.label = "group"))

demo1.aov <- aov(pulse ~ group * time + Error(id), data = demo1)
summary(demo1.aov)