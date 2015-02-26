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
