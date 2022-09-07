rm(list=ls()) # remove all data previously loaded
install.packages("Hmisc")
install.packages("tidyverse")
library(Hmisc)
library(tidyverse)


COVID19_data <- read.csv("C:/Users/digic/Desktop/Covid_19 Data Project/COVID19_line_list_data.csv")
describe(COVID19_data)

# Cleaned up death column 
COVID19_data$death_dummy <- as.integer(COVID19_data$death != 0)
# death rate
sum(COVID19_data$death_dummy) / nrow(COVID19_data)

# AGE
# claim: people who die are older
dead = subset(COVID19_data, death_dummy == 1)
alive = subset(COVID19_data, death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

# Is this satistically significant?
t.test(alive$age, dead$age, alternative="two.sided",conf.level = 0.99)

# normally if the p-value is < 0.05, we reject the null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis
# conclude that this is statistically significant

# GENDER
# claim: gender has no effect
male = subset(COVID19_data, gender == "male") #8.5%
female = subset(COVID19_data, gender == "female") #3.5%
mean(male$death_dummy, na.rm = TRUE)
mean(female$death_dummy, na.rm = TRUE)

# Is this satistically significant?
t.test(male$death_dummy, female$death_dummy, alternative="two.sided",conf.level = 0.99)

# 99% Confidence that men have from .8% to 8.8% higher fatality than women
# p-value = 0.002 < 0.05 so this is statistically signifigant

