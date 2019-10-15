---
title: "DATS6101 Project 1 RMD"
author: "Sayra Moore, Andrea Piolini, Sabina Shahbazzade, Caroline Sklaver, Spencer Staub"
date: "10/13/2019"
output: html_document
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
```

## Data File

This data file was retrieved from https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results
'NOC' is the name of country, Medal with 'NA' value indicates athelete who did not win medal. We will be using only Summer Olympics data to narrow our questions and findings.

```{r read, echo=FALSE, include = TRUE}
#setwd('/Users/carolinesklaver/Desktop/Intro_DS/Project 1')
athlete_events = read.csv("athlete_events.csv")
str(athlete_events)
```


```{r NAs, echo=FALSE}
# omitting NAs leaves only athletes that won medals
summer_events = na.omit(athlete_events[athlete_events$Season == 'Summer',])
# keeping NAs for analysis of winners and losers
summer_w_NAs = athlete_events[athlete_events$Season=='Summer',]
```

## Descriptive Statistics

### Medals

The total number of Summer Olympic Games is 29 over 120 years (every 4 years except for 1916 when it was canceled due to WWI). The average total number of medals earned by each country is 36.72609.

```{r Descriptive Stats Medals, echo=FALSE, include=FALSE}
#Total Number of Summer Games
unique_games = unique(summer_w_NAs$Games)
length(unique_games)
# Medals per country
medals_table = table(summer_events$Medal,summer_events$NOC)
mean(medals_table)
```


Graphical representation of top countries total medals earned by medal type. This clearly shows teams that have dominated the 29 Olympic games. Additionally total number of medals earned by men vs. women. Women have earned fewer medals because the number of women athletes competing has trailed that of men (see athlete participation visual below).

```{r Descriptive Stats Medals Graphs, echo=FALSE, include=TRUE}
# stacked barplot of medals won by top countries
subset_medals = medals_table[,c('USA', 'URS', 'GBR', 'GER', 'ITA', 'FRA', 'AUS',
                                'CHN','HUN','JPN','NED','RUS','SWE')]
barplot(subset_medals, main = "Total medals won by country", 
        ylab = 'Country Code', xlab = "Number of medals won", col = c('gold', 'light gray','gold4'),
        legend.text = c("Gold",'Silver','Bronze'), horiz = TRUE, las=2)
# Graph of total numebr of medals won by men and women
athlete_events_MPYG = summer_events %>% select(Team, Medal, Sex, Year) %>% group_by(Team, Year, Sex)
medal_distribution_peryeargender = count(athlete_events_MPYG, Medal)
medal_distribution_peryeargender = subset(medal_distribution_peryeargender, medal_distribution_peryeargender$Medal != "NA")
ggplot(medal_distribution_peryeargender, aes(x = Sex, y = n, fill = Sex)) + geom_bar(stat = 'identity', width = 0.5) + scale_fill_manual(values=c('F' = 'deeppink1', 'M' = 'dodgerblue1')) + ggtitle('Comparison of Medals Won by Men and Women') + labs(x = 'Sex', y = 'Number of Medals')
```


## Athletes Participation Over 120 Years of Summer Olympic Games

Graphical representation of number of athletes over the 120 years of Olympics. Additionally, graph of male and female athlete participation over time to visualize the difference in participation by gender. Female participation has almost matched that of males with increasing numebr of female althletes at almost every game. Additional bar graphs to show popularity of sports both overall and differences in Male/Female.

```{r Descriptive Statistics Athletes, echo=FALSE, include=TRUE}
# participants per Year
library(dplyr)
athlete_events_PPYnn = summer_w_NAs %>% select(Name, Year) %>% group_by(Year)
participants_distribution = count(athlete_events_PPYnn, Year)
participants_distribution = subset(participants_distribution, participants_distribution$Year != "NA")
# participants over time
athlete_events_MPC = summer_w_NAs %>% group_by(Year) %>% summarise(count = n_distinct(Team))
ggplot(data=athlete_events_MPC, aes(x=Year, y=count)) +
  geom_line()+
  geom_point() + ggtitle('Number of Olympic athletes over time') + ylab('Number of participants')
# plot male female participants over time
group_count_mf <- summer_w_NAs %>% group_by(Year, Sex) %>%
  summarize(Athletes = length(unique(ID)))
group_count_mf$Year <- as.integer(group_count_mf$Year)
ggplot(group_count_mf, aes(x=Year, y=Athletes, group=Sex, color=Sex)) +
  geom_point(size=2) +
  geom_line()  +
  scale_color_manual(values=c("deeppink1","dodgerblue1")) +
  labs(title = "Number of male and female Olympic Athletes over time") +
  theme(plot.title = element_text(hjust = 0.5))
# participation by sport
sport_participation = sort(table(summer_w_NAs$Sport), decreasing = TRUE)
barplot(sport_participation[2:20], las=2, main="Total Atheletes by Sport", col = 'gray78')
# male and female by sport
female_w_NAs = summer_w_NAs[summer_w_NAs$Sex == 'F',]
male_w_NAs = summer_w_NAs[summer_w_NAs$Sex == 'M',]
female_sport_participation = sort(table(female_w_NAs$Sport), decreasing = TRUE)
male_sport_participation = sort(table(male_w_NAs$Sport), decreasing = TRUE)
barplot(female_sport_participation[2:10], las=2, main="Number of Female Athletes by Sport", col = 'deeppink1',
        ylim = c(0,18000))
barplot(male_sport_participation[2:10], las=2, main="Number of Male Athletes by Sport", col='dodgerblue1',
        ylim = c(0,18000))
```


# Age, Height, Weight

Histograms to view the distribution of Age, Height, and Weight for all Olympic Athletes. This is useful to determine the distribution. Age and weight are a bit right-skewed, height is very normal. These distributions do not vary greatly when looking at males and females separately. 

```{r , echo=FALSE, include=TRUE}
# Descriptive stats of Age, Height, Weight
par(mfrow=c(1,3))
hist(summer_w_NAs$Age, breaks = 30, main = 'Histogram of Age', xlab = 'Age', ylab = 'Frequency', col = 'rosybrown1')
#hist(f$Age, breaks = 30)
#hist(m$Age, breaks = 30)
hist(summer_w_NAs$Height, breaks = 30, main = 'Histogram of Height',xlab = 'Height(cm)', ylab = 'Frequency', col = 'darkseagreen3')
#hist(f$Height, breaks = 30)
#hist(m$Height, breaks = 30)
hist(summer_w_NAs$Weight, breaks = 30, main = 'Histogram of Weight',xlab = 'Weight(kg)', ylab = 'Frequency', col = 'lightblue3')
#hist(f$Weight, breaks = 30)
#hist(m$Weight, breaks = 30)
```

Correlations between age, height, and weight, also represented in graphics. Height and weight are highly correlated (0.7951830). 

```{r AHW Correlations, echo=FALSE, include=TRUE}
# Corrrelations between age, height, weight
corr_col = cor(na.omit(summer_w_NAs[c(4,5,6)]))
corr_col
library(corrplot)
corrplot(corr_col, method = "circle")
```


## Statistical Tests

### Does age, height, or weight determine outcome (winning of medals)?

Chi-squared tests in which the null hypothesis is that age, height, and weight do not have a significant effect on whether athletes won a medal or not.These tests resulted in p-values <0.05 meaning we can reject the null hypotheses and conclude that age, height, and weight all determine whether athletes win medals. The age, height, and weight data was made categorical into intervals based on quartiles and mean of each variable. 

```{r Chi-Squraed, echo=FALSE, include=TRUE}
# Chi Squared: Does age, height, or weight determine if you won a medal or not
# differenciate between winners and losers
summer_w_NAs <- subset(athlete_events, Season == 'Summer')
summer_w_NAs$Medal <- as.character(summer_w_NAs$Medal)
summer_w_NAs$Medal[is.na(summer_w_NAs$Medal)] <- "Loser"
summer_w_NAs$Medal[summer_w_NAs$Medal == 'Bronze'] <- 'Winner'
summer_w_NAs$Medal[summer_w_NAs$Medal == 'Silver'] <- 'Winner'
summer_w_NAs$Medal[summer_w_NAs$Medal == 'Gold'] <- 'Winner'
# make age/height/weight categorical
summary(summer_w_NAs$Age)
summer_w_NAs$Cat_Age = summer_w_NAs$Age
summer_w_NAs$Cat_Age = replace(summer_w_NAs$Cat_Age, summer_w_NAs$Cat_Age >= 28, 'old')
summer_w_NAs$Cat_Age = replace(summer_w_NAs$Cat_Age, summer_w_NAs$Cat_Age<=21, 'young')
summer_w_NAs$Cat_Age = replace(summer_w_NAs$Cat_Age, 
                               summer_w_NAs$Cat_Age > 21 & summer_w_NAs$Cat_Age < 28, 'avg')
summary(summer_w_NAs$Height)
summer_w_NAs$Cat_Height = summer_w_NAs$Height
summer_w_NAs$Cat_Height = replace(summer_w_NAs$Cat_Height, summer_w_NAs$Cat_Height >= 183, 'tall')
summer_w_NAs$Cat_Height = replace(summer_w_NAs$Cat_Height, summer_w_NAs$Cat_Height<=168, 'short')
summer_w_NAs$Cat_Height = replace(summer_w_NAs$Cat_Height, 
                               summer_w_NAs$Cat_Height > 168 & summer_w_NAs$Cat_Height < 183, 'avg')
summary(summer_w_NAs$Weight)
summer_w_NAs$Cat_Weight = summer_w_NAs$Weight
summer_w_NAs$Cat_Weight = replace(summer_w_NAs$Cat_Weight, summer_w_NAs$Cat_Weight >= 79, 'heavy')
summer_w_NAs$Cat_Weight = replace(summer_w_NAs$Cat_Weight, summer_w_NAs$Cat_Weight<=60, 'light')
summer_w_NAs$Cat_Weight = replace(summer_w_NAs$Cat_Weight, 
                               summer_w_NAs$Cat_Weight > 60 & summer_w_NAs$Cat_Weight < 79, 'avg')
cont_age_medal_w_na <- table(summer_w_NAs$Cat_Age, summer_w_NAs$Medal)
chitest4 = chisq.test(cont_age_medal_w_na)
chitest4
cont_height_medal_w_na <- table(summer_w_NAs$Cat_Height, summer_w_NAs$Medal)
chitest5 = chisq.test(cont_height_medal_w_na)
chitest5
cont_weight_medal_w_na <- table(summer_w_NAs$Cat_Weight, summer_w_NAs$Medal)
chitest6 = chisq.test(cont_weight_medal_w_na)
chitest6
```


### Does the average age vary between men and women?

Performed a 2-sample t-test to determine whether the mean age of men significantly differs from that of women. Null hypothesis is that the two means do not differ. With p-value < 0.05, we reject the null hypothesis and conclude that the average age of men and women significantly differs in summer Olympic athletes.

```{r ttest, echo=FALSE, include = TRUE}
# 2-Sample T-Test
m=subset(summer_w_NAs, Sex=='M')
f=subset(summer_w_NAs, Sex=='F')
t = t.test(m$Age,f$Age, alternative = "two.sided", var.equal = FALSE)
t
```

### Does age, height, and weight significantly change over the years?

Performed ANOVA test to determine whether the mean age, height, and weight of both male and female athletes separately changes over the 120 years of data (30 Olympic Games). Null hypotheses state that the average age, height, and weight of both males and females does not change over 30 Olympic Games. All of our p-values are < 0.05 meaning we reject the null hypotheses and conclude that the average age, height, and weight of both males and females significantly differs for at least one of the summer Olympic games. 

```{r ANOVA, echo=FALSE, include = TRUE}
# ANOVA - does mean height statistically change over the years
summer_w_NAs = na.omit(summer_w_NAs, Age)
summer_w_NAs$Year = as.factor(summer_w_NAs$Year)
male_aov = summer_w_NAs[summer_w_NAs$Sex == 'M',]
female_aov = summer_w_NAs[summer_w_NAs$Sex == 'F',]
library(RColorBrewer)
# ANOVA for Height
# Male
aov_m_height = aov(Height~Year, data=male_aov)
summary(aov_m_height)
# Female
aov_f_height = aov(Height~Year, data=female_aov)
summary(aov_f_height)
plot(Height~Year, data=male_aov, col = terrain.colors(29), main = 'Mean Male Height per Year', ylim=c(130,220))
plot(Height~Year, data=female_aov, col = terrain.colors(29), main = 'Mean Female Height per Year', ylim=c(130,220))
# ANOVA for Age
aov_m_age = aov(Age~Year, data=male_aov)
summary(aov_m_age)
# Female
aov_f_age = aov(Age~Year, data=female_aov)
summary(aov_f_age)
plot(Age~Year, data=male_aov, col = terrain.colors(29), main = 'Mean Male Age per Year')
plot(Age~Year, data=female_aov, col = terrain.colors(29), main = 'Mean Female Age per Year')
# ANOVA for Weight
aov_m_weight = aov(Weight~Year, data=male_aov)
summary(aov_m_weight)
# Female
aov_f_weight = aov(Weight~Year, data=female_aov)
summary(aov_f_weight)
plot(Weight~Year, data=male_aov, col = terrain.colors(29), main = 'Mean Male Weight per Year', ylim = c(20,150))
plot(Weight~Year, data=female_aov, col = terrain.colors(29), main = 'Mean Female Weight per Year', ylim = c(20,150))
```

```{r tukeyHSD, echo= FALSE, include=FALSE}
tukey_f_height <- TukeyHSD(aov_f_height)
tukey_f_height
tukey_m_height <- TukeyHSD(aov_m_height)
tukey_m_height
tukey_f_age <- TukeyHSD(aov_f_age)
tukey_f_age
tukey_m_age <- TukeyHSD(aov_m_age)
tukey_m_age
tukey_f_weight <- TukeyHSD(aov_f_weight)
tukey_f_weight
tukey_m_weight <- TukeyHSD(aov_m_height)
tukey_m_weight
```
