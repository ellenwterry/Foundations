library(tidyverse)
library(MASS)
library(ISLR)
library(caret)

set.seed(913)

dfDefault = Default

p1 = ggplot(data = dfDefault, aes(balance, fill = default)) +
  geom_histogram(binwidth = 500) +
  facet_wrap(~ student)
p1


# ================= break on density ===============#

# you can also just get density:

p2 = ggplot(data = dfDefault, aes(balance, ..density..,  fill = default)) +
  geom_histogram(binwidth = 500) +
  facet_wrap(~ student)
p2

# densities are grouped here - by default, by student, so 4 density groups

# FYI, you can get the data for a ggplot object by using the 
# ggplot_build function - this creates an object w list 

pg <- ggplot_build(p1)

pgData <- data.frame(pg$data)





# now I'm creating a probability by multiplying density
# density is a function to find probability, given the area
# in discrete cases, we can find that by height (in density) * width
# in continuous cases, we need to use integration

pgData <- pgData %>% mutate(prob = (xmax- xmin)*density)

# so check to see if prob = 1:

pgData %>% group_by(PANEL, group) %>% summarise(Prob = sum(prob), Density = sum(density), na.rm = T)

# we can see the bins by listing the xmins

dplyr::select(pgData, xmin, xmax, PANEL, count, density, prob) %>% 
  group_by(xmin) %>%
  arrange(xmin)

# so, let's pull 1250-1750 in PANEL 1 where there's both groups 

dplyr::select(pgData, xmin, xmax, group, PANEL, count, density, prob) %>% 
  filter(xmin == 750, PANEL == 1)

p2 = ggplot(data = dfDefault, aes(balance, ..density..,  fill = default)) +
  geom_histogram(binwidth = 500) +
  facet_wrap(~ student)
p2

# translating to probs (this is a real stretch - just for understanding):

pgData = pgData %>% mutate(mid = ((xmin+xmax)/2) )

p2a = ggplot(data = pgData, aes(mid, prob,  fill = factor(group))) +
  geom_bar(stat="identity") +
  facet_wrap(~ factor(PANEL))
p2a

# another way to look at it

library(ggridges)

ggplot(dfDefault,
       aes(y = default, x = balance, fill = student)) +
  geom_density_ridges() +
  theme(panel.background = element_rect(fill = "white"))







# ================= end break on density ===============#

  
# since we grouped by bin = 500, we can also use cut_width to get the same, and assign groups:

dfDefault$balGrp = as.numeric(as.factor(cut_width(dfDefault$balance, 500)))

# there's also cut_interval(x, n = NULL, length = NULL, ...) and cut_number(x, n = NULL, ...)
# to discretize data

dfDefault %>% 
  group_by(balGrp, default) %>%
  tally()


DefaultAnalysis = dfDefault %>% 
  group_by(default, balGrp, student) %>%
  tally()


# so now we can use geom_bar and control the bin presentation

p = ggplot(DefaultAnalysis, aes(x = balGrp, y = n, fill = default)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ student)
p  

# slide 3

BayesN = dfDefault %>% filter(student == "Yes", balGrp == 3, default == "Yes") %>% 
  summarise(Tot = n())

BayesD = dfDefault %>% filter(student == "Yes", balGrp == 3) %>% 
  summarise(Tot = n())

BayesN/BayesD # (agrees to slide 3)

# slide 4:

# data - month 1
dfDefault1 = dfDefault %>% 
  dplyr::select(default, student, balance, balGrp, income) %>%
  mutate (month = 1)

# create month 2 with increase in defaults from 5 to 24

dfDefault2 = dfDefault1 %>%
  filter(default == "Yes", balGrp == 3, student == "Yes") %>%
  sample_n(20, replace = T) %>%
  mutate (balance = balance + rnorm(balance, 0, 30)) %>%
  filter(balance > 750, balance <1250) %>%
  bind_rows(dfDefault1) %>%
  mutate(month = 2)

dfDefaultNew = bind_rows(dfDefault1, dfDefault2) 

dfDefaultNew %>% 
  filter(student == "Yes", balGrp == 3, default == "Yes", month == 1) %>% 
  summarise(Tot = n())

# new posterior for month 2:
Bayes2N = dfDefault2 %>% filter(student == "Yes", balGrp == 3, default == "Yes") %>% 
  summarise(Tot = n())
Bayes2D = dfDefault2 %>% filter(student == "Yes", balGrp == 3) %>% 
  summarise(Tot = n())
Bayes2N/Bayes2D


# Slide 5. We could also analyze ALL (month1 + month 2) the data 
# (in which case, the posterior becomes 1.2%)

BayesNT = dfDefaultNew %>% filter(student == "Yes", balGrp == 3, default == "Yes") %>% 
  summarise(Tot = n())
BayesDT = dfDefaultNew %>% filter(student == "Yes", balGrp == 3) %>% 
  summarise(Tot = n())
BayesNT/BayesDT
