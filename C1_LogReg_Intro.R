library(tidyverse)
library(ISLR)
library(caret)
# from book

set.seed(0907)

dfDefault <- Default

p <- ggplot(dfDefault, aes(balance, fill = default)) +
  geom_histogram(binwidth = 500) +
  facet_wrap(~student) +
  theme(panel.background = element_rect(fill = "white")) 
p

pl1 <- ggplot(dfDefault, aes(balance, fill = default))  + 
  geom_density(alpha = 0.2, adjust = 5 ) +
  theme(panel.background = element_rect(fill = "white")) 
pl1

# when you get into equations and many modeling algortihms, you'll find that 
# variable values need to be integers (0 and 1 only), and factors will often convert to 1, 2:
unique(as.numeric(dfDefault[,"default"]))
dfDefault$default <- as.integer(dfDefault$default)-1

dfDefault <- dfDefault %>% rownames_to_column("SampleID")
train <- sample_n(dfDefault, round(nrow(dfDefault)*.6,0))
test <- dfDefault %>% anti_join(train, by = "SampleID")

# showing equation with one categorical varible

glm.fit <- glm(default ~ student, data = dfDefault, family = binomial)
summary(glm.fit)
dfDefault$Prob <- predict(glm.fit, type = "response")
ggplot(dfDefault, aes(x=balance, y=Prob)) + geom_point()  


# now one continuous variable

glm.fit <- glm(default ~ balance, data = train, family = binomial)
summary(glm.fit)
test$Prob <- predict(glm.fit, type = "response", newdata = test)
ggplot(test, aes(x=balance, y=Prob)) + geom_point()  


# ----------------------  multiple logistic regression ---------------- #

glMod2 <- glm(default ~ student + balance + income, data = train, family = binomial)
summary(glMod2)
test$mProb2 <- predict(glMod2, type = "response", newdata = test)

mTest = model.matrix(default ~ student + balance + income, data = test)
bet1 <- as.numeric(glMod2$coefficients)
test$tmProb2 <- exp( t(bet1%*%t(mTest)))/(1+exp(t(bet1%*%t(mTest))))


ggplot(test, aes(x=balance, y=tmProb2, color = factor(student))) + 
  geom_point() +
  theme(panel.background = element_rect(fill = "white")) 


# let prove the eqquation out:
sum(round(test$tProb - test$mProb,0))

# how did we do? 

test$class = factor(if_else(test$tmProb < .5, "No", "Yes"))  
test$D2 = factor(if_else(test$default < .5, "No", "Yes"))  

table(test$class, test$D2)

confusionMatrix((test$class), factor(test$D2), positive = "Yes")
test %>% group_by(default) %>% summarise(Cnt = n())

