library(tidyverse)
library(MASS)
library(ISLR)
library(caret)

set.seed(913)

dfDefault <- Default

dfDefault %>% dplyr::count(default)

p <- ggplot(dfDefault, aes(balance, fill = default)) +
  geom_histogram(binwidth = 500)

p

pl1 <- ggplot(dfDefault, aes(balance, fill = default)) 
pl1 <- pl1 + geom_density(alpha = 0.2, adjust = 5 )
pl1

# ----------------- 

lda.fit <- lda(default ~ balance, data = dfDefault) 
lda.fit

lda.pred <- predict(lda.fit)
dfPred <- data.frame(lda.pred)
dfPred %>% dplyr::count(class)

pl1 <- pl1 + geom_vline(xintercept = mean(lda.fit$means) )
pl1
p <- p + geom_vline(xintercept = mean(lda.fit$means) )
p

# get decision rule (don't worry about doing this - just FYI)
A <- A <- mean(lda.fit$means)
B <- log(lda.fit$prior[2]) - log(lda.fit$prior[1])
s2.k <- t(tapply(dfDefault$balance, dfDefault$default, var)) %*% lda.fit$prior
C <- s2.k/(lda.fit$means[1] - lda.fit$means[2])
dr <- A + B * C
dr

p <- p + geom_vline(xintercept = dr, color = "red" )
p

tst = confusionMatrix(factor(lda.pred$class) ,  factor(dfDefault$default), positive = "Yes")

tst$table
tst$byClass[1]
tst$byClass[2]
tst$byClass[3]
tst$byClass[4]



# look at the data again

firstAnalysis <- as_tibble(cbind(as.character(lda.pred$class), 
                                 as.character(dfDefault$default), 
                                 lda.pred$posterior))

firstAnalysis <- cbind(firstAnalysis,dplyr::select(dfDefault, student, balance, income))

#write_csv(firstAnalysis, "firstAnalysis.csv")

# let's adjust the threshold

pred <- rep('No', nrow(dfDefault))
pred[lda.pred$posterior[,2] >= 0.2] <- 'Yes' 
dfPred <- data.frame(pred)
dfPred %>% dplyr::count(pred)


confusionMatrix(factor(pred), factor(dfDefault$default),   positive = "Yes")


# now splitting into validation sets


dfDefault <- dfDefault %>% rownames_to_column("SampleID")
xTrain <- sample_n(dfDefault, round(nrow(dfDefault)*.6))
xTest <- dfDefault %>% anti_join(xTrain, by = "SampleID")


lda.fit <- lda(default ~ balance, xTrain) 
lda.pred <- predict(lda.fit, xTest)


# get decision rule

A <- A <- mean(lda.fit$means)
B <- log(lda.fit$prior[2]) - log(lda.fit$prior[1])
s2.k <- t(tapply(xTest$balance, xTest$default, var)) %*% lda.fit$prior
C <- s2.k/(lda.fit$means[1] - lda.fit$means[2])
dr <- A + B * C
dr
p <- p + geom_vline(xintercept = dr, color = 'red' )
p

# same place

confusionMatrix((lda.pred$class), factor(xTest$default), positive = "Yes")

# add more predictors (p)
# remember, visualization is gone in p>2

lda.fit <- lda(default ~ student + balance + income, xTrain) 
lda.fit

lda.pred <- predict(lda.fit, xTest)

confusionMatrix( lda.pred$class, factor(xTest$default), positive = "Yes")

# get back orignial and look at it:
#finalAnalysis <- as_tibble(cbind(as.character(lda.pred$class), as.character(xTest$default), lda.pred$posterior))
#finalAnalysis <- cbind(finalAnalysis,dplyr::select(xTest, student, balance, income))
#write_csv(finalAnalysis, "finalAnalysis.csv")





