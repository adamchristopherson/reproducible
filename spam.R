library(kernlab)
data(spam)
# Perform the subsampling
set.seed(3435)
trainIndicator = rbinom(4601, size=1, prob = 0.5)
table(trainIndicator)
# Define training and test sets
trainSpam = spam[trainIndicator == 1, ]
testSpam = spam[trainIndicator == 0, ]

## Exploratory data analysis

names(trainSpam)
head(trainSpam)
# frequencies of words within each of the emails

table(trainSpam$type)
#capitalAve = avg number of capital letters
plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve+1)~ trainSpam$type)
# spam emails have much higher rate of capital letters

plot(log10(trainSpam[, 1:4]+1))

# see what variables tend to cluster together
hCluster = hclust(dist(t(trainSpam[,1:57])))
plot(hCluster)
# transform
hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:55] + 1))))
plot(hClusterUpdated)

# Statistical prediction/modeling
trainSpam$numType = as.numeric(trainSpam$type) - 1
costFunction = function(x,y) sum(x != (y > 0.5))
cvError = rep(NA, 55)
library(boot)
for (i in 1:55){
    lmFormula = reformulate(names(trainSpam)[i], response = "numType")
    glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
    cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}
# which predictor has minumum cross-validated error?
names(trainSpam)[which.min(cvError)]
# use best model from the group
predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam)

# get predictions on the test set
predictionTest = predict(predictionModel, testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])

# classify "as spam" for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"

# classification table
table(predictedSpam, testSpam$type)

## Interpret results
#  * fraction of characters that are $ can predict if email is spam
#  * anthing with more than 6.6% $ signs is spam
#  * more $ always means more spam
#  * our test set error rate was 22.4%
