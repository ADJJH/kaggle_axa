#####################
#####Data Set Up#####
#####################
load("Data/small_axa.RData")

dim(axa) #600 Observations, 88 Variables
names(axa)
sapply(axa, class)
table(sapply(axa, class)) #Characters need to be converted to numeric type.

###Converting character variables to numeric variables.
converted.columns = sapply(axa[, 2:87], as.numeric)
axa.converted = data.frame(axa[, 1], converted.columns, axa[, 88])
names(axa.converted) = names(axa) 

axa.converted$driver = as.factor(substr(axa.converted$driver_trip, 1, 1))
names <- names(axa.converted)
names <- gsub("-", "_", names)
names(axa.converted) <- names
remove(axa)
axa = axa.converted
remove(axa.converted)





#######################################
#####Imputing & Dropping Variables#####
#######################################
library(VIM)
axa.new = axa[, -c(1, 74:79, 90)]
axa.knn = kNN(axa.new)
axa.imputed = axa.knn[, 1:82]





################################
#####AUC for Random Forests#####
################################
library(AUCRF)

Y = axa.imputed$isDriver
Y[which(Y == TRUE)] = 1
Y = as.factor(Y)
axa.binary = axa.imputed
axa.binary = axa.binary[, -81]
axa.binary = cbind("isDriver" = Y, axa.binary)

fit = AUCRF(isDriver ~ .,
            ntree = 1000,
            data = axa.binary)
summary(fit)
plot(fit)

fit.cv = AUCRFcv(fit,
                 nCV = 10, ###Number of folds.
                 M = 10) ###Number of CV repetitions.
summary(fit.cv)
plot(fit.cv)
names(fit.cv)

temp = NULL

for (i in 1:length(fit.cv$Xopt)) {
  temp = paste(temp, " + ", fit$Xopt[i], sep = "")
}

library(rpart)
final.model = rpart(isDriver ~ jerk_quantile.25. +
                      jerk_quantile.75. +
                      direct_dist +
                      attr.turn.ap.sd +
                      attr.turn.v.sd +
                      attr.turn.al.sd +
                      max.ap. +
                      attr.turn.ap.mean +
                      accel_sd +
                      accel_quantile.25. +
                      speed_sd +
                      accel_quantile.75. +
                      driver +
                      jerk_sd +
                      path_efficiency,
                    data = axa.binary,
                    control = rpart.control(minbucket = 1))

library(rattle)
fancyRpartPlot(final.model)
plot(final.model)
text(final.model)

predicted.classes = predict(final.model, axa.binary[, -1], type = "class")

library(mclust)
truth = axa.binary$isDriver
classError(predicted.classes, truth)
table(truth, predicted.classes)

misclass = classError(predicted.classes, truth)$misclassified
predict(final.model, axa.binary[, -1], type = "prob")[misclass]
