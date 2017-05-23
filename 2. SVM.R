### SVM model testing

### Data
cc = read.csv("./data/creditcard.csv")
mtsvr = mtcars[order(mtcars$mpg),]
mtsvr_factor = mtsvr
mtsvr_factor[,c("vs", "am", "cyl", "gear", "carb")]  = lapply(mtsvr_factor[,c("vs", "am", "cyl", "gear", "carb")], as.factor)
mtsvr_norm = data.frame(scale(mtsvr))

library(e1071)

### Fit model
mtsvr_fit = svm(mpg ~ ., mtsvr)
mtsvr_factor_fit = svm(mpg ~ ., mtsvr_factor)
mtsvr_norm_fit = svm(mpg ~ ., mtsvr_norm)

### predict
mtsvr_pred = predict(mtsvr_fit, mtsvr)
mtsvr_factor_pred = predict(mtsvr_factor_fit, mtsvr_factor)
mtsvr_norm_pred = predict(mtsvr_norm_fit, mtsvr_norm)

### Check fitted model error
plot(mtsvr$mpg)
lines(mtsvr_pred, col="red", pch=4)
lines(mtsvr_factor_pred, col="blue", pch=19)
plot(mtsvr_norm$mpg)
lines(mtsvr_norm_pred, col="green", pch=3)

### inspect SVM parameters
sqrt(mean((mtsvr$mpg-mtsvr_pred)^2)) # 2.12 RMSE of the vanilla vit
sqrt(mean((scale(mtsvr$mpg)-scale(mtsvr_pred))^2)) # .32 - scaled vanilla fit - still better
sqrt(mean((mtsvr$mpg-mtsvr_factor_pred)^2)) # 2.30 RMSE of the factor fit
sqrt(mean((mtsvr_norm$mpg-mtsvr_norm_pred)^2)) # .35 RMSE of the scaled fit

### RMSE is worse than our multivariate linear model!

### Tune the model with grid-search
mtsvr_tuned = tune(svm, mpg ~ ., data=mtsvr, ranges = list(epsilon=seq(0,0,0.1), cost = 2^(2:9)))
plot(mtsvr_tuned)
mtsvr_tuned = tune(svm, mpg ~ ., data=mtsvr, ranges = list(epsilon=seq(0.3,0.6,0.01), cost = 2^(2:9)))
plot(mtsvr_tuned)


### Test the prediction