### Neural networks
library(dplyr, nnet)

### Data

### Vanilla train and test splitting
set.seed(90210)
smp_size = floor(0.70 * nrow(iris))
train_ind = sample(seq_len(nrow(iris)), size=smp_size)

### Split into more even sample with caret
library(caret)
train_ind = createDataPartition(iris$Species, times = 4, p = 0.7, list = TRUE, groups = min(1, length(iris)))[[1]]
i_train = iris[train_ind,]
i_test = iris[-train_ind,]

### Try single layer neuralnet with nnet
library(nnet)
i_fit = nnet(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, 
             size=30, 
             data = iris, 
             maxit=1000)

i_cm = confusionMatrix(i_test$Species, i_test$prediction)

i_test$prediction = colnames(predict(i_fit, i_test)[,apply(predict(i_fit, i_test), 1, function(x) which(x==max(x)))])

### Try out neuralnet
library(neuralnet)

### Convert train to model matrix with categorical output vars as binary
train_m = data.frame(model.matrix(~ . +Species+0, data=i_train))

### Fit
n = colnames(train_m)
nn_form = as.formula(paste(paste(n[grep("Species", n)], collapse = " + "), " ~ " , paste(n[-grep("Species", n)], collapse = " + "), sep=""))
nn_ifit = neuralnet(nn_form, 
                    train_m,
                    hidden = c(3,10),
                    lifesign="full",
                    stepmax="1e+05",
                    threshold = 0.01,
                    rep=3)

### Inspect the fit
summary(nn_ifit)
nn_prediction = compute(nn_ifit, i_test[-5])
pred_id = apply(nn_prediction$net.result, 1, which.max)
prediction = c("setosa", "versicolor", "virginica")[pred_id]
confusionMatrix(i_test$Species, prediction)


