### PCA Reference

### DATA
mt_pca = mtcars

### Remove response variable
mt_pca = mt_pca %>% select(-mpg)

### Apply PCA to features
mt_pca = prcomp(mt_pca, scale.=T)

### Visualize the components
biplot(mt_pca, scale=0)

### Compute the varience from the stdev of the principle components
mt_pca_var = mt_pca$sdev^2

### Compute the proportional varience for each component
mt_pca_propvar = mt_pca_var/sum(mt_pca_var)

### Scree plot the proportional varience
plot(mt_pca_propvar, type='b')

### Cumulative varience plot
plot(cumsum(mt_pca_propvar), type='b')

### add components onto response variables - 4 in this case
mt_pca_test = data.frame(mpg = mtcars$mpg, mt_pca$x[,1:4])

### Give svm a try
mt_pca_svm_fit = svm(mpg ~ ., mt_pca_test)
mt_pca_svm_tune = tune(svm, mpg ~ ., data=mt_pca_test, ranges = list(epsilon=seq(0,1,0.1), cost = 2^(2:9)))


