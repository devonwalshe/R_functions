### Linear regression

### Dataset - IRIS
iris = iris

### Correlation analysis
corr = round(cor(iris[,c(1:4)]), 1) # Correlation matrix
corr_p = cor_pmat(iris[,c(1:4)]) # significance values for each one
cplot = ggcorrplot(corr, method='circle', hc.order=TRUE , outline.col='white', lab='true', p.mat=corr_p, insig='blank')

### looks like petal width ~ petal length

### split into sample and test
set.seed(90210)
smp_size = floor(0.30 * nrow(iris))
sample_ind = sample(seq_len(nrow(iris)), size=smp_size)
iris_sample = iris[sample_ind,]
iris_test = iris[-sample_ind,]

### fit a model between width and length
iris_fit = lm(Petal.Length ~ Petal.Width, data=sample)

### predict petal length on the test group
length_pred = predict(iris_fit, newdata= iris_test)
iris_test$length_pred = length_pred

### Calculate error of the model
iris_test = iris_test %>% mutate(prediction_error = length_pred-Petal.Length)

### Now try a multivariate regression
library(ggcorrplot)
car_corr = round(cor(mtcars), 3)
car_corr_p = cor_pmat(mtcars) # significance values for each one
car_cplot = ggcorrplot(car_corr, method='square', hc.order=TRUE , type=c('lower'), 
                       colors=c('red', 'white', 'green'), outline.col='white', 
                       lab='true', p.mat=car_corr_p, insig='blank', show.legend=FALSE, lab_size=3)

### Apply factors to vs, am, cyl, gear and carb
mtcars_factor = mtcars
mtcars_factor[,c("vs", "am", "cyl", "gear", "carb")]  = lapply(mtcars_factor[,c("vs", "am", "cyl", "gear", "carb")], as.factor)

### Fit model for mpg
car_fit = lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am+gear+carb, data=mtcars )
car_factor_fit = lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am+gear+carb, data=mtcars_factor )

### Test it out
mtcars$mpg_vanilla_pred = predict(car_fit, mtcars)
mtcars$mpg_factor_pred = predict(car_factor_fit, mtcars_factor)

### Plot the error
library(reshape2)
mtcars$model = row.names(mtcars)
mtmelt = melt(mtcars[,c('model', 'mpg', 'mpg_factor_pred', 'mpg_vanilla_pred')], id.vars="model")

mtmelt$model = factor(mtmelt$model, levels = row.names(mtcars[order(mtcars$mpg),]))
ggplot(mtmelt, aes(x=model, y=value, group=variable, colour=variable)) +
  geom_line(aes(group=variable)) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1),
        panel.background = element_blank(), axis.line=element_line(size=0.2),
        panel.grid.major=element_line(colour="#dddddd", size=0.1))

# interesting the factor model has less overall error
mtcars = mtcars %>% mutate(v_error = mpg_vanilla_pred-mpg, f_error = mpg_factor_pred-mpg) 
sum(mtcars$v_error^2)
sum(mtcars$f_error^2)

# Thats enough for now

