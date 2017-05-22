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
sample = iris[sample_ind,]
test = iris[-sample_ind,]


