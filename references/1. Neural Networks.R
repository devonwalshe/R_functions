### Neural networks

### Data

### Vanilla train and test splitting
set.seed(90210)
smp_size = floor(0.70 * nrow(iris))
train_ind = sample(seq_len(nrow(iris)), size=smp_size)

### Split into more even sample with caret
library(caret)
createDataPartition(iris, times = 1, p = 0.7, list = TRUE, groups = min(1, length(iris)))