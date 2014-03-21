# dogcat_class.R

# Contains classification methods for the Dog/Cat Asirra Project

library("e1071")
library("jpeg")
library("randomForest")

# Take matrix and test half the files against the other half
# (12500 vs 12500)

# METHODS

# 1) Logistic Regression

# 2) Random Forests

# *********** DEFINING TRAINING & TEST DATA ***************

# rows: number of rows in original matrix
# sampsize: number of entries in teach train & test set
# (defaults to 500), so train 1000, test 1000
# if set to 6250: train 12500, test 12500
# Returns the indices to use in order to sample
# two equal sized sets of cats and dogs
samp.nums = function(rows, sampsize = 500)
{
    
    cats.nums = sample(1:(rows/2), sampsize*2, replace = FALSE)
    cats.nums.tr = cats.nums[1:sampsize]
    cats.nums.ts = cats.nums[(1+sampsize):(sampsize*2)]
    dogs.nums = sample((1+(rows/2)):rows, sampsize*2, replace = FALSE)
    dogs.nums.tr = dogs.nums[1:sampsize]
    dogs.nums.ts = dogs.nums[(1+sampsize):(sampsize*2)]
    
    indices = data.frame(cats.nums.tr, cats.nums.ts,
                         dogs.nums.tr, dogs.nums.ts)
    indices
}

# FULL TRAINING & TEST DATA

FULL DATA: nums = samp.nums(25000, 6250)
# SAMPLE ONLY: nums = samp.nums(25000, 500)

train.samp.c = x[(nums[,1]),] # Select cats.nums.tr indices
train.samp.d = x[(nums[,3]),] # Select dogs.nums.tr indices
train.samp = rbind(train.samp.c, train.samp.d) # Combine
train.indices = c(nums[,1],nums[,3]) # Identifies indices chosen
train.y = c(rep(0,6250),rep(1,6250))
test.samp.c = x[(nums[,2]),] # Select cats.nums.ts indices
test.samp.d = x[(nums[,4]),] # Select dogs.nums.ts indices
test.samp = rbind(test.samp.c, test.samp.d) # Combine
test.indices = c(nums[,2],nums[,4]) # Identifies indices chosen
test.y = c(rep(0,6250),rep(1,6250))


# Used for testing only a small number of pictures
# mini.train.y = train.y[6201:6300]
# mini.train.samp = train.samp[6201:6300,]
# mini.test.y = test.y[6201:6300]
# mini.test.samp = test.samp[6201:6300,]

# ****** GLM CLASSIFICATION ******

# For a set with fewer variables, could use:
# fit1 = glm(mini.train.y~mini.train.samp, family = binomial)
# NOTE: This will NOT work! There's too few entries, even with the entire
# training data, to get a result that isn't NAs.
# Instead we must use PENALIZED GLM.

# Cross-validated penalized GLM with glmnet

glm.start = proc.time()
fit2 = cv.glmnet(x = train.samp, y = train.y, family = "binomial")
pred2 = predict(fit2, newx = test.samp, type = "class", s = fit2$lambda.min)
glm.time = proc.time() - glm.start

# Error Rate of Classification
glm.cat.err = sum(as.numeric(pred2[1:6250]))/6250
glm.dog.err = 1 - sum(as.numeric(pred2[6251:12500]))/6250 
glm.avg.err = mean(c(glm.cat.err, glm.dog.err))

# Constructing a table of error values
glm.cats = c(6250-(glm.cat.err*6250),glm.cat.err*6250)
glm.dogs = c(glm.dog.err*6250, 6250-(glm.dog.err*6250))
glm.df = as.data.frame(matrix(c(glm.cats,glm.dogs),2,2, byrow = TRUE))
row.names(glm.df) = c("cats", "dogs")
colnames(glm.df) = c("cats", "dogs")
glm.df

plot(fit2, main = "Cross-Validation Curve")
coef(fit2, s = "lambda.min")

# ****** RANDOM FOREST CLASSIFICATION ******
train.y.f = as.factor(train.y)
test.y.f = as.factor(test.y)


# Sample version
rf.start = proc.time()
rf.train = randomForest(x = train.samp, y = train.y.f,
                        xtest = test.samp, ytest = test.y.f,
                        ntree=50)
rf.time = proc.time() - rf.start

# Full version - 100 trees
train.y.f = as.factor(c(rep(0,6250),rep(1,6250)))
test.y.f = as.factor(c(rep(0,6250),rep(1,6250)))

rf.start.100 = proc.time()
rf.trees.100 = randomForest(x = train.samp, y = train.y.f,
                        xtest = test.samp, ytest = test.y.f,
                        ntree=100)
rf.time.100 = proc.time() - rf.start.100
# Mistake: take rf.time.100 = rf.time.100 + rf.start - rf.start.100
rf.time.fixed = rf.time.100 + rf.start - rf.start.100
    
# Once we have rf.trees.100, we can observe the error present
# for a given number of trees.

rf.cat.err = rf.trees.100$err.rate[,2]
rf.dog.err = rf.trees.100$err.rate[,3]
rf.avg.err = c(rf.cat.err + rf.dog.err)/2

# Error Rate Plot - Random Forests
plot(rf.cat.err, type = "l", main = "Random Forests Error Rate",
     col = "red", ylim = c(0.25,0.45), xlab = "# Trees",
     ylab = "Misclassification Rate")
lines(rf.dog.err, col = "blue")
lines(rf.avg.err, col = "black")
abline(h = rf.avg.err[100], lty=2)
legend("topright", legend = c("Cats", "Dogs", "Average"),
       col = c("red", "blue", "black"), pch = 15)
legend("bottomleft", legend = "Final Average Error",
       col = "black", pch = "_")
