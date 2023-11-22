
rm(list = ls())
Sys.setenv(LANG = "en")

library("tidyverse")
library(glmnet) # Lasso
library(gbm) # Boosting

data <- readRDS("Data/training_data_cleaned.rds")
# data <- head(data, 10000)
# attach(data)


## Lasso Regression -----------------------------------------------------------

# Preparation:
x <- model.matrix(rent_full ~ ., data)[, -1]
y <- data$rent_full

# Splitting data into training and test set
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)  
test <- (-train) 
y.test <- y[test]

# Fit a lasso model
grid <- 10^seq(10, -2, length = 100) # lambda from 10^10 to 10^(-2)
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

# Use cross-validation to choose tuning parameter λ:
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1) # training data
plot(cv.out)
bestlam <- cv.out$lambda.min

# Calculate test MSE associated with this λ:
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test, ]) # test data
mean((lasso.pred - y.test)^2)

# Perform shrinkage method on full data set:
out <- glmnet(x, y, alpha = 1, lambda = grid)   # full data
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20, ]
lasso.coef[lasso.coef != 0] 




## Boosting -------------------------------------------------------------------

# Error in gbm.fit(x = x, y = y, offset = offset, distribution = distribution,  : 
# gbm does not currently handle categorical variables with more than 1024 levels. Variable 1: GDENAMK has 1857 levels.
data2 <- data %>% 
  select(-GDENAMK, 
         -GDENR, 
         -descr, 
         -key)

# Preparation:
x <- model.matrix(rent_full ~ ., data2)[, -1]
y <- data2$rent_full

# Splitting data into training and test set
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)  
test <- (-train) 
y.test <- y[test]

# Build Boosting:
set.seed(1)
boost.mod <- gbm(rent_full ~ ., data = data2[train , ],
                 distribution = "gaussian", n.trees = 5000,
                 interaction.depth = 4)
summary(boost.mod)

plot(boost.mod, i = "msregion")
plot(boost.mod, i = "area")
plot(boost.mod, i = "rooms")

# Predict dependent variable on test set:
yhat.boost <- predict(boost.mod, newdata = data2[-train , ], 
                      n.trees = 5000)
mean((yhat.boost - y.test)^2) # 125'100
plot(yhat.boost, y.test)

# Adjust shrinkage parameter λ: (only if wanted)
boost.mod <- gbm(rent_full ~ ., data = data2[train , ],
                 distribution = "gaussian", n.trees = 5000,
                 interaction.depth = 4, shrinkage = 0.2, verbose = F)
yhat.boost <- predict(boost.mod, newdata = data2[-train , ], 
                      n.trees = 5000)
mean((yhat.boost - y.test)^2) # 144'300

