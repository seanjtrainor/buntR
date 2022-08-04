###################################
# Sean Trainor                    #
# http://github.com/seanjtrainor  #
###################################

###################################
#Training an XGBoost Model
###################################

set.seed(3434)

splits <- createDataPartition(sit_all_comp$runs_created, p = .8, list = F)
train <- sit_all_comp[splits, ]
test <- sit_all_comp[-splits, ]

#split train and validation set
train_id <- createDataPartition(train$runs_created, p = .8, list = F)
training <- train[train_id,]
validation <- train[-train_id,]

#define predictor and response variables in training set
train_x = data.matrix(training[, -3])
train_y = training$runs_created

#define predictor and response variables in validation set
valid_x = data.matrix(validation[, -3])
valid_y = validation$runs_created

#define predictor and response variables in testing set
test_x = data.matrix(test[, -3])
test_y = test$runs_created

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_valid = xgb.DMatrix(data = valid_x, label = valid_y)
xgb_test = xgb.DMatrix(data = test_x)

#define watchlist
watchlist = list(train=xgb_train, valid=xgb_valid)

#fit XGBoost model and display training and testing data at each round
params = list(booster = "gbtree", objective = "count:poisson", eta = .3)
model = xgb.train(data = xgb_train, objective = "count:poisson", max.depth = 3, watchlist=watchlist, nrounds = 2000)
xgbpred <- predict(model, xgb_test)
RMSE(test_y, xgbpred)

mat <- xgb.importance(feature_names = colnames(train_x), model = model)
xgb.plot.importance(importance_matrix = mat[1:20])


########################################
#Fine Tune the Model
#########################################

# Create empty lists
lowest_error_list = list()
parameters_list = list()

# Create 10,000 rows with random hyperparameters
set.seed(206)
for (iter in 1:50){
  param <- list(booster = "gbtree",
                objective = "count:poisson",
                max_depth = sample(3:10, 1),
                eta = runif(1, .01, .3),
                subsample = runif(1, .7, 1),
                colsample_bytree = runif(1, .6, 1),
                min_child_weight = sample(0:10, 1)
  )
  parameters <- as.data.frame(param)
  parameters_list[[iter]] <- parameters
}

# Create object that contains all randomly created hyperparameters
parameters_df = do.call(rbind, parameters_list)

# Use randomly created parameters to create 10,000 XGBoost-models
for (row in 1:nrow(parameters_df)){
  set.seed(20)
  mdcv <- xgb.train(data=xgb_train,
                    booster = "gbtree",
                    objective = "count:poisson",
                    max_depth = parameters_df$max_depth[row],
                    eta = parameters_df$eta[row],
                    subsample = parameters_df$subsample[row],
                    colsample_bytree = parameters_df$colsample_bytree[row],
                    min_child_weight = parameters_df$min_child_weight[row],
                    nrounds= 2000,
                    eval_metric = "poisson-nloglik",
                    early_stopping_rounds= 30,
                    print_every_n = 100,
                    watchlist = watchlist
  )
  lowest_error <- as.data.frame(min(mdcv$evaluation_log$test_poisson_nloglik))
  lowest_error_list[[row]] <- lowest_error
}

lowest_error_df <- do.call(rbind, lowest_error_list)

# Bind columns of accuracy values and random hyperparameter values
randomsearch = cbind(lowest_error_df, parameters_df) %>%
  arrange(desc('min(mdcv$evaluation_log$test_poisson_nloglik)'))

#build final model

xg.model.fin <- xgb.train(data=xgb_train,
                  booster = "gbtree",
                  objective = "count:poisson",
                  max_depth = 6,
                  eta = .07,
                  subsample = .858,
                  colsample_bytree = .935,
                  min_child_weight = 7,
                  nrounds= 2000,
                  eval_metric = "poisson-nloglik",
                  early_stopping_rounds= 30,
                  print_every_n = 100,
                  watchlist = watchlist
)

saveRDS(xg.model.fin, "model.rds")

pred <- predict(xg.model.fin, xgb_test)

################################
# Building the Model
################################



#boost.bunt.all <- gbm(runs_created ~., data = sit_all_comp,
#                      distribution = "poisson", n.trees = 5000,
#                      interaction.depth = 4, cv.folds = 5)

#saveRDS(boost.bunt.all, "model.rds")

#print(boost.bunt.all)
#sqrt(min(boost.bunt.all$cv.error))
#gbm.perf(boost.bunt.all, method = "cv")
#summary(boost.bunt.all)