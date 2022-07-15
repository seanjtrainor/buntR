###################################
# Sean Trainor                    #
# http://github.com/seanjtrainor  #
###################################

################################
# Building the Model
################################



boost.bunt.all <- gbm(runs_created ~., data = sit_all_comp,
                      distribution = "poisson", n.trees = 5000,
                      interaction.depth = 4, cv.folds = 5)

saveRDS(boost.bunt.all, "model.rds")

#print(boost.bunt.all)
#sqrt(min(boost.bunt.all$cv.error))
#gbm.perf(boost.bunt.all, method = "cv")
#summary(boost.bunt.all)