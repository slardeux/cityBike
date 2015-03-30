setwd("/media/data/data_project/citybike")

library(dplyr)
library(gbm)
library(caret)

load('train/train_bin')
load('test/test_bin')
cmat <- list()
monthFit <- list()
for(i in 1:12){
  mth <- gmstbin %>% filter(month == i)
  mth <- mth[,-2]
  mtht <- gmstbin_test %>% filter(month == i)
  mtht <- mtht[,-2]
  
  set.seed(97)    
  gbmFit <- gbm(formula = value ~ .,            #def best for hjuly
                 distribution = "bernoulli", 
                 data = mth,
                 n.trees = 500,              
                 interaction.depth = 9,  
                 shrinkage = 0.1,          
                 verbose = FALSE,
                 cv.folds = 10,
                 n.cores = 4)   
  

  gbmPred <- predict(gbmFit, newdata = mtht, n.trees = 500,type = "response")
  pred <- ifelse(gbmPred > .5, 1, 0)
  
  cmat[[i]] <- confusionMatrix(pred, mtht$value)
  monthFit[[i]] <- gbmFit
  print(i)
}
save(monthFit, file = 'monthFit')
save(cmat, file = 'month_confmat')
