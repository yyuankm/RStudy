library(cvTools)
library(foreach)
library(randomForest)
set.seed(719)
K=10
R=3
cv <- cvFolds(NROW(iris), K=K, R=R)

grid <- expand.grid(ntrees=c(10,100,200), mtry=c(3,4))

result <- foreach(g=1:NROW(grid), .combine=rbind) %do% {
   foreach(r=1:R, .combine=rbind) %do% {
      foreach(k=1:K, .combine=rbind) %do% {
         validation_idx <- cv$subsets[which(cv$which == k), r]
         train <- iris[-validation_idx, ]
         validation <- iris[validation_idx, ]
         m <- randomForest(Species ~., 
                           data=train, 
                           ntree=grx=id[g, "ntrees"],
                           mtry=grid[g, "mtry"])
         predicted <- predict(m, newdata=validation)
         precision <- sum(predicted == validation$Species) / NROW(predicted)
         return(data.frame(g=g, precision=precision))
      }
   }
}
