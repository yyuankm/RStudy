
#Data reading and cleaning
titanic = read.csv("Dataset/Titanic/titanic3.csv") #To read data from csv file
titanic_new <- titanic[, !names(titanic) %in% c("home.dest", "boat", "body")] #To delete the unnecessary columns
titanic_new$pclass <- as.factor(titanic_new$pclass) #To convert the data type to factor for "Class"
titanic_new$name <- as.character(titanic_new$name) #To convert the data type to Character for "Name"
titanic_new$ticket <- as.character(titanic_new$ticket) #To convert the data type to Character for "Ticket"
titanic_new$cabin <- as.character(titanic_new$cabin) #To convert the data type to Character for "Cabin"
titanic_new$survived <- factor(titanic_new$survived, levels=c(0, 1), labels=c("dead", "survived")) #To convert the data type to factor for "Survive"
str(titanic_new)
levels(titanic_new$embarked)[1] <- NA
str(titanic_new)
titanic_new$cabin <- ifelse(titanic_new$cabin == "", NA, titanic$cabin)

#Training data preparation for cross validation
library(caret)
set.seed(137)
test_idx <- createDataPartition(titanic_new$survived, p=0.1)$Resample1
titanic_new.test <- titanic_new[test_idx, ]
titanic_new.train <- titanic_new[-test_idx, ]
NROW(titanic_new)
NROW(titanic_new.test)
NROW(titanic_new.train)
prop.table(table(titanic_new.train$survived))

save(titanic_new, titanic_new.test, titanic_new.train, file="Dataset/Titanic/Titanic.RData")



create_ten_fold_cv <- function() {

   set.seed(137)
   lapply(createFolds(titanic_new.train$survived, k=10), function(idx) {
      return(list(train=titanic_new.train[-idx,],
             validation=titanic_new.train[idx, ]))
   })
}


#Data investigation


