run_analysis <- function() {
    
    features <- read.table("features.txt")
    features[, "V2"] <- as.character(features[, "V2"])
    features <- features[grepl("mean|std", features$V2),]
    activity_labels <- read.table("activity_labels.txt")
    names(activity_labels) <- c("Level", "Label")
    
    
    train_set <- read.table("train/X_train.txt")
    train_subjects <- read.table("train/subject_train.txt")
    names(train_subjects) <- "Subject"
    train_activity <- read.table("train/Y_train.txt")
    names(train_activity) <- "Activity"
    train_set <- train_set[, features$V1]
    names(train_set) <- features$V2
    dataset_train <- cbind(train_subjects, train_activity, train_set)
    
    
    test_set <- read.table("test/X_test.txt")
    test_subjects <- read.table("test/subject_test.txt")
    names(test_subjects) <- "Subject"
    test_activity <- read.table("test/Y_test.txt")
    names(test_activity) <- "Activity"
    test_set <- test_set[, features$V1]
    names(test_set) <- features$V2
    dataset_test <- cbind(test_subjects, test_activity, test_set)
    
    dataset1 <- rbind(dataset_train, dataset_test)
    dataset1$Activity <- factor(dataset1$Activity, levels = activity_labels$Level, labels = activity_labels$Label)
    dataset1 <- dataset1[order(dataset1$Subject),]
    
    
    
    library(plyr)
    library(dplyr)
    dataset2 <- ddply(dataset1, .(Subject, Activity), 
                      function(x) {
                          cm <- colMeans(select(x, -c(Subject, Activity)))
                          data.frame(t(cm))
                      })
    write.table(dataset2, file = "dataset.txt", row.names = FALSE)
    
}