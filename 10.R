
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
library(caret)
data(iris)
set.seed(123)
train_index <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
train_data <- iris[train_index, ]
test_data <- iris[-train_index, ]
tree_model <- rpart(Species ~ .,
 data = train_data,
 method = "class", # For classification
 control = rpart.control(minsplit = 10, cp = 0.01))
rpart.plot(tree_model, box.palette = "auto", nn = TRUE)
predictions <- predict(tree_model, test_data, type = "class")
confusionMatrix(predictions, test_data$Species)

