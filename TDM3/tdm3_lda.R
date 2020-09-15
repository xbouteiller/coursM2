# http://www.sthda.com/english/articles/36-classification-methods-essentials/146-discriminant-analysis-essentials-in-r/


# LDA
# install.packages("caret")

library(tidyverse)
library(caret)
library(ade4)
theme_set(theme_classic())

# Load the data
data("iris")

# Describe Df

str(iris)

plot(Petal.Length ~ Species, data = iris)
plot(Petal.Width ~ Species, data = iris)
plot(Sepal.Length ~ Species, data = iris)
plot(Sepal.Width ~ Species, data = iris)

plot(iris, col = iris$Species)

# PCA
pca = dudi.pca(iris[,1:4], scannf=FALSE, nf=2)
inertia.dudi(pca, row = FALSE, col = TRUE)

scatter(pca)
s.class(pca$li, iris$Species)



# Split the data into training (50%) and test set (50%)
set.seed(123)
training.samples <- iris$Species %>%
  createDataPartition(p = 0.5, list = FALSE)
train.data <- iris[training.samples, ]
test.data <- iris[-training.samples, ]


# Estimate preprocessing parameters
preproc.param <- train.data %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)


library(MASS)
model <- lda(Species~., data = train.transformed)
model

# LDA determines group means and computes, for each individual, the probability of belonging
# to the different groups. The individual is then affected to the group with the highest probability 
# score.
# 
# The lda() outputs contain the following elements:
#   
# Prior probabilities of groups: the proportion of training observations in each group. For example,
# there are 31% of the training observations in the setosa group
# Group means: group center of gravity. Shows the mean of each variable in each group.
# Coefficients of linear discriminants: Shows the linear combination of predictor variables 
# that are used to form the LDA decision rule. 
# for example, LD1 = 0.91*Sepal.Length + 0.64*Sepal.Width - 4.08*Petal.Length - 2.3*Petal.Width. 
# Similarly, LD2 = 0.03*Sepal.Length + 0.89*Sepal.Width - 2.2*Petal.Length - 2.6*Petal.Width.



plot(model)

predictions <- model %>% predict(test.transformed)
names(predictions)


# Predicted classes
head(predictions$class, 6)
# Predicted probabilities of class memebership.
head(predictions$posterior, 6) 
# Linear discriminants
head(predictions$x, 3) 


lda.data <- cbind(train.transformed, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Species))

  
mean(predictions$class==test.transformed$Species)

# Confusion Matrix
xtab <- table(predictions$class, test.transformed$Species)
xtab
confusionMatrix(xtab)


lda.data <- cbind(test.transformed, predict(model, test.transformed)$x, pred= predict(model, test.transformed)$class)
str(lda.data)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Species, pch = pred)) 
  

lda.data[lda.data$Species != lda.data$pred,]


