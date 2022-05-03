# Packages ----

# Install any packages that we will be using that aren't already installed
packages <- c('tidyverse', 'caret', 'corrplot', 'rpart', 'rpart.plot', 'scales', 'pcaPP')
install.packages(setdiff(packages, rownames(installed.packages())))
rm(packages)

# Load packages

library(tidyverse)
library(caret)
library(corrplot)
library(rpart)
library(rpart.plot)
library(scales)
library(pcaPP)

# Set seed ----

set.seed(2022, sample.kind = "Rounding")

# Read in ----

# This is the URL for the data that will be analyzed

urlfile <- "https://raw.githubusercontent.com/jasonhendrix9/IS4300-Final-Project/main/Dry_Bean_Dataset.csv"

# Read the data into R

df <- read_csv(url(urlfile))

# Make Class a factor ----

df$Class <- factor(df$Class)

class_index <- which(names(df) == 'Class') # Store the index of Class

# Split into train and test ----

# There's not a ton of data, so let's just do a 50/50 split for train and test

testIndex <- createDataPartition(df$Class, times = 1, p = 0.5, list = FALSE)

train1 <- df[-testIndex,]
test1 <- df[testIndex,]

# Scale ----

# Since we will be using KNN, which uses Euclidean distance, let's scale our variables

train1[-class_index] <- scale(train1[-class_index])
test1[-class_index] <- scale(test1[-class_index])

# rpart Tree ----

# Let's train our model

tree1 <- rpart(Class ~ ., data = train1)

# Now let's plot our tree

rpart.plot(tree1)

# Use the model to make predictions

y_pred_tree1 <- as.vector(predict(tree1, test1[-class_index], type = 'class'))

# Calculate the accuracy from the results

cm_tree <- table(Actual = test1$Class, Predicted = y_pred_tree1)
(cm_tree[1,1] + cm_tree[2,2] + cm_tree[3,3] + cm_tree[4,4] + cm_tree[5,5] + cm_tree[6,6] + cm_tree[7,7]) / sum(cm_tree)


# KNN 5 ----

# Train a model to look at the 5 nearest neighbors

knn_5 <- knn3(Class ~ ., data = train1, k = 5)

# Make predictions

y_pred_knn_5 <- predict(knn_5, test1, type = "class") %>%
  factor(levels = levels(train1$Class))

# Calculate accuracy

cm_knn_5 <- table(Actual = test1$Class, Predicted = y_pred_knn_5)

(cm_knn_5[1,1] + cm_knn_5[2,2] + cm_knn_5[3,3] + cm_knn_5[4,4] + cm_knn_5[5,5] + cm_knn_5[6,6] + cm_knn_5[7,7]) / sum(cm_knn_5)

# Best K ----

# F Score doesn't work since there are multiple factor levels to Class
# Let's just take a look and see what maximizes accuracy

kv <- seq(1, 101, 2)  # try neighbors 1, 3, 5, 7, 9, ..., 101

# Now wee will loop through all Ks in our list and get the most accurate one

bestK <- 1
bestK_accuracy <- 0
accuracies <- c()

for(i in kv)
{
  # Train the model
  
  knn_i <- knn3(Class ~ ., data = train1, k = i)
  
  # Make predictions
  
  y_pred_i <- predict(knn_i, test1, type = "class") %>%
    factor(levels = levels(train1$Class))
  
  # Calculate accuracy
  
  cm_i <- table(Actual = test1$Class, Predicted = y_pred_i)
  
  accuracy <- (cm_i[1,1] + cm_i[2,2] + cm_i[3,3] + cm_i[4,4] + cm_i[5,5] + cm_i[6,6] + cm_i[7,7]) / sum(cm_i)
  
  # If this is the best accuracy so far, store it
  
  if(accuracy > bestK_accuracy)
  {
    bestK <- i
    bestK_accuracy <- accuracy
  }
  
  # Keep a list of all accuracies so we can plot it later
  
  accuracies <- c(accuracies, accuracy)
}

# Plot the accuracies

ggplot() + 
  geom_line(mapping = aes(kv, accuracies)) +  # y = accuracy
  geom_vline(xintercept = bestK, # vertical line to mark the most accurate
             col = 'firebrick', 
             linetype = 'dashed') +
  geom_hline(yintercept = bestK_accuracy, # horizontal line to mark the highest accuracy
             col = 'firebrick', 
             linetype = 'dashed') +
  scale_x_continuous(breaks = c(seq(0, 100, 25), bestK)) + # label the vertical line with the value
  scale_y_continuous(labels = percent, # make the y axis percents
                     breaks = c(seq(0.90, 0.92, 0.005), round(bestK_accuracy, 3))) + # lable the horizontal line with the accuracy
  labs(title = 'KNN Accuracy',
       subtitle = 'With Varying Levels of K',
       x = 'K', 
       y = 'Accuracy') +
  theme_minimal()

# Turns out the best K is 15

# Train a new model with K = 15

knn_best1 <- knn3(Class ~ ., data = train1, k = bestK)

# Make predictions

y_pred_knn_best1 <- predict(knn_best1, test1, type = "class") %>%
  factor(levels = levels(train1$Class))

# Calculate Accuracy

cm_knn_best <- table(Actual = test1$Class, Predicted = y_pred_knn_best1)

(cm_knn_best[1,1] + cm_knn_best[2,2] + cm_knn_best[3,3] + cm_knn_best[4,4] + cm_knn_best[5,5] + cm_knn_best[6,6] + cm_knn_best[7,7]) / sum(cm_knn_best)

# Accuracy improved!

# Evaluation Plots ----

# 2 Most Important Variables to the first tree
# Sort the variables in order of descending importance and then return the top 2

names(head(sort(tree1$variable.importance, 
                decreasing = T), 2))

# Top two are Perimeter and MinorAxisLength

# Scatter Plot

ggplot() +
  geom_point(data = train1, 
             mapping = aes(x = Perimeter, y = MinorAxisLength, col = Class)) + # color points by Class
  labs(title = 'Bean Plot by Class (Training Set)',
       subtitle = 'Using Perimeter and Minor Axis Length',
       x = 'Perimeter',
       y = 'Minor Axis Length',
       color = 'Bean Class') + # rename legend title
  theme_minimal()

# Density Plot

ggplot() +
  geom_density2d(data = train1, 
                 mapping = aes(Perimeter, MinorAxisLength, col = Class)) + # color lines by Class
  labs(title = 'Bean Density by Class (Training Set)',
       subtitle = 'Using Perimeter and Minor Axis Length',
       x = 'Perimeter',
       y = 'Minor Axis Length',
       color = 'Bean Class') + # rename legend title
  theme_minimal()

# Points that were incorrectly predicted

wrong1 <- cbind.data.frame(test1$Perimeter, test1$MinorAxisLength, test1$Class, y_pred_knn_best1) %>% # combine variables, actual results, and predicted results
  rename(Perimeter = 1, # the numbers represent the column number - for example, 1 = test1$Perimeter
         MinorAxisLength = 2,
         Class = 3,
         Predicted = 4) %>% 
  filter(Class != Predicted) # only the incorrect predictions

ggplot() +
  geom_density2d(data = train1, 
                 mapping = aes(Perimeter, MinorAxisLength, col = Class)) + # color lines by class
  geom_point(data = wrong1,
             mapping = aes(Perimeter, MinorAxisLength), # scatter plot
             size = 0.25) + # make points small
  labs(title = 'Incorrect Bean Classification',
       subtitle = 'Density is Training Data and Points are Incorrect Predictions',
       x = 'Perimeter',
       y = 'Minor Axis Length',
       color = 'Bean Class') + # rename legend title
  theme_minimal()

# Correlation ----

# If two variables are highly correlated, they won't be independent. Since two dependent variables convey about the same information in this case, we will remove highly correlated variables and retrain the models

# Simpler > Slightly more accurate

# Most of the variables are not normal, so we should probably use Kendall instead of Pearson correlation
# We could use cor(..., method = 'kendall'), but that is really slow - O(n^2)
# Instead, let's use cor.fk - O(n log n)

# Let's make a plot

corrplot(cor.fk(as.data.frame(df[-class_index])), # correlation matrix
         tl.srt = 45) # rotate top 45º

# Let's remove all variables that have |corr| > 90% with any other variable
# The findCorrelation function does that for us

df.clean <- df[-findCorrelation(cor.fk(as.data.frame(df[-class_index])))]

# Find which column is the class column again

class_index <- which(names(df.clean) == 'Class')

# Replot our correlations

corrplot(cor.fk(as.data.frame(df.clean[-class_index])), 
         tl.srt = 45)

# They look much cleaner!

# Rerun! ----

# Split into train and test ----

train <- df.clean[-testIndex,]
test <- df.clean[testIndex,]

# Scale ----

train[-class_index] <- scale(train[-class_index])
test[-class_index] <- scale(test[-class_index])

# rpart ----

# Train a new model

tree <- rpart(Class ~ ., data = train)

# Plot the tree

rpart.plot(tree)

# Make predictions on the test set

y_pred_tree <- as.vector(predict(tree, test[-class_index], type = 'class'))

# Calculate accuracy

cm_tree <- table(Actual = test$Class, Predicted = y_pred_tree)
(cm_tree[1,1] + cm_tree[2,2] + cm_tree[3,3] + cm_tree[4,4] + cm_tree[5,5] + cm_tree[6,6] + cm_tree[7,7]) / sum(cm_tree)


# Best K ----

# F Score doesn't work since there are multiple outputs
# Let's just make a look and see what maximizes accuracy

kv <- seq(1, 101, 2)  # try neighbors 1, 3, 5, 7, 9, ..., 101

# Loop through the neighbors to find the K with the highest accuracy

bestK <- 1
bestK_accuracy <- 0
accuracies <- c()

for(i in kv)
{
  # Create the model
  
  knn_i <- knn3(Class ~ ., data = train, k = i)
  
  # Make predictions
  
  y_pred_i <- predict(knn_i, test, type = "class") %>%
    factor(levels = levels(train$Class))
  
  # Calculate Accuracy
  
  cm_i <- table(Actual = test$Class, Predicted = y_pred_i)
  
  accuracy <- (cm_i[1,1] + cm_i[2,2] + cm_i[3,3] + cm_i[4,4] + cm_i[5,5] + cm_i[6,6] + cm_i[7,7]) / sum(cm_i)
  
  # If the accuracy is the best so far, keep it
  
  if(accuracy > bestK_accuracy)
  {
    bestK <- i
    bestK_accuracy <- accuracy
  }
  
  # Store all accuracies to plot later
  
  accuracies <- c(accuracies, accuracy)
}

ggplot() + 
  geom_line(mapping = aes(kv, accuracies)) + # y = accuracy
  geom_vline(xintercept = bestK, # vertical line for the best K
             col = 'firebrick', 
             linetype = 'dashed') +
  geom_hline(yintercept = bestK_accuracy, # horizontal line for the highest accuracy
             col = 'firebrick', 
             linetype = 'dashed') +
  scale_x_continuous(breaks = c(seq(0, 100, 25), bestK)) + # label the best K
  scale_y_continuous(labels = percent, # make y axis be percents
                     breaks = c(seq(0.90, 0.92, 0.005), round(bestK_accuracy, 3))) + # label the highest accuracy
  labs(title = 'KNN Accuracy',
       subtitle = 'With Varying Levels of K',
       x = 'K', 
       y = 'Accuracy') +
  theme_minimal()

# Turns out the best K is 17

knn_best <- knn3(Class ~ ., data = train, k = bestK)

y_pred_knn_best <- predict(knn_best, test, type = "class") %>%
  factor(levels = levels(train$Class))

cm_knn_best <- table(Actual = test$Class, Predicted = y_pred_knn_best)

(cm_knn_best[1,1] + cm_knn_best[2,2] + cm_knn_best[3,3] + cm_knn_best[4,4] + cm_knn_best[5,5] + cm_knn_best[6,6] + cm_knn_best[7,7]) / sum(cm_knn_best)

# Very little accuracy was lost, and we have a much smaller model!

# Evaluation Plots ---- 

# 2 Most Important Variables for Tree 2
# Sort by descending importance and take the top two variables

names(head(sort(tree$variable.importance, decreasing = T), 2))

# Eccentricity and ShapeFactor2

# Scatter Plot

ggplot() +
  geom_point(data = train, 
             mapping = aes(Eccentricity, ShapeFactor2, col = Class)) + # color points by Class
  labs(title = 'Bean Plot by Class (Training Set)',
       subtitle = 'Using Eccentricity and Shape Factor 2',
       x = 'Eccentricity',
       y = 'Shape Factor 2',
       color = 'Bean Class') + # rename legend
  theme_minimal()

# Density Plot

ggplot() +
  geom_density2d(data = train, 
                 mapping = aes(Eccentricity, ShapeFactor2, col = Class)) + # color points by Class
  labs(title = 'Bean Plot by Class (Training Set)',
       subtitle = 'Using Eccentricity and Shape Factor 2',
       x = 'Eccentricity',
       y = 'Shape Factor 2',
       color = 'Bean Class') + # rename legend
  theme_minimal()

# Points that were incorrectly predicted

wrong <- cbind.data.frame(test$Eccentricity, test$ShapeFactor2, test$Class, y_pred_knn_best) %>% 
  rename(Eccentricity = 1,
         ShapeFactor2 = 2,
         Class = 3,
         Predicted = 4) %>% 
  filter(Class != Predicted)

ggplot() +
  geom_density2d(data = train, 
                 mapping = aes(Eccentricity, ShapeFactor2, col = Class)) + # color lines by Class
  geom_point(data = wrong,
             mapping = aes(Eccentricity, ShapeFactor2),
             size = 0.25) +
  labs(title = 'Incorrect Bean Classification',
       subtitle = 'Density is Training Data and Points are Incorrect Predictions',
       x = 'Eccentricity',
       y = 'Shape Factor 2',
       color = 'Bean Class') +
  theme_minimal()

# Most incorrect classifications occur between borders or near overlap
