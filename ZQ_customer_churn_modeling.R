# --------------------------------------Telco Customer Churn Project Implementation---------------------------
#----------------LOAD NEEDED LIBRARY AND PACKAGES-----------------------------------------------------------
#install.packages("e1071")
#install.packages("comprehenr")
#install.packages("data.table")
#install.packages(c("caret", "pROC", "plotROC"))

library(caret)
library(pROC)
library(plotROC)
library(rpart)
library(gains)
library(e1071)
library(comprehenr)
library(forecast)
library(ggplot2)
library(gplots)
library(reshape)
library(GGally)
library(MASS)
library(data.table)

# Task 1 & 2 Implementation: Data Exploration and Preprocessing
# extract df from csv
telco.df <- read.csv("Telco-Customer-Churn.csv")

# first look at the data and its content -AC
str(telco.df)

# Check if the data has any missing/NA values -AC
sapply(telco.df, function(x) sum(is.na(x)))

# There are 11 missing values in “TotalCharges” columns,
# so we will remove those values - AC
telco.df$lTotalCharges<-log(telco.df$TotalCharges)
telco.df <- na.omit(telco.df)

# customer ID is not important in the analysis of this dataset, so remove it
telco.df <- subset(telco.df, select = -customerID)

# change all cells that have "no phone service" or "no internet service" into "No" 
# because there are already column about that
# but they are not included in heatmap and ggpair for simplicity
no.ggpair=character(0)
for (col in names(telco.df)) {
  if (all(telco.df[[col]] %in% c("Yes", "No","No phone service"))) {
    telco.df[[col]][telco.df[[col]] == "No phone service"] <- "No"
    
  }
  if (any(telco.df[[col]] %in% c("No internet service","No phone service"))) {
    no.ggpair=c(no.ggpair,col)
  }
}

for (col in names(telco.df)) {
  if (all(telco.df[[col]] %in% c("Yes", "No","No internet service"))) {
    telco.df[[col]][telco.df[[col]] == "No internet service"] <- "No"
  }
  if (any(telco.df[[col]] %in% c("No internet service","No phone service"))) {
    no.ggpair=c(no.ggpair,col)
  }
}


# Convert column that have values with just Yes and No into 1 and 0
for (col in names(telco.df)) {
  if (all(telco.df[[col]] %in% c("Yes", "No"))) {
    telco.df[[col]] <- ifelse(telco.df[[col]] == "Yes", 1, 0)
  }
}

# construct 2 empty dataframes for continuous and categorical data
con.telco.df <- data.frame(row.names = c(1:nrow(telco.df)))
cat.telco.df <- data.frame(row.names = c(1:nrow(telco.df)))

# All the continuous variables in telco.df are numeric and have at least 5
# in value of length(unique(column data)) by observing the csv file.
# Assign them to 2 different data frames for calculating their stats.
for (i in names(telco.df)) {
  if (is.numeric(telco.df[, i])) {
    if (length(unique(telco.df[, i])) >= 6) {
      con.telco.df[, i] <- telco.df[, i]
    } else {
      cat.telco.df[, i] <- telco.df[, i]
    }
  } else {
    cat.telco.df[, i] <- telco.df[, i]
  }
}


# Prepare box.df for boxplot in below loop
box.df <- con.telco.df
box.df$Churn <- cat.telco.df$Churn

box.df$Churn <- ifelse(box.df$Churn == 1, "Yes", "No")

# Summary stat for continuous variable (Q1)
con.stat <- data.frame(row.names = (names(con.telco.df)))
con.stat["Min"] <- NA
con.stat["First Quartile"] <- NA
con.stat["Median"] <- NA
con.stat["Third Quartile"] <- NA
con.stat["Max"] <- NA
con.stat["Mean"] <- NA
con.stat["Stdev"] <- NA
con.stat["Skewness"] <- NA
for (i in names(con.telco.df)) {
  print(paste("============================================"))
  print(paste("Visualization for continuous variable ", i))
  con.stat[i, 1] <- min(con.telco.df[, i], na.rm = TRUE)
  con.stat[i, 2] <- quantile(con.telco.df[, i], 0.25, na.rm = TRUE)
  con.stat[i, 3] <- median(con.telco.df[, i], na.rm = TRUE)
  con.stat[i, 4] <- quantile(con.telco.df[, i], 0.75, na.rm = TRUE)
  con.stat[i, 5] <- max(con.telco.df[, i], na.rm = TRUE)
  con.stat[i, 6] <- mean(con.telco.df[, i], na.rm = TRUE)
  con.stat[i, 7] <- sd(con.telco.df[, i], na.rm = TRUE)
  con.stat[i, 8] <- skewness(con.telco.df[, i], type = 2, na.rm = TRUE)
  hist(con.telco.df[, i],
       breaks = "FD", xlab = paste("Number of ", i),
       main = paste("Histogram of ", i)
  )
  boxplot(box.df[, i] ~ box.df$Churn,
          xlab = "Churn", ylab = i,
          names = c("No", "Yes"),
          main = paste("Barplot of ", i)
  )
}
con.stat <- round(con.stat, digits = 3)
con.stat


# Summary stat for categorical variable (Q1)
# First convert cat.telco.df 1 and 0 back into "yes" and "no"
for (col in names(cat.telco.df)) {
  if (all(cat.telco.df[[col]] %in% c(1, 0))) {
    cat.telco.df[[col]] <- ifelse(cat.telco.df[[col]] == 1, "Yes", "No")
  }
}

# Then display categorical variable visualization and frequency
for (i in names(cat.telco.df)) {
  print(paste("============================================"))
  print(paste("Below is the frequency distribution of", i))
  selected.column <- cat.telco.df[, i]
  selected.column.freq <- data.frame(table(selected.column))
  names(selected.column.freq)[1] <- "Type"
  names(selected.column.freq)[2] <- "Frequency"
  selected.column.freq <- selected.column.freq[order(-selected.column.freq$Frequency), ]
  print(selected.column.freq)
  print(paste("Below is the relative frequency distribution of ", i))
  selected.column.freq$Rel.Freq <- selected.column.freq$Frequency / sum(selected.column.freq$Frequency)
  selected.column.freq$Percent.Freq <- selected.column.freq$Rel.Freq * 100
  print(selected.column.freq[, -2])
  sum.cat <- length(unique(cat.telco.df[, i])) # Sum of category in a column
  print(paste("Below is the bar plot of ", i))
  # Display minimum of (sum of category or 8) bars in the bar chart
  barplot(selected.column.freq$Frequency[1:min(8, sum.cat)],
          names.arg = selected.column.freq$Type[1:min(8, sum.cat)],
          xlab = paste("Categorical value of", i),
          ylab = paste("Frequency of", i),
          legend = TRUE,
          main = paste("Bar Chart of", i)
          
  )
  if (i=="PaperlessBilling"){  # this categorical variable having too many unique value
    par(cex.axis = 0.7)
  }else{
    par(cex.axis = 1)
  }
  
  
}

# create a df for numerical variable
num.telco.df <- data.frame(row.names = c(1:nrow(telco.df)))
for (i in names(telco.df)) {
  if (is.numeric(telco.df[, i])) {
    num.telco.df[, i] <- telco.df[, i]
  }
}




# Make heatmap
heatmap.2(cor(num.telco.df),
          Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(num.telco.df), 2), notecol = "black",
          key = FALSE, trace = "none", margins = c(8, 8)
)

# remove the columns on the vector no.ggpair from num.telco.df
num.telco.df <- num.telco.df[, !names(num.telco.df) %in% no.ggpair]

# Do ggpairs
# ggpairs(num.telco.df)
num.telco.df$Churn <- ifelse(num.telco.df$Churn == 1, "Yes", "No")
ggpairs(num.telco.df, aes(color = forcats::fct_rev(as.factor(num.telco.df$Churn))),
        diag = list(
          continuous = wrap("densityDiag", alpha = 0.6),
          discrete = wrap("barDiag", alpha = 0.7, color = "red")
        )
)+theme(text = element_text(size = 8),
        axis.text = element_text(size = 8),
        strip.text = element_text(size = 8),
        legend.text = element_text(size = 8))

# con.stat.churn <- telco.df$Churn
# con.telco.df_new <- cbind(con.telco.df, con.stat.churn ) # combine Churn
# names(con.telco.df_new)[names(con.telco.df_new) == "con.stat.churn"] <- "Churn" ## rename column to Churn
# con.telco.df_new$Churn <- as.factor(con.telco.df_new$Churn)
#
# ggpairs(con.telco.df, aes(color=forcats::fct_rev(Churn)),
#         diag = list(continuous = wrap("densityDiag", alpha = 0.6),
#                     discrete = wrap("barDiag", alpha = 0.7, color="red")))

# Do Parallel coordinates plot
# the plot will be too ugly if there are too many data in it
# a visualization of sampled data could give clearer graph
set.seed(123)
num.telco.df$Churn <- ifelse(num.telco.df$Churn == "Yes", 1, 0)
train <- sample(nrow(num.telco.df), nrow(num.telco.df) / 10)
parcoord(num.telco.df[train, ],
         var.label = TRUE,
         col = "grey"
)

# Remove outliers
outlier.row.num <- c()
for (i in names(con.telco.df)) {
  lower.bound <- quantile(con.telco.df[, i], p = 0.25, na.rm = TRUE) -
    1.5 * IQR(con.telco.df[, i], na.rm = TRUE)
  upper.bound <- quantile(con.telco.df[, i], p = 0.75, na.rm = TRUE) +
    1.5 * IQR(con.telco.df[, i], na.rm = TRUE)
  for (j in (1:nrow(con.telco.df))) {
    if (!is.na(con.telco.df[j, i])) {
      if (con.telco.df[j, i] < lower.bound | con.telco.df[j, i] > upper.bound &
          (upper.bound != 0)) {
        if (!j %in% outlier.row.num) {
          outlier.row.num <- append(outlier.row.num, j)
        }
      }
    }
  }
}


if (length(outlier.row.num) > 0) {
  telco.clean <- telco.df[-outlier.row.num,]
}else{
  telco.clean <- telco.df
}


# Task 3 Implementation: Data Dimension Reduction & Feature Selection
# do backward stepwise elimination for reducing dimension

formula.str<-paste("Churn", "~", paste(setdiff(names(telco.clean), "Churn"), collapse = "+"))
premodel<-(glm(formula.str,data=telco.clean,family="binomial"))
premodel <- step(premodel, direction = "backward")
premodel
# from the result, subset the variables
telco.reduced <- telco.clean[, c("Churn", "SeniorCitizen", "MultipleLines", "InternetService",
                                 "OnlineSecurity", "DeviceProtection", "TechSupport", "StreamingTV",
                                 "StreamingMovies", "Contract", "PaperlessBilling", "PaymentMethod",
                                 "MonthlyCharges", "lTotalCharges")]


#----------------Task 4 & 5 Implementation: Data Partition & 10-Fold Implementation & Model Building---------------#

# 10 fold cv
set.seed(123)

telco.duplicate <- telco.reduced
n_folds <- 10
n_rows <- nrow(telco.duplicate)
fold_size <- n_rows %/% n_folds
remainder <- n_rows %% n_folds

train_indices_list <- vector("list", n_folds)

# Distribute remainder among the first few folds
for (i in 1:n_folds) {
  current_fold_size <- fold_size + (i <= remainder)
  train_indices_list[[i]] <- sample(n_rows, current_fold_size)
  n_rows <- n_rows - current_fold_size
}

#----------Initialize empty data.tables for pooling Logistic Regression model predictions------------
total_predicted_train <- data.table()
total_predicted_valid <- data.table()
total_actual_train <- data.table()


#----------Initialize empty data.tables for pooling CART model predictions--------------------------
total_predicted_train_cart <- data.table()
total_predicted_valid_cart <- data.table()


for (i in 1:n_folds) {
  valid_indices <- train_indices_list[[i]]
  train_indices <- setdiff(1:nrow(telco.reduced), valid_indices)
  
  #-----------------Fit Logistic Regression Model and Apply to training and validation sets---------------------
  train.model <- glm(Churn ~ ., data = telco.reduced[train_indices, ], family = "binomial")
  predicted.valid <- data.table(predict(train.model, telco.reduced[valid_indices, ]))
  predicted.train <- data.table(predict(train.model, telco.reduced[train_indices, ]))
  
  #-----Bind and Pool rows cumulatively for Logistic Regression predicted_train and predicted_valid-------------
  total_predicted_valid <- rbind(total_predicted_valid, predicted.valid)
  total_predicted_train <- rbind(total_predicted_train, predicted.train)
  total_actual_train <- rbind(total_actual_train, telco.reduced[train_indices, "Churn"])
  
  
  #---------------Fit the CART model and apply to training and validation sets----------------------------------
  train.model.cart<-rpart(Churn ~ .,  data=telco.reduced[train_indices,], method = "class")
  predicted.valid.cart <- data.table(predict(train.model.cart, telco.reduced[valid_indices, ], type = "class"))
  predicted.train.cart <- data.table(predict(train.model.cart, telco.reduced[train_indices, ], type = "class"))
  
  #-----Bind and Pool rows cumulatively for CART predicted_train and predicted_valid---------------------------
  total_predicted_valid_cart <- rbind(total_predicted_valid_cart, predicted.valid.cart)
  total_predicted_train_cart <- rbind(total_predicted_train_cart, predicted.train.cart)
  
}
#----renaming column name for pooled actual values---------------------------------------------
colnames(total_actual_train)[colnames(total_actual_train) == "x"] <- "Churn"


#-----------TASK 6 :MODEL EVALUATION METRICS CALCULATION FOR LOGISTIC REGRESSION MODEL----------------

# Convert probabilities to binary predictions using a threshold of 0.5
predicted_binary_train <- ifelse(total_predicted_train >= 0.5, 1, 0)
predicted_binary_train <- factor(predicted_binary_train, levels = c(1, 0))
actual_train_churn <- factor(total_actual_train$Churn, levels = c(1, 0))

# Calculate confusion matrix for pooled training prediction
conf_matrix_train <- confusionMatrix(predicted_binary_train, actual_train_churn)
conf_matrix_train

# Convert probabilities to binary predictions using a threshold of 0.5
predicted_binary <- ifelse(total_predicted_valid >= 0.5, 1, 0)

# Calculate confusion matrix for pooled validation prediction
conf_matrix_valid <- confusionMatrix(factor(predicted_binary, levels = c(1, 0)),  factor(telco.reduced$Churn, levels = c(1, 0)))
conf_matrix_valid


#--------------------------------Print Metrics for LOGISTIC REGRESSION training sets---------------------------------------#
#  Printing six core metrics Accuracy, Sensitivity,  Specificity,  Precision, FDR, FOR 
conf_matrix_train <- confusionMatrix(predicted_binary_train, actual_train_churn)


# Extract confusion matrix elements
tp <- conf_matrix_train$table[1, 1]
fn <- conf_matrix_train$table[2, 1]
fp <- conf_matrix_train$table[1, 2]
tn <- conf_matrix_train$table[2, 2]

# Calculate metrics
accuracy <- (tp + tn) / (tp + tn + fp + fn)
sensitivity <- tp / (tp + fn)
specificity <- tn / (tn + fp)
precision <- tp / (tp + fp)
fdr <- fp / (fp + tp)
f.o.r <- fn / (fn + tn)

# Create a dataframe to store the metrics
metrics_df <- data.frame(
  Accuracy = accuracy,
  Sensitivity = sensitivity,
  Specificity = specificity,
  Precision = precision,
  FDR = fdr,
  FOR = f.o.r
)

metrics_df
#---------------------------------------------------------------------------------------------------------------#

#--------------------------------Print Metrics for LOGISTIC REGRESSION Validation sets---------------------------------------#
#  Printing six core metrics Accuracy, Sensitivity,  Specificity,  Precision, FDR, FOR 
conf_matrix_valid <- confusionMatrix(factor(predicted_binary, levels = c(1, 0)),  factor(telco.reduced$Churn, levels = c(1, 0)))

# Extract confusion matrix elements
tp <- conf_matrix_valid$table[1, 1]
fn <- conf_matrix_valid$table[2, 1]
fp <- conf_matrix_valid$table[1, 2]
tn <- conf_matrix_valid$table[2, 2]

# Calculate metrics
accuracy <- (tp + tn) / (tp + tn + fp + fn)
sensitivity <- tp / (tp + fn)
specificity <- tn / (tn + fp)
precision <- tp / (tp + fp)
fdr <- fp / (fp + tp)
f.o.r <- fn / (fn + tn)

# Create a dataframe to store the metrics
metrics_df <- data.frame(
  Accuracy = accuracy,
  Sensitivity = sensitivity,
  Specificity = specificity,
  Precision = precision,
  FDR = fdr,
  FOR = f.o.r
)

metrics_df

#------------------------------------------other Metrics LOGISTIC REGRESSION MODEL--------------------------------------#
# Calculate ROC curve and AUC for pooled training prediction
roc_obj <- roc(as.factor(total_actual_train$Churn), as.numeric(total_predicted_train$V1))
auc <- auc(roc_obj)

# ROC curve and AUC for pooled validation prediction
roc_obj1 <- roc(as.factor(telco.reduced$Churn), as.numeric(total_predicted_valid$V1))
auc1 <- auc(roc_obj1)

# Plot ROC curve for pooled training prediction
plot(roc_obj, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")

# Plot ROC curve for pooled validation prediction
plot(roc_obj1, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")

# Calculate and plot Lift chart for pooled training prediction
# Convert total_actual_train$Churn to a factor variable
total_actual_train$Churn <- as.factor(total_actual_train$Churn)

# Create a formula for lift calculation
formula_lift <- as.formula(paste("Churn ~ as.numeric(total_predicted_train$V1)"))

# Calculate and plot Lift chart
lift_obj <- lift(formula_lift, data = total_actual_train)
plot(lift_obj, main = "Lift Chart")

# Calculate and plot Lift chart for pooled validation prediction
# Convert total_actual_train$Churn to a factor variable
telco.reduced$Churn <- as.factor(telco.reduced$Churn)

# Create a formula for lift calculation
formula_lift1 <- as.formula(paste("Churn ~ as.numeric(total_predicted_valid$V1)"))

# Calculate and plot Lift chart
lift_obj1 <- lift(formula_lift1, data = total_actual_train)
plot(lift_obj, main = "Lift Chart")

#-------------TASK 6 :MODEL EVALUATION METRICS CALCULATION FOR CART MODEL-----------------------------------------

#-----------------------Training prediction CONFUSION MATRIX------------------------------------------------------
total_actual <- total_actual_train$Churn

# Convert V1 column in total_predicted_train to a factor
total_predicted_train_cart$V1 <- as.factor(total_predicted_train_cart$V1)

# Convert total_actual to a factor
total_actual <- as.factor(total_actual)

# Reverse the order of levels in total_predicted_valid$V1
total_predicted_train_cart$V1 <- factor(total_predicted_train_cart$V1, levels = c("1", "0"))
total_actual <- factor(total_actual, levels = c("1", "0"))

# Create the confusion matrix for pooled prediction
cm_cart <- confusionMatrix(total_predicted_train_cart$V1, total_actual)
cm_cart

#--------------------------Validation prediction CONFUSION MATRIX---------------------------------------------
total.real <- telco.reduced$Churn

# Convert V1 column in total_predicted_train to a factor
total_predicted_valid_cart$V1 <- as.factor(total_predicted_valid_cart$V1)

# Convert total_actual to a factor
total.real <- as.factor(total.real)
# Reverse the order of levels in total_predicted_valid$V1
total_predicted_valid_cart$V1 <- factor(total_predicted_valid_cart$V1, levels = c("1", "0"))
total.real <- factor(total.real, levels = c("1", "0"))

# Create the confusion matrix
cm.cart.valid <- confusionMatrix(total_predicted_valid_cart$V1, total.real)
cm.cart.valid

#-------------------------------- Print  metrics CART Training prediction sets----------------------------------------------
# print six core metrics Accuracy, Sensitivity,  Specificity,  Precision, FDR, FOR 
cm_cart <- confusionMatrix(as.factor(total_predicted_train_cart$V1), as.factor(total_actual))

# Extract confusion matrix elements
tp <- cm_cart$table[1, 1]
fn <- cm_cart$table[2, 1]
fp <- cm_cart$table[1, 2]
tn <- cm_cart$table[2, 2]

# Calculate metrics
accuracy <- (tp + tn) / (tp + tn + fp + fn)
sensitivity <- tp / (tp + fn)
specificity <- tn / (tn + fp)
precision <- tp / (tp + fp)
fdr <- fp / (fp + tp)
f.o.r <- fn / (fn + tn)

# Create a dataframe to store the metrics
metrics_df <- data.frame(
  Accuracy = accuracy,
  Sensitivity = sensitivity,
  Specificity = specificity,
  Precision = precision,
  FDR = fdr,
  FOR = f.o.r
)

metrics_df


#-------------------------------------------------------------------------------------------------------------
#-------------------------------- Print performance metrics CART Validation prediction sets--------------------------------------
#  Printing six core metrics Accuracy, Sensitivity,  Specificity,  Precision, FDR, FOR 
cm.cart.valid <- confusionMatrix(total_predicted_valid_cart$V1, total.real)

# Extract confusion matrix elements
tp <- cm.cart.valid$table[1, 1]
fn <- cm.cart.valid$table[2, 1]
fp <- cm.cart.valid$table[1, 2]
tn <- cm.cart.valid$table[2, 2]

# Calculate metrics
accuracy <- (tp + tn) / (tp + tn + fp + fn)
sensitivity <- tp / (tp + fn)
specificity <- tn / (tn + fp)
precision <- tp / (tp + fp)
fdr <- fp / (fp + tp)
f.o.r <- fn / (fn + tn)

# Create a data frame to store the metrics
metrics_df <- data.frame(
  Accuracy = accuracy,
  Sensitivity = sensitivity,
  Specificity = specificity,
  Precision = precision,
  FDR = fdr,
  FOR = f.o.r
)

metrics_df

#-----------------------------------------------other performance metrics CART Model-------------------------------------------#
# Calculate ROC curve and AUC for pooled training prediction
roc.obj.cart <- roc(as.factor(total_actual_train$Churn), as.numeric(total_predicted_train_cart$V1))
auc <- auc(roc.obj.cart)

# ROC curve and AUC for pooled validation prediction
roc.obj.valid <- roc(as.factor(telco.reduced$Churn), as.numeric(total_predicted_valid_cart$V1))
auc1 <- auc(roc.obj.valid)

# Plot ROC curve for pooled training prediction
plot(roc.obj.cart, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")

# Plot ROC curve for pooled validation prediction
plot(roc.obj.valid, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")

# Calculate and plot Lift chart for pooled training prediction
# Convert total_actual_train$Churn to a factor variable
total_actual_train$Churn <- as.factor(total_actual_train$Churn)

# Create a formula for lift calculation
formula_lift <- as.formula(paste("Churn ~ as.numeric(total_predicted_train_cart$V1)"))

# Calculate and plot Lift chart
lift_obj <- lift(formula_lift, data = total_actual_train)
plot(lift_obj, main = "Lift Chart")

# Calculate and plot Lift chart for pooled validation prediction
# Convert total_actual_train$Churn to a factor variable
telco.reduced$Churn <- as.factor(telco.reduced$Churn)

# Create a formula for lift calculation
formula_lift1 <- as.formula(paste("Churn ~ as.numeric(total_predicted_valid_cart$V1)"))

# Calculate and plot Lift chart
lift_obj1 <- lift(formula_lift1, data = total_actual_train)
plot(lift_obj, main = "Lift Chart")


#--------------------------------------------Task 7 Implementation---------------------------------------------------

# Building a logistic regression model using the whole dataset.

final.model <- glm(Churn ~ ., data = telco.reduced, family = "binomial")
options(scipen = 999)
summary(final.model)

#------------------------------------------------The-End------------------------------------------------------------



