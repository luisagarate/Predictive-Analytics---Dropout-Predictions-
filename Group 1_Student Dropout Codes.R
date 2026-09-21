# ============================================================
# 1. Setup: Load libraries and data
# ============================================================

# Load required libraries
library(dplyr)
library(psych)
library(ggplot2)
library(tidyr)
library(rpart)
library(rpart.plot)
library(pROC)

# Load the data
df <- read.csv("data_clean_predictive.csv", check.names = FALSE)

# Preview
head(df)
dim(df)





# ============================================================
# 2. Data Preparation: Filter and recode variables
# ============================================================

# Filter OUT the "Enrolled" group
df_filtered <- df %>%
  filter(Target != "Enrolled")

# Check remaining groups
table(df_filtered$Target)
cat("Rows before filtering:", nrow(df), "\n")
cat("Rows after filtering: ", nrow(df_filtered), "\n")

# Recode variables for readable labels
df_filtered <- df_filtered %>%
  mutate(
    Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male")),
    
    Displaced = factor(Displaced, levels = c(0, 1), labels = c("Not Displaced", "Displaced")),
    
    `Daytime/evening attendance\t` = factor(`Daytime/evening attendance\t`,
                                            levels = c(0, 1),
                                            labels = c("Evening", "Daytime")),
    Target = factor(Target, levels = c("Dropout", "Graduate"))
  )





# ============================================================
# 3. Descriptive Analysis: Summary statistics and plots
# ============================================================

# Mean & Median of Age at Enrollment
age_stats <- df_filtered %>%
  group_by(Target) %>%
  summarise(
    Count  = n(),
    Mean   = round(mean(`Age at enrollment`, na.rm = TRUE), 2),
    Median = median(`Age at enrollment`, na.rm = TRUE),
    SD     = round(sd(`Age at enrollment`, na.rm = TRUE), 2),
    Min    = min(`Age at enrollment`, na.rm = TRUE),
    Max    = max(`Age at enrollment`, na.rm = TRUE)
  )

print(age_stats)

# Bar Chart: Dropout vs Graduate Counts
count_data <- df_filtered %>%
  count(Target)

ggplot(count_data, aes(x = Target, y = n, fill = Target)) +
  geom_col(width = 0.6, color = "transparent") +
  geom_text(aes(label = format(n, big.mark = ",")),
            vjust = -0.5,
            size = 5) +
  scale_fill_manual(values = c("Dropout" = "#bca37f",
                               "Graduate" = "#414e4b")) +
  labs(title = "Numbers of Dropout vs Graduate Students",
       x = "Student Category",
       y = "Numbers of Student",
       fill = "") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    axis.title = element_text(face = "bold")
  ) +
  ylim(0, max(count_data$n) * 1.15)

# Boxplot of Age at Enrollment
ggplot(df_filtered, aes(x = Target, y = `Age at enrollment`, fill = Target)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "grey50") +

  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  
  stat_summary(fun = mean, geom = "text",
               aes(label = paste("Mean:", round(after_stat(y), 1))),
               vjust = -1.2, hjust = -0.1, size = 4, color = "black") +

  stat_summary(fun = median, geom = "text",
               aes(label = paste("Median:", round(after_stat(y), 1))),
               vjust = 2, hjust = -0.1, size = 4, color = "black") +
  
  scale_fill_manual(values = c("Dropout" = "#bca37f", "Graduate" = "#414e4b")) +
  labs(title = "Age at Enrollment: Dropout vs Graduate",
       x = "Group", y = "Age at Enrollment") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# Gender distribution
gender_dist <- df_filtered %>%
  count(Gender) %>%
  mutate(Percentage = paste0(round(n / sum(n) * 100, 1), "%")) %>%
  rename(Count = n)

print(gender_dist)

# Attendance distribution
attendance_dist <- df_filtered %>%
  count(`Daytime/evening attendance\t`) %>%
  mutate(Percentage = paste0(round(n / sum(n) * 100, 1), "%")) %>%
  rename(`Attendance Type` = `Daytime/evening attendance\t`, Count = n)

print(attendance_dist)

# Gender vs Target
gender_target <- df_filtered %>%
  count(Gender, Target) %>%
  group_by(Gender) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1),
         Label = paste0(n, "\n(", Percentage, "%)"))

print(gender_target)

# Stacked 100% Bar Chart
p2 <- ggplot(gender_target, aes(x = Gender, y = n, fill = Target)) +
  geom_bar(stat = "identity", position = "fill", width = 0.5) +
  geom_text(aes(label = paste0(Percentage, "%")),
            position = position_fill(vjust = 0.5),
            size = 4.5, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("Dropout" = "#bca37f", "Graduate" = "#414e4b")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Dropout vs Graduate by Gender",
       subtitle = "Proportional view within each gender group",
       x = "Gender", y = "Proportion", fill = "Target") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "grey50"))
p2





# ============================================================
# 4. Modeling Data Preparation: Train/Test split
# ============================================================

# Reload original data (kept exactly as your logic)
dat <- read.csv("data_clean_predictive.csv")

dat1 <- dat[dat$Target != "Enrolled", ]
dat_e <- dat[dat$Target == "Enrolled", ]

dat1$Target <- factor(dat1$Target,levels = c("Dropout", "Graduate"),
                      labels =c(0,1))
dat1$Target <- as.numeric(dat1$Target)-1

# Split data
sub.drop <- subset(dat1, dat1$Target == 0)
sub.grad <- subset(dat1, dat1$Target == 1)

set.seed(112233)

train.drop <- sample(1:nrow(sub.drop),710)
train.grad <- sample(1:nrow(sub.grad), 1104)

dat1.tr <- rbind(sub.drop[train.drop,],
                 sub.grad[train.grad,])

dat1.tst <- rbind(sub.drop[-train.drop,],
                  sub.grad[-train.grad,])





# ============================================================
# 5. Feature Subsets
# ============================================================

# Define variable groups
indir_vars <- c("Target", "Marital.status", "Application.mode", "Application.order",
                "Daytime.evening.attendance.", "Nacionality", 
                "Mother.s.qualification", "Father.s.qualification",
                "Mother.s.occupation", "Father.s.occupation",
                "Displaced", "Debtor", "Gender", "Age.at.enrollment",
                "International", "Unemployment.rate", "Inflation.rate", "GDP")

demo_vars <- c("Target", "Marital.status", "Nacionality", 
               "Displaced", "Gender", "Age.at.enrollment",
               "International")

acc_vars <- c("Target","Application.mode", "Application.order",
              "Daytime.evening.attendance.")

soc_vars <- c("Target", "Mother.s.qualification", "Father.s.qualification",
              "Mother.s.occupation", "Father.s.occupation", "Debtor")

econ_vars <- c("Target", "Unemployment.rate", "Inflation.rate", "GDP")

# Create subsets
dat1.tr.ind <- dat1.tr[, indir_vars]
dat1.tr.dem <- dat1.tr[, demo_vars]
dat1.tr.acc <- dat1.tr[, acc_vars]
dat1.tr.soc <- dat1.tr[, soc_vars]
dat1.tr.eco <- dat1.tr[, econ_vars]

dat1.tst.ind <- dat1.tst[, indir_vars]
dat1.tst.dem <- dat1.tst[, demo_vars]
dat1.tst.acc <- dat1.tst[, acc_vars]
dat1.tst.soc <- dat1.tst[, soc_vars]
dat1.tst.eco <- dat1.tst[, econ_vars]





# ============================================================
# 6. Predictive Modeling: Logistic Regression
# ============================================================

## Subset 1: Indirect Variables
lr.tr.ind <- glm(Target ~ . , data = dat1.tr.ind, family = "binomial")
sum.lr.tr.ind <- summary(lr.tr.ind)
sum.lr.tr.ind

DevRed.ind <- 1 - (lr.tr.ind$deviance / lr.tr.ind$null.deviance)
DevRed.ind


## Subset 2: Demographic
lr.tr.dem <- glm(Target ~ . , data = dat1.tr.dem, family = "binomial")
sum.lr.tr.dem <- summary(lr.tr.dem)
sum.lr.tr.dem

DevRed.dem <- 1 - (lr.tr.dem$deviance / lr.tr.dem$null.deviance)
DevRed.dem


## Subset 3: Academic
lr.tr.acc <- glm(Target ~ . , data = dat1.tr.acc, family = "binomial")
sum.lr.tr.acc <- summary(lr.tr.acc)
sum.lr.tr.acc

DevRed.acc <- 1 - (lr.tr.acc$deviance / lr.tr.acc$null.deviance)
DevRed.acc


## Subset 4: Socioeconomic
lr.tr.soc <- glm(Target ~ . , data = dat1.tr.soc, family = "binomial")
sum.lr.tr.soc <- summary(lr.tr.soc)
sum.lr.tr.soc

DevRed.soc <- 1 - (lr.tr.soc$deviance / lr.tr.soc$null.deviance)
DevRed.soc


## Subset 5: Macroeconomic
lr.tr.eco <- glm(Target ~ . , data = dat1.tr.eco, family = "binomial")
sum.lr.tr.eco <- summary(lr.tr.eco)
sum.lr.tr.eco

DevRed.eco <- 1 - (lr.tr.eco$deviance / lr.tr.eco$null.deviance)
DevRed.eco





# ============================================================
# 7. Predictive Modeling: Decision Tree
# ============================================================

## Demographic Tree
tree.dem <- rpart(Target ~ ., data = dat1.tr.dem, method = "class")
rpart.plot(tree.dem, type =2, extra=104,
           fallen.leaves = TRUE, 
           box.palette = "Browns", 
           main ="Decision Tree: Demographic")

## Socioeconomic Tree
tree.soc <- rpart(Target ~ ., data = dat1.tr.soc, method = "class")
rpart.plot(tree.soc, type=2, extra=104,
           fallen.leaves = TRUE, 
           box.palette = "Browns", 
           main ="Decision Tree: Socioeconomic")

## Academic Tree
tree.acc <- rpart(Target ~ ., data = dat1.tr.acc, method = "class")
rpart.plot(tree.acc, type=2, extra=104,
           fallen.leaves = TRUE,
           box.palette= "Browns",
           main ="Decision Tree: Academic")

## Macroeconomic Tree
tree.eco <- rpart(Target ~ ., data = dat1.tr.eco, method = "class")
rpart.plot(tree.eco, type=2, extra=104,
           fallen.leaves = TRUE,
           box.palette = "Browns",
           main ="Decision Tree: Macroeconomic")

# Evaluate prediction accuracy on test set

## Demographic
pred.dem <- predict(tree.dem, newdata = dat1.tst.dem, type = "class")
mean(pred.dem == dat1.tst.dem$Target)

## Socioeconomic
pred.soc <- predict(tree.soc, newdata = dat1.tst.soc, type = "class")
mean(pred.soc == dat1.tst.soc$Target)

## Academic
pred.acc <- predict(tree.acc, newdata = dat1.tst.acc, type = "class")
mean(pred.acc == dat1.tst.acc$Target)

## Macroeconomic
pred.eco <- predict(tree.eco, newdata = dat1.tst.eco, type = "class")
mean(pred.eco == dat1.tst.eco$Target)

tree.ind <- rpart(Target ~ ., 
                  data = dat1.tr.ind,
                  method = "class")

rpart.plot(tree.ind,
           type = 2,
           extra = 104,
           fallen.leaves = TRUE,
           box.palette = "Browns",
           main=  "Decision Tree: Combined Model (All Variables)")

pred.ind <- predict(tree.ind,
                    newdata = dat1.tst.ind,
                    type = "class")
mean(pred.ind == dat1.tst.ind$Target)





# ============================================================
# 8. Model Evaluation and Comparison
# ============================================================

# Logistic Regression Prediction on Test Data
prob.lr.tst <- predict(lr.tr.ind,
                       newdata = dat1.tst.ind,
                       type = "response")
pred.lr.tst <- ifelse(prob.lr.tst > 0.5, 1, 0)

## Logistic Regression Accuracy
acc.lr <- mean(pred.lr.tst == dat1.tst.ind$Target)
acc.lr
## Decision Tree Accuracy
acc.tree <- mean(pred.ind == dat1.tst.ind$Target)
acc.tree

# Confusion Matrices
## Logistic Regression Confusion Matrix
cm.lr <- table(Predicted = pred.lr.tst,
               Actual = dat1.tst.ind$Target)
cm.lr

## Decision Tree Confusion Matrix
cm.tree <- table(Predicted = pred.ind,
                 Actual = dat1.tst.ind$Target)
cm.tree

# Precision, Recall, F1 Score
## Logistic Regression metrics
TP.lr <- cm.lr["1","1"]
TN.lr <- cm.lr["0","0"]
FP.lr <- cm.lr["1","0"]
FN.lr <- cm.lr["0","1"]

precision.lr <- TP.lr / (TP.lr + FP.lr)
recall.lr <- TP.lr / (TP.lr + FN.lr)
f1.lr <- 2 * (precision.lr * recall.lr) / (precision.lr + recall.lr)

precision.lr
recall.lr
f1.lr

## Decision Tree metrics
TP.tree <- cm.tree["1","1"]
TN.tree <- cm.tree["0","0"]
FP.tree <- cm.tree["1","0"]
FN.tree <- cm.tree["0","1"]

precision.tree <- TP.tree / (TP.tree + FP.tree)
recall.tree <- TP.tree / (TP.tree + FN.tree)
f1.tree <- 2 * (precision.tree * recall.tree) / (precision.tree + recall.tree)

precision.tree
recall.tree
f1.tree

# ROC Curve and AUC
## Logistic Regression ROC
roc.lr <- roc(dat1.tst.ind$Target, prob.lr.tst)
auc.lr <- auc(roc.lr)
auc.lr

plot(roc.lr, col = "blue",
     main = "ROC Curve Comparison")

## Decision Tree ROC
prob.tree <- predict(tree.ind,
                     newdata = dat1.tst.ind,
                     type = "prob")[,2]

roc.tree <- roc(dat1.tst.ind$Target, prob.tree)
auc.tree <- auc(roc.tree)
auc.tree

plot(roc.tree, col = "red", add = TRUE)

legend("bottomright",
       legend = c("Logistic Regression", "Decision Tree"),
       col = c("blue", "red"),
       lwd = 2)

# Model Performance Summary
model.results <- data.frame(
  Model = c("Logistic Regression", "Decision Tree"),
  Accuracy = c(acc.lr, acc.tree),
  Precision = c(precision.lr, precision.tree),
  Recall = c(recall.lr, recall.tree),
  F1_Score = c(f1.lr, f1.tree),
  AUC = c(auc.lr, auc.tree)
)
model.results





# ============================================================
# 9. Application: Enrolled student prediction
# ============================================================

dat_e.ind <- dat_e[, indir_vars[-1]]

prob.enroll <- predict(lr.tr.ind,
                       newdata = dat_e.ind,
                       type = "response")

prob.dropout <- 1 - prob.enroll
risk_threshold <- 0.7
pred.dropout <- ifelse(prob.dropout > risk_threshold, 1, 0)
table(pred.dropout)

pred_summary <- data.frame(
  Category = c("Predicted Dropout", "Predicted Graduate"),
  Count = c(sum(pred.dropout == 1),
            sum(pred.dropout == 0))
)

ggplot(pred_summary, aes(x = Category, y = Count, fill = Category)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = format(Count, big.mark = ",")),
            vjust = -0.5,
            size = 5) +
  scale_fill_manual(values = c("Predicted Dropout" = "#bca37f",
                               "Predicted Graduate" = "#414e4b")) +
  labs(title = "Predicted Outcomes for Enrolled Students",
       x = "Prediction Category",
       y = "Number of Students",
       fill = "") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    axis.title = element_text(face = "bold")
  ) +
  ylim(0, max(pred_summary$Count) * 1.15)

# Identify High-Risk Students
high_risk <- dat_e[prob.dropout > risk_threshold, ]
nrow(high_risk)
head(high_risk)

# Save prediction results
enrolled_predictions <- dat_e
enrolled_predictions$Graduate_Probability <- prob.enroll
enrolled_predictions$Dropout_Probability <- prob.dropout
enrolled_predictions$Predicted_Dropout <- pred.dropout
head(enrolled_predictions)