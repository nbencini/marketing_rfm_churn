library(caret)
library(rpart)
library(rpart.plot)
library(MLmetrics)
library(randomForest)
library(funModeling)
library(e1071)

scontrini <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1,
         TIC_DATE < as.Date("01/01/2019", format = "%d/%m/%Y"),
         TIC_DATE >= as.Date("01/10/2018", format = "%d/%m/%Y"))

# holdout period is 2 months (~60d, 1/1 to 28/2)
holdout <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1,
         TIC_DATE < as.Date("01/03/2019", format = "%d/%m/%Y"),
         TIC_DATE >= as.Date("01/01/2019", format = "%d/%m/%Y"))

non_churner <- unique(holdout$ID_CLI)

churn_recency <- scontrini %>%
  group_by(ID_CLI) %>%
  summarise(LAST_PURCHASE = max(TIC_DATE))

churn_recency$RECENCY <- difftime(as.Date("01/01/2019", format = "%d/%m/%Y"),
                                  churn_recency$LAST_PURCHASE,
                                  units = "days")

churn_frequency <- scontrini %>%
  group_by(ID_CLI) %>%
  summarise(COUNT_ACQUISTI = n_distinct(ID_SCONTRINO))

churn_monetary <- scontrini %>%
  group_by(ID_CLI) %>%
  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO)) %>%
  ungroup() %>%
  as.data.frame()

churn <- merge(churn_recency, churn_frequency, by = "ID_CLI")
churn <- merge(churn, churn_monetary, by = "ID_CLI") %>%
  select(ID_CLI, RECENCY, IMPORTO_LORDO, COUNT_ACQUISTI)


churn$CHURN <- 1

for (i in c(1:nrow(churn))){
  if (churn$ID_CLI[i] %in% non_churner) churn$CHURN[i] <- 0
}

churn$CHURN <- as.factor(churn$CHURN)

ggplot(data = churn, aes(x = CHURN)) +
  geom_bar() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# join con i dati delle carte fedeltà
churn <- left_join(churn, df_1_cli_fid_clean[, c("ID_CLI", "LAST_COD_FID")], by = "ID_CLI")
churn <- left_join(churn, df_2_cli_account_clean[, c("ID_CLI", "TYP_JOB")], by = "ID_CLI")

# Machine Learning model

train_index <- createDataPartition(churn$CHURN, p = .70, list = FALSE, times = 1)

#-- Train Test Split
train <- churn[train_index,]
test <- churn[-train_index,]


# Decision Tree
tree <- rpart(CHURN ~ RECENCY + IMPORTO_LORDO + COUNT_ACQUISTI + LAST_COD_FID + TYP_JOB,
              data = train)
summary(tree)

tree_pred <- predict(tree, test[, -5], type = "class")
p1 <- unlist(tree_pred)
confusionMatrix(p1, test$CHURN)

precision(p1, test$CHURN, relevant="1")
recall(p1, test$CHURN, relevant="1")
F1_Score(p1, test$CHURN, positive = "1")


# Random Forest
tree_rf <- randomForest(CHURN ~ RECENCY + IMPORTO_LORDO + COUNT_ACQUISTI + LAST_COD_FID + TYP_JOB,
                        data = train, ntree = 100)
print(tree_rf)

tree_rf_pred <- predict(tree_rf, test[,-5], type = "class")
confusionMatrix(tree_rf_pred, test$CHURN)

precision(tree_rf_pred, test$CHURN, relevant="1")
recall(tree_rf_pred, test$CHURN, relevant="1")
F1_Score(tree_rf_pred, test$CHURN, positive = "1")


# Logistic Regression
logistic <- train(CHURN ~ RECENCY + IMPORTO_LORDO + COUNT_ACQUISTI + LAST_COD_FID + TYP_JOB,
                  data = train,
                  method = "glm")
summary(logistic)

logistic_pred <- predict(logistic, test[, -5], type = "raw")
confusionMatrix(logistic_pred, test$CHURN)

recall(logistic_pred, test$CHURN, relevant="1")
precision(logistic_pred, test$CHURN, relevant="1")
F1_Score(logistic_pred, test$CHURN, positive="1")

# Support Vector Machine
svmfit <- svm(CHURN ~ RECENCY + IMPORTO_LORDO + COUNT_ACQUISTI + LAST_COD_FID + TYP_JOB,
             data = train,
             kernel = "linear")
print(svmfit)

svm_pred <- predict(svmfit, test[, -5])
confusionMatrix(svm_pred, test$CHURN)

recall(svm_pred, test$CHURN, relevant="1")
precision(svm_pred, test$CHURN, relevant="1")
F1_Score(svm_pred, test$CHURN, positive="1")


# Naïve Bayes
nbfit <- naiveBayes(CHURN ~ RECENCY + IMPORTO_LORDO + COUNT_ACQUISTI + LAST_COD_FID + TYP_JOB,
                    data = train)
nbfit

nb_pred <- predict(nbfit, test[, -5])
confusionMatrix(nb_pred, test$CHURN)

recall(nb_pred, test$CHURN, relevant="1")
precision(nb_pred, test$CHURN, relevant="1")
F1_Score(nb_pred, test$CHURN, positive="1")


scores <- as.data.frame(rbind(
  cbind(recall(tree_pred, test$CHURN, relevant="1"), precision(tree_pred, test$CHURN, relevant="1"), F1_Score(tree_pred, test$CHURN, positive="1")),
  cbind(recall(tree_rf_pred, test$CHURN, relevant="1"), precision(tree_rf_pred, test$CHURN, relevant="1"), F1_Score(tree_rf_pred, test$CHURN, positive="1")),
  cbind(recall(logistic_pred, test$CHURN, relevant="1"), precision(logistic_pred, test$CHURN, relevant="1"), F1_Score(logistic_pred, test$CHURN, positive="1")),
  cbind(recall(svm_pred, test$CHURN, relevant="1"), precision(svm_pred, test$CHURN, relevant="1"), F1_Score(svm_pred, test$CHURN, positive="1")),
  cbind(recall(nb_pred, test$CHURN, relevant="1"), precision(nb_pred, test$CHURN, relevant="1"), F1_Score(nb_pred, test$CHURN, positive="1"))))


scores <- as.data.frame(cbind(c("Decision Tree", "Random Forest", "Logistic Regression", "SVM", "Naive Bayes"), scores))

colnames(scores) <- c("Model", "Recall", "Precision", "F1_Score")

scores

ggplot(data = scores, aes(x = Model, y = F1_Score)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(0.7, 0.8)) +
  theme_minimal() +
  guides(fill = FALSE) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = scores, aes(x = Model, y = Precision)) +
  geom_bar(stat = "identity") +
    coord_cartesian(ylim = c(0.6, 0.8)) +
  theme_minimal() +
  guides(fill = FALSE) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = scores, aes(x = Model, y = Recall)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(0.75, 1)) +
  theme_minimal() +
  guides(fill = FALSE) +
  theme(plot.title = element_text(hjust = 0.5))




scontrini_futuro <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1,
         TIC_DATE < as.Date("30/04/2019", format = "%d/%m/%Y"),
         TIC_DATE >= as.Date("01/02/2019", format = "%d/%m/%Y"))

new_churn_recency <- scontrini_futuro %>%
  group_by(ID_CLI) %>%
  summarise(LAST_PURCHASE = max(TIC_DATE))

new_churn_recency$RECENCY <- difftime(as.Date("30/04/2019",format = "%d/%m/%Y"),
                                      new_churn_recency$LAST_PURCHASE,
                                      units = "days")

new_churn_frequency <- scontrini_futuro %>%
  group_by(ID_CLI) %>%
  summarise(COUNT_ACQUISTI = n_distinct(ID_SCONTRINO))

new_churn_monetary <- scontrini_futuro %>%
  group_by(ID_CLI) %>%
  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO)) %>%
  ungroup() %>%
  as.data.frame()

new_churn <- merge(new_churn_recency, new_churn_frequency, by = "ID_CLI")
new_churn <- merge(new_churn, new_churn_monetary, by = "ID_CLI") %>%
  select(ID_CLI, RECENCY, IMPORTO_LORDO, COUNT_ACQUISTI)

new_churn <- left_join(new_churn, df_1_cli_fid_clean[, c("ID_CLI", "LAST_COD_FID")], by = "ID_CLI")


new_churn$prob_to_churn <- predict(logistic, new_churn, type = "prob")[,2]
