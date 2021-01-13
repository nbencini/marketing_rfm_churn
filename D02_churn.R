library(caret)
library(rpart)
library(rpart.plot)
library(MLmetrics)
library(randomForest)
library(funModeling)


scontrini <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1,
         TIC_DATE < as.Date("01/01/2019", format = "%d/%m/%Y"),
         TIC_DATE >= as.Date("01/10/2018", format = "%d/%m/%Y"))

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
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)) %>%
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


# join con i dati delle carte fedeltà
churn <- left_join(churn, df_1_cli_fid_clean[, c("ID_CLI", "LAST_COD_FID")], by = "ID_CLI")


# Machine Learning model

train_index <- createDataPartition(churn$CHURN, p = .70, list = FALSE, times = 1)

#-- Train Test Split
train <- churn[train_index,]
test <- churn[-train_index,]





tree <- rpart(CHURN ~ RECENCY + IMPORTO_LORDO + COUNT_ACQUISTI + LAST_COD_FID,
              data = train)
rpart.plot(tree, extra = "auto")
summary(tree)


tree_rf <- randomForest(CHURN ~ RECENCY + IMPORTO_LORDO + COUNT_ACQUISTI + LAST_COD_FID,
                        data = train, ntree = 100)
print(tree_rf)


logistic <- train(CHURN ~ RECENCY + IMPORTO_LORDO + COUNT_ACQUISTI + LAST_COD_FID,
                  data = train,
                  method = "glm")
summary(logistic)


tree_pred <- predict(tree, test[, -5], type = "class")
p1 <- unlist(tree_pred)
confusionMatrix(p1, test$CHURN)

tree_rf_pred <- predict(tree_rf, test[,-5], type = "class")
confusionMatrix(tree_rf_pred, test$CHURN)

logistic_pred <- predict(logistic, test[, -5], type = "raw")
confusionMatrix(logistic_pred, test$CHURN)




accuracy <- as.data.frame(t(cbind(confusionMatrix(logistic_pred, test$CHURN)$overall[1],
                                  confusionMatrix(tree_rf_pred, test$CHURN)$overall[1],
                                  confusionMatrix(tree_pred, test$CHURN)$overall[1])))

accuracy <- as.data.frame(cbind(c("Logistic Regression", "Random Forest", "Decision Tree"), accuracy))

colnames(accuracy) <- c("Models", "Accuracy")

ggplot(data = accuracy,
       aes(x = Models,
           y = Accuracy,
           fill = Models)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(0.681, 0.693)) +
  theme_minimal() +
  guides(fill = FALSE) +
  labs(title = "Accuracy",
       x = "Models",
       y = " ") +
  scale_fill_manual(values = c("#FF1053","#6C6EA0","#66C7F4","#C1CAD6")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  
  plot(accuracy$Accuracy)


#-- Probability
p_tree = predict(tree, test[,-5], "prob")[,1]
p_rf = predict(tree_rf, test[,-5], "prob")[,1]
p_log = predict(logistic, test[,-5], "prob")[,1]

#-- Data Frame
data_class = as.data.frame(cbind(p_tree, p_rf, p_log))
data_class = cbind(data_class, test$CHURN)
colnames(data_class) <- c("p_tree", "p_rf", "p_log", "churn")
head(data_class)

#-- Lift
lift_tree = gain_lift(data = data_class, score = 'p_tree', target = 'churn')
lift_rf = gain_lift(data = data_class, score = 'p_rf', target = 'churn')
lift_log = gain_lift(data = data_class, score = 'p_log', target = 'churn')






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
