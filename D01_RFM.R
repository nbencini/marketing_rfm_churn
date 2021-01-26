scontrini_2019 <- df_7_tic_clean_final %>%
  filter(TIC_DATE > as.Date("01/01/2019", format = "%d/%m/%Y"))

# RECENCY
recency <- scontrini_2019 %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(LAST_PURCHASE = max(TIC_DATE))

recency$RECENCY <- difftime(as.Date("30/04/2019", format = "%d/%m/%Y"),
                            recency$LAST_PURCHASE,
                            units = "days")

recency <- within(recency,
                  CLASS_RECENCY <- cut(as.numeric(recency$RECENCY),
                                       breaks = quantile(recency$RECENCY, probs = c(0, .25, .75, 1)),
                                       include.lowest = T,
                                       labels = c("LOW", "MEDIUM", "HIGH")))

recency_counts <- as.data.frame(table(recency$CLASS_RECENCY))
recency_counts


# FREQUENCY
frequency <- scontrini_2019 %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(COUNT_ACQUISTI = n_distinct(ID_SCONTRINO))


frequency <- within(frequency,
                    CLASS_FREQUENCY <- cut(frequency$COUNT_ACQUISTI,
                                           breaks = quantile(frequency$COUNT_ACQUISTI, probs = c(0, .50, .75, 1)),
                                           include.lowest = T,
                                           right = F,
                                           labels = c("LOW", "MEDIUM", "HIGH")))

frequency_counts <- as.data.frame(table(frequency$CLASS_FREQUENCY))
frequency_counts


# MONETARY
monetary <- scontrini_2019 %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)) %>%
  ungroup() %>%
  as.data.frame()

monetary <- within(monetary,
                   CLASS_MONETARY <- cut(monetary$IMPORTO_LORDO,
                                         breaks = quantile(monetary$IMPORTO_LORDO, probs = c(0, .25, .75, 1)),
                                         include.lowest = T,
                                         labels = c("LOW", "MEDIUM", "HIGH")))

monetary_counts <- as.data.frame(table(monetary$CLASS_MONETARY))
monetary_counts


# RFM
rfm <- merge(frequency, monetary, by = "ID_CLI")
rfm <- merge(rfm, recency, by = "ID_CLI")

rfm$RF <- NA

for(i in c(1:nrow(rfm))){
  if(rfm$CLASS_RECENCY[i] == "LOW" && rfm$CLASS_FREQUENCY[i] == "LOW") rfm$RF[i] <- "One-Timer"
  if(rfm$CLASS_RECENCY[i] == "MEDIUM" && rfm$CLASS_FREQUENCY[i] == "LOW") rfm$RF[i] <- "One-Timer"
  if(rfm$CLASS_RECENCY[i] == "HIGH" && rfm$CLASS_FREQUENCY[i] == "LOW") rfm$RF[i] <- "Leaving"
  if(rfm$CLASS_RECENCY[i] == "LOW" && rfm$CLASS_FREQUENCY[i] == "MEDIUM") rfm$RF[i] <- "Engaged"
  if(rfm$CLASS_RECENCY[i] == "MEDIUM" && rfm$CLASS_FREQUENCY[i] == "MEDIUM") rfm$RF[i] <- "Engaged"
  if(rfm$CLASS_RECENCY[i] == "HIGH" && rfm$CLASS_FREQUENCY[i] == "MEDIUM") rfm$RF[i] <- "Leaving"
  if(rfm$CLASS_RECENCY[i] == "LOW" && rfm$CLASS_FREQUENCY[i] == "HIGH") rfm$RF[i] <- "Top"
  if(rfm$CLASS_RECENCY[i] == "MEDIUM" && rfm$CLASS_FREQUENCY[i] == "HIGH") rfm$RF[i] <- "Top"
  if(rfm$CLASS_RECENCY[i] == "HIGH" && rfm$CLASS_FREQUENCY[i] == "HIGH") rfm$RF[i] <- "Leaving Top"
}

rf_counts = rfm %>%
  count(CLASS_RECENCY, CLASS_FREQUENCY, RF)
rf_counts

ggplot(rf_counts, aes(x=CLASS_FREQUENCY, y=CLASS_RECENCY, fill=n)) + 
  geom_tile() +
  geom_text(aes(label = n)) +
  scale_fill_distiller(direction=1) +
  theme_minimal()


rf <- as.data.frame(table(rfm$RF)) %>%
  arrange(Freq)
rf

ggplot(data = rf, aes(x=Var1, y=Freq)) +
  geom_bar(stat = "identity") +
  scale_colour_brewer(palette = "Spectral") +
  labs(title = "RF Classes Distribution", x = "RF Classes", y = "# Customers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = FALSE)



rfm$RFM <- NA

for(i in c(1:nrow(rfm))){
  if(rfm$RF[i] == "One-Timer" && rfm$CLASS_MONETARY[i] == "LOW") rfm$RFM[i] <- "Cheap"
  if(rfm$RF[i] == "Leaving" && rfm$CLASS_MONETARY[i] == "LOW") rfm$RFM[i] <- "Tin"
  if(rfm$RF[i] == "Engaged" && rfm$CLASS_MONETARY[i] == "LOW") rfm$RFM[i] <- "Copper"
  if(rfm$RF[i] == "Leaving Top" && rfm$CLASS_MONETARY[i] == "LOW") rfm$RFM[i] <- "Bronze"
  if(rfm$RF[i] == "Top" && rfm$CLASS_MONETARY[i] == "LOW") rfm$RFM[i] <- "Silver"
  
  if(rfm$RF[i] == "One-Timer" && rfm$CLASS_MONETARY[i] == "MEDIUM") rfm$RFM[i] <- "Tin"
  if(rfm$RF[i] == "Leaving" && rfm$CLASS_MONETARY[i] == "MEDIUM") rfm$RFM[i] <- "Copper"
  if(rfm$RF[i] == "Engaged" && rfm$CLASS_MONETARY[i] == "MEDIUM") rfm$RFM[i] <- "Bronze"
  if(rfm$RF[i] == "Leaving Top" && rfm$CLASS_MONETARY[i] == "MEDIUM") rfm$RFM[i] <- "Silver"
  if(rfm$RF[i] == "Top" && rfm$CLASS_MONETARY[i] == "MEDIUM") rfm$RFM[i] <- "Gold"
  
  if(rfm$RF[i] == "One-Timer" && rfm$CLASS_MONETARY[i] == "HIGH") rfm$RFM[i] <- "Copper"
  if(rfm$RF[i] == "Leaving" && rfm$CLASS_MONETARY[i] == "HIGH") rfm$RFM[i] <- "Bronze"
  if(rfm$RF[i] == "Engaged" && rfm$CLASS_MONETARY[i] == "HIGH") rfm$RFM[i] <- "Silver"
  if(rfm$RF[i] == "Leaving Top" && rfm$CLASS_MONETARY[i] == "HIGH") rfm$RFM[i] <- "Gold"
  if(rfm$RF[i] == "Top" && rfm$CLASS_MONETARY[i] == "HIGH") rfm$RFM[i] <- "Diamond"
}


rfm_counts = rfm %>%
  count(RF, CLASS_MONETARY, RFM)

rfm_counts

ggplot(rfm_counts, aes(x = RF, y = CLASS_MONETARY, fill = n)) + 
  geom_tile() +
  geom_text(aes(label = n)) +
  scale_fill_distiller(direction=1) +
  theme_minimal()


rfm_distrib <- as.data.frame(table(rfm$RFM))

ggplot(data = rfm_distrib, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  scale_colour_brewer() +
  labs(title = "RFM Classes Distribution", x = "RFM Classes", y = "# Customers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = FALSE)
