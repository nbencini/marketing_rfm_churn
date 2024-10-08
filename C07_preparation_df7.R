#### FIRST LOOK of df_7 ####

str(df_7_tic)
summary(df_7_tic)

#### START CLEANING df_7 ####

df_7_tic_clean <- df_7_tic

#### CLEANING DATA TYPES in df_7 ####

## formatting dates and times ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(TIC_DATETIME = as.POSIXct(DATETIME, format="%Y-%m-%dT%H%M%S")) %>%
  mutate(TIC_HOUR = hour(TIC_DATETIME)) %>%
  mutate(TIC_DATE = as.Date(TIC_DATETIME)) %>%
  select(-DATETIME)

## formatting boolean as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(DIREZIONE = as.factor(DIREZIONE))

## formatting numerical categories as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(COD_REPARTO = as.factor(COD_REPARTO))

#### CONSISTENCY CHECK ID_CLI in df_1/df_7 ####

cons_idcli_df1_df7 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_7_tic_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_7 = 1) %>%
              distinct(),
            by = "ID_CLI") %>%
  group_by(is_in_df_1, is_in_df_7) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df7

#!!! NOTE: all ID_CLI in df_7 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_7 !!!#  

#### RESHAPING df_7 ####

df_7_tic_clean_final <- df_7_tic_clean %>%
  ## adding day characterization ##
  mutate(TIC_DATE_WEEKDAY = wday(TIC_DATE)) %>%
  mutate(TIC_DATE_HOLIDAY = isHoliday("Italy", TIC_DATE)) %>%
  mutate(TIC_DATE_TYP = case_when(
    (TIC_DATE_WEEKDAY %in% c(6,7)) ~ "weekend",
    (TIC_DATE_HOLIDAY == TRUE) ~ "holiday",
    (TIC_DATE_WEEKDAY < 7) ~ "weekday",
    TRUE ~ "other"))

#### EXPLORE VARIABLES in df_7 ####

### GENERAL OVERVIEW ###

## compute aggregate
df7_overview <- df_7_tic_clean_final %>% 
  summarize(MIN_DATE = min(TIC_DATE),
            MAX_DATE = max(TIC_DATE),
            TOT_TICs = n_distinct(ID_SCONTRINO),
            TOT_CLIs = n_distinct(ID_CLI))

df7_overview

### Variable DIREZIONE ###

## compute aggregate
df7_dist_direction <- df_7_tic_clean_final %>%
  group_by(DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO),
            TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_TICs = TOT_TICs/df7_overview$TOT_TICs,
         PERCENT_CLIs = TOT_CLIs/df7_overview$TOT_CLIs)

df7_dist_direction

### Variable TIC_HOURS ###

## compute aggregate
df7_dist_hour <- df_7_tic_clean_final %>%
  group_by(TIC_HOUR, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO),
            TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE,
                     ALL_TOT_TICs = TOT_TICs,
                     ALL_TOT_CLIs = TOT_CLIs),
            by = 'DIREZIONE') %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs,
         PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_hour

## plot aggregate
plot_df7_dist_hour <- (
  ggplot(data=df7_dist_hour, aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_hour

## plot aggregate percent
plot_df7_dist_hour_percent <- (
  ggplot(data=df7_dist_hour, aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_hour_percent


### Variable COD_REPARTO ###

## compute aggregate
df7_dist_dep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO),
            TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE,
                     ALL_TOT_TICs = TOT_TICs,
                     ALL_TOT_CLIs = TOT_CLIs),
            by = 'DIREZIONE') %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs,
         PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)
    
df7_dist_dep

## plot aggregate
plot_df7_dist_dep <- (
  ggplot(data=df7_dist_dep, aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_dep

## plot aggregate percent
plot_df7_dist_dep_percent <- (
  ggplot(data=df7_dist_dep, aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_dep_percent

### Variable TIC_DATE_TYP ###

## compute aggregate
df7_dist_datetyp <- df_7_tic_clean_final %>%
  group_by(TIC_DATE_TYP, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO),
            TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE,
                     ALL_TOT_TICs = TOT_TICs,
                     ALL_TOT_CLIs = TOT_CLIs),
            by = 'DIREZIONE') %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs,
         PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_datetyp

## plot aggregate
plot_df7_dist_datetyp <- (
  ggplot(data=df7_dist_datetyp, aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_datetyp

## plot aggregate percent
plot_df7_dist_datetyp_percent <- (
  ggplot(data=df7_dist_datetyp, aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_datetyp_percent

### Variable average IMPORTO_LORDO and average SCONTO per TICKET ###

## compute aggregate
df7_dist_importosconto <- df_7_tic_clean_final %>%
  group_by(ID_SCONTRINO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO),
            SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto <- df7_dist_importosconto %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO),
            AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto

## plot aggregate
plot_df7_dist_importo <- (
  ggplot(data=df7_dist_importosconto %>% filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000)), aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo

## plot aggregate
plot_df7_dist_sconto <- (
  ggplot(data=df7_dist_importosconto %>% filter((SCONTO > -250) & (SCONTO < 250)), aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto

#### ???? TO DO df_7 ???? ####
# EXPLORE average IMPORTO_LORDO and average SCONTO by COD_REPARTO
df7_dist_importosconto_cod_rep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = mean(IMPORTO_LORDO),
            SCONTO = mean(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_importosconto_cod_rep

#-- IMPORTO_LORDO
ggplot(data = df7_dist_importosconto_cod_rep, aes(fill = DIREZIONE, x = COD_REPARTO, y = IMPORTO_LORDO)) +
  geom_bar(stat = "identity") +
  theme_minimal()

#-- SCONTO
ggplot(data = df7_dist_importosconto_cod_rep, aes(fill = DIREZIONE, x = COD_REPARTO, y = SCONTO)) +
  geom_bar(stat = "identity") +
  theme_minimal()

# EXPLORE ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO)
df_7_tic_clean_final$ID_ARTICOLO <- as.factor(df_7_tic_clean_final$ID_ARTICOLO)

df7_dist_id_articolo <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_ARTICOLO) %>% 
  summarize(NUM_VENDITE = n_distinct(ID_SCONTRINO)) %>%
  ungroup() %>%
  as.data.frame() %>%
  arrange(desc(NUM_VENDITE))

df7_dist_id_articolo

# EXPLORE average IMPORTO_LORDO and average SCONTO per ID_CLI
df7_dist_importosconto_cli <- df_7_tic_clean_final %>%
  group_by(ID_CLI, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = mean(IMPORTO_LORDO),
            SCONTO = mean(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_importosconto_cli

# IMPORTO_LORDO
ggplot(data = df7_dist_importosconto_cli %>% filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000)), aes(fill = DIREZIONE, x= IMPORTO_LORDO)) +
  geom_histogram(binwidth=20) +
  theme_minimal()

#-- SCONTO
ggplot(data = df7_dist_importosconto_cli %>% filter((SCONTO > -250) & (SCONTO < 250)), aes(fill = DIREZIONE, x = SCONTO)) +
  geom_histogram(binwidth = 10) +
  theme_minimal()

# compute the distribution of customers by number of purchases (as described in the slides)
df7_dist_customers_purchases <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(NUM_PURCHASES = n_distinct(ID_SCONTRINO)) %>%
  ungroup() %>%
  as.data.frame() %>%
  arrange(desc(NUM_PURCHASES))

df7_dist_customers_purchases

ggplot(data = df7_dist_customers_purchases %>% filter(NUM_PURCHASES <= 50), aes(x= NUM_PURCHASES)) +
  geom_histogram(binwidth=1) +
  theme_minimal()



# compute the days for next purchase curve (as described in the slides)
df7_days_next_purchase <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  select(ID_CLI, ID_ARTICOLO, TIC_DATE, DIREZIONE) %>%
  arrange(ID_CLI) %>%
  group_by(ID_CLI) %>%
  mutate(date_diff = TIC_DATE - lag(TIC_DATE)) %>%
  group_by(ID_CLI) %>%
  na.omit() %>%
  summarize(AVG_next_purchase = mean(date_diff)) %>%
  arrange(AVG_next_purchase)

lag_date_purchases <- as.data.frame(table(df7_days_next_purchase$AVG_next_purchase))
lag_date_purchases <- lag_date_purchases[-1, ]
lag_date_purchases$Perc <- x$Freq/sum(x$Freq)

ggplot(lag_date_purchases, aes(x = as.numeric(as.character(Var1)), y = cumsum(Perc))) +
  labs(title = "Next Purchase Curve",
       x = "Last Purchase Date (in Days)",
       y = "Cumulative Percent") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = 60, linetype = "dotted") +
  geom_line(size = 1)



dist_scontrini <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  group_by(Mese = paste(year(TIC_DATETIME), month(TIC_DATETIME), sep="-")) %>%
  summarise(n = n_distinct(ID_SCONTRINO))

d2 <- as.data.frame(rbind(
  cbind("2018-5", 63601),
  cbind("2018-6", 65651),
  cbind("2018-7", 69052),
  cbind("2018-8", 70829),
  cbind("2018-9", 73766),
  cbind("2018-10", 79970),
  cbind("2018-11", 92383),
  cbind("2018-12", 82476),
  cbind("2019-1", 73830),
  cbind("2019-2", 72410),
  cbind("2019-3", 89348),
  cbind("2019-4", 74530)
))

d2$V1 <- factor(d2$V1, levels = d2$V1)

ggplot(data=d2, aes(x=V1, y=V2)) +
  geom_bar(stat="identity") +
  labs(title = "Distribuzione Scontrini per Mese", x = "Mese", y = "# Scontrini") +
  theme_minimal()


# count scontrini
n_distinct((df_7_tic_clean_final %>%
  filter(DIREZIONE == 1))$ID_CLI)


#### FINAL REVIEW df_7_clean ####

str(df_7_tic_clean_final)
summary(df_7_tic_clean_final)
