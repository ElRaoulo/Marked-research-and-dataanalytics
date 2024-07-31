library(tidyverse)

#import data
data <- read_csv("hauptstudie_data_renamed1.csv")
data <- rename(data, "SuspicionProbe" =  "Suspicion Probe")



Suspicion <- select(data, "SuspicionProbe")
#Es gibt keine Teilnehmer, die Vermutet haben, dass Aufmerksamkeitsallokation untersucht wurde.
#-> alle Daten können verwendet werden


demographics <- c("Alter", "Geschlecht", "Beruf", "Einkommen", "SuspicionProbe")

#get the relevant data
data_attention <- data %>%
  select(contains("Attention"), all_of(demographics))


#replace nagative values with 0
data_attention <- data_attention %>%
  mutate(across(where(is.numeric), ~if_else(. < 0, NA, .)))




#data_compact <- data.frame(data_attention[1])

#data_compact$Wertigkeit_Schuhe_AttentionPrimedHipHopSchuhe <- rowMeans(data_attention[,2:4], na.rm = TRUE)

names(data_attention) <- gsub("product_|Produkt_", "", names(data_attention))

data_attention <- data_attention %>%
  mutate(across(contains("Attention"), as.factor))



data_attention <- data_attention %>%
  mutate(across(demographics, as.factor))


data_attention_long <- data_attention %>%
  pivot_longer(cols = contains("Attention"), names_to = "Variable", values_to = "Rating") %>%
  separate(Variable, into = c("Attribute", "Product", "Group_type"), sep = "_", extra = "merge") %>%
  mutate(
    Group = case_when(
      Group_type == "AttentionPrimedHipHopSchuhe" ~ "Primed_Schuhe",
      Group_type == "AttentionNOTPrimedHipHopSchuhe" ~ "NOTPrimed_Schuhe",
      Group_type == "AttentionPrimedHipHopTasche" ~ "Primed_Tasche",
      Group_type == "AttentionNOTPrimedHipHopTasche" ~ "NOTPrimed_Tasche",
    )
  ) %>%
  select(-Group_type)


data_attention_long <- data_attention_long[!is.na(data_attention_long$Rating), ]

clean_data <- pivot_wider(data = data_attention_long, 
                                id_cols = c(SuspicionProbe, Alter, Geschlecht, Beruf, Einkommen, Product, Group), 
                                names_from = Attribute, 
                                values_from = Rating, 
                                values_fn = list(Rating = function(x) x[1]))


clean_data <- clean_data %>%
  mutate(across(8:ncol(clean_data), as.character))%>%
  mutate(across(8:ncol(clean_data), as.numeric))

clean_data <- clean_data %>%
  mutate(Wertigkeit = rowMeans(across(starts_with("Wertigkeit")), na.rm = TRUE))%>%
  mutate(Liking = rowMeans(across(starts_with("Liking")), na.rm = TRUE))%>%
  mutate(Fluency = rowMeans(across(starts_with("Fluency")), na.rm = TRUE))%>%
  mutate(Konsistenz = rowMeans(across(starts_with("Konsistenz")), na.rm = TRUE))

clean_data <- clean_data %>%
  mutate(across(contains("Stil"),
                ~case_when(
                  . == 1 ~ "Minimalismus",
                  . == 2 ~ "Hippie",
                  . == 3 ~ "HipHop",
                  TRUE ~ as.character(.)
                )))

##################################################

#       VISUALISIERUNG

##################################################


#Boxplot Preiserwartung aller daten

clean_data_boxplot <- clean_data %>%
  filter(Product != "outfit")

ggplot(clean_data_boxplot, aes(x = Product, y = Preiserwartung, fill = Group)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 500)) +
  labs(x = "Produkt", y = "Preiserwartung in Euro", title = "Boxplot der Preiserwartung nach Gruppe") +
  theme_minimal() 


#Boxplot Preiserwartung Attention NOT Primed Hippie Tasche

clean_data_primed_tasche <- clean_data %>%
  filter(Group == "NOTPrimed_Tasche")

ggplot(clean_data_primed_tasche, aes(x = Product, y = Preiserwartung, fill = Product)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 300)) +
  labs(x = "Group", y = "Preiserwartung in Euro", title = "Boxplot der Preiserwartung nach Gruppe") +
  theme_minimal() 



#Boxplot Wertigkeit

ggplot(clean_data, aes(x = Product, y = Wertigkeit, fill = Group)) +
  geom_boxplot() +
  labs(x = "Group", y = "Wertigkeit", title = "Boxplot der Wertigkeit nach Gruppe") +
  theme_minimal()



#Boxplot Liking

ggplot(clean_data, aes(x = Product, y = Liking, fill = Group)) +
  geom_boxplot() +
  labs(x = "Group", y = "Liking", title = "Boxplot des Likings nach Gruppe") +
  theme_minimal()


#Boxplot outfits Fluency

clean_data_outfits <- clean_data %>%
  filter(Product == "outfit")

ggplot(clean_data_outfits, aes(x = Group, y = Fluency, fill = Group)) +
  geom_boxplot() +
  labs(x = "Group", y = "Fluency", title = "Boxplot der Fluency nach Gruppe") +
  theme_minimal()


#Boxplot outfits Konsistenz

ggplot(clean_data_outfits, aes(x = Group, y = Konsistenz, fill = Group)) +
  geom_boxplot() +
  labs(x = "Group", y = "Konsistenz", title = "Boxplot der Fluency nach Gruppe") +
  theme_minimal()



##################################################

#       CRONBACHAS ALPHY

##################################################

#install.packages("psych")
library(psych)

#Alpha Wertigkeit

print(psych::alpha(clean_data[, c("Wertigkeit1", "Wertigkeit2", "Wertigkeit3")], na.rm = TRUE))
#0.78 Eigentlich zu wenig O.O
# Pretest: 0.87 (raw_alpha) 


#Alpha Liking

print(psych::alpha(clean_data[, c("Liking1", "Liking2", "Liking3")], na.rm = TRUE))
#0.94 Sehr Gut
# Pretest: 0.95 (raw_alpha) 


#Alpha Fluency

print(psych::alpha(clean_data[, c("Fluency1", "Fluency2", "Fluency3", "Fluency4")], na.rm = TRUE))
#0.66 viel zu wenig O.O
# Pretest: 0.87 (raw_alpha) 



#Alpha Konsistenz

print(psych::alpha(clean_data[, c("Konsistenz1", "Konsistenz2", "Konsistenz3")], na.rm = TRUE))
#0.73 Eigentlich zu wenig O.O
# Pretset: 0.95 (raw_alpha) 




##################################################

#       TESTEN  Prieserwartung inkonsistenz in den Schuhen

##################################################

#data_primed
data_primed_schuhe <- clean_data%>%
  filter(Group == "Primed_Schuhe")%>%
  filter(Product == "Schuhe")%>%
  select(Preiserwartung)%>%
  rename("Preiserwartung_Primed" = "Preiserwartung")%>%
  arrange(Preiserwartung_Primed)


#data_NOT_primed
data_NOT_primed_schuhe <- clean_data%>%
  filter(Group == "NOTPrimed_Schuhe")%>%
  filter(Product == "Schuhe")%>%
  select(Preiserwartung)%>%
  rename("Preiserwartung_NOTPrimed" = "Preiserwartung")%>%
  arrange(Preiserwartung_NOTPrimed)

#zusammenfügen
data_schuhe <- cbind(data_primed_schuhe, data_NOT_primed_schuhe)

data_schuhe_long <- tidyr::pivot_longer(data_schuhe, 
                                 cols = everything(),
                                 names_to = "Condition", 
                                 values_to = "Price")

######################### Testen auf normalverteilung

#data_schuhe <- data_schuhe[1:10,]

hist(data_schuhe$Preiserwartung_Primed, freq = FALSE)
curve(dnorm(x, mean = mean(data_schuhe$Preiserwartung_Primed), sd = sd(data_schuhe$Preiserwartung_Primed)), add = TRUE)

shapiro.test(data_schuhe$Preiserwartung_Primed)
#W = 0.62672, p-value = 5.484e-05
#  -> nicht normalverteilt!!!!!! 
# -> kein t-test möglich,     -> also Man Whitney U test


wilcox.test(Price ~ Condition, data = data_schuhe_long)
#W = 38.5, p-value = 0.1559
# -> kein statistisch signifikanter Unterschied festzustellen





##################################################

#       TESTEN  Liking inkonsistenz in den Schuhen

##################################################

#data_primed
liking_primed_schuhe <- clean_data%>%
  filter(Group == "Primed_Schuhe")%>%
  filter(Product == "Schuhe")%>%
  select(Liking)%>%
  rename("Liking_Primed" = "Liking")%>%
  arrange(Liking_Primed)


#data_NOT_primed
liking_NOT_primed_schuhe <- clean_data%>%
  filter(Group == "NOTPrimed_Schuhe")%>%
  filter(Product == "Schuhe")%>%
  select(Liking)%>%
  rename("Liking_NOTPrimed" = "Liking")%>%
  arrange(Liking_NOTPrimed)

#zusammenfügen
liking_schuhe <- cbind(liking_primed_schuhe, liking_NOT_primed_schuhe)

liking_schuhe_long <- tidyr::pivot_longer(liking_schuhe, 
                                        cols = everything(),
                                        names_to = "Condition", 
                                        values_to = "Price")

######################### Testen auf normalverteilung


hist(liking_schuhe$Liking_Primed, freq = FALSE)
curve(dnorm(x, mean = mean(liking_schuhe$Liking_Primed), sd = sd(liking_schuhe$Liking_Primed)), add = TRUE)

shapiro.test(liking_schuhe$Liking_Primed)
#W = 0.89981, p-value = 0.1839
#  -> normalverteilt 
# -> t-test möglich


hist(liking_schuhe$Liking_NOTPrimed, freq = FALSE)
curve(dnorm(x, mean = mean(liking_schuhe$Liking_NOTPrimed), sd = sd(liking_schuhe$Liking_NOTPrimed)), add = TRUE)

shapiro.test(liking_schuhe$Liking_NOTPrimed)
#W = 0.9553, p-value = 0.712
#  -> normalverteilt 
# -> t-test möglich


t.test(liking_schuhe$Liking_Primed, liking_schuhe$Liking_NOTPrimed, paired = FALSE)
#t = 0.3518, df = 18.998, p-value = 0.7289
# -> kein statistisch signifikanter Unterschied festzustellen






##################################################

#       TESTEN  Prieserwartung inkonsistenz in der Tasche

##################################################

#data_primed
data_primed_tasche <- clean_data%>%
  filter(Group == "Primed_Tasche")%>%
  filter(Product == "Schuhe")%>%
  select(Preiserwartung)%>%
  rename("Preiserwartung_Primed" = "Preiserwartung")%>%
  arrange(Preiserwartung_Primed)


#data_NOT_primed
data_NOT_primed_tasche <- clean_data%>%
  filter(Group == "NOTPrimed_Tasche")%>%
  filter(Product == "Schuhe")%>%
  select(Preiserwartung)%>%
  rename("Preiserwartung_NOTPrimed" = "Preiserwartung")%>%
  arrange(Preiserwartung_NOTPrimed)

data_NOT_primed_tasche <- data_NOT_primed_tasche[2:12,]

#zusammenfügen
data_tasche <- cbind(data_primed_tasche, data_NOT_primed_tasche)

data_tasche_long <- tidyr::pivot_longer(data_tasche, 
                                        cols = everything(),
                                        names_to = "Condition", 
                                        values_to = "Price")

######################### Testen auf normalverteilung


hist(data_tasche$Preiserwartung_Primed, freq = FALSE)
curve(dnorm(x, mean = mean(data_tasche$Preiserwartung_Primed), sd = sd(data_tasche$Preiserwartung_Primed)), add = TRUE)

shapiro.test(data_tasche$Preiserwartung_Primed)
#W = 0.57418, p-value = 1.217e-05
#  -> nicht normalverteilt!!!!!! 
# -> kein t-test möglich,     -> also Man Whitney U test


wilcox.test(Price ~ Condition, data = data_tasche_long)
#W = 68.5, p-value = 0.6194
# -> kein statistisch signifikanter Unterschied festzustellen





##################################################

#       TESTEN  Liking inkonsistenz in der Tasche

##################################################

#data_primed
liking_primed_tasche <- clean_data%>%
  filter(Group == "Primed_Tasche")%>%
  filter(Product == "Schuhe")%>%
  select(Liking)%>%
  rename("Liking_Primed" = "Liking")%>%
  arrange(Liking_Primed)


#data_NOT_primed
liking_NOT_primed_tasche <- clean_data%>%
  filter(Group == "NOTPrimed_Tasche")%>%
  filter(Product == "Schuhe")%>%
  select(Liking)%>%
  rename("Liking_NOTPrimed" = "Liking")%>%
  arrange(Liking_NOTPrimed)

liking_NOT_primed_tasche <- liking_NOT_primed_tasche[2:12,]


#zusammenfügen
liking_tasche <- cbind(liking_primed_tasche, liking_NOT_primed_tasche)

liking_tasche_long <- tidyr::pivot_longer(liking_tasche, 
                                          cols = everything(),
                                          names_to = "Condition", 
                                          values_to = "Price")

######################### Testen auf normalverteilung


hist(liking_tasche$Liking_Primed, freq = FALSE)
curve(dnorm(x, mean = mean(liking_tasche$Liking_Primed), sd = sd(liking_tasche$Liking_Primed)), add = TRUE)

shapiro.test(liking_tasche$Liking_Primed)
#W = 0.88864, p-value = 0.1337#  -> normalverteilt 
# -> t-test möglich


hist(liking_tasche$Liking_NOTPrimed, freq = FALSE)
curve(dnorm(x, mean = mean(liking_tasche$Liking_NOTPrimed), sd = sd(liking_tasche$Liking_NOTPrimed)), add = TRUE)

shapiro.test(liking_tasche$Liking_NOTPrimed)
#W = 0.82078, p-value = 0.01766
#  -> normalverteilt 
# -> t-test möglich


t.test(liking_tasche$Liking_Primed, liking_tasche$Liking_NOTPrimed, paired = FALSE)
#t = 0.27616, df = 14.983, p-value = 0.7862
# -> kein statistisch signifikanter Unterschied festzustellen


























