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
  scale_y_continuous(limits = c(0, 300)) +
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



