# ==========================
library(readxl)
library(dplyr)
library(lme4)
library(Matrix)
library(emmeans)
library(ggplot2)
library(binom)

df <- read_excel("SOR.xlsx")
names(df) <- tolower(trimws(names(df)))

# Création des variables
df <- df %>%
  mutate(SOR_bin = ifelse(sor == 1, 1, 0))

df <- df %>%
  mutate(
    Traitement = case_when(
      traitement == "Placebo-Fluviral" & dose == 1 ~ "Placebo",
      traitement == "Placebo-Fluviral" & dose == 2 ~ "Fluviral",
      traitement == "Fluviral-Placebo" & dose == 1 ~ "Fluviral",
      traitement == "Fluviral-Placebo" & dose == 2 ~ "Placebo",
      traitement == "Placebo-Vaxigrip" & dose == 1 ~ "Placebo",
      traitement == "Placebo-Vaxigrip" & dose == 2 ~ "Vaxigrip",
      traitement == "Vaxigrip-Placebo" & dose == 1 ~ "Vaxigrip",
      traitement == "Vaxigrip-Placebo" & dose == 2 ~ "Placebo",
      TRUE ~ NA_character_
    )
  )
# ==========================
# Transformer les variables en facteurs
df$Traitement <- factor(df$Traitement, levels = c("Placebo", "Fluviral", "Vaxigrip"))
df$dose <- factor(df$dose, levels = c(1, 2))
df$categorie <- factor(df$categorie)

# Modèle GEE incluant interaction pour effet groupe/trt
mod_gee <- geeglm(SOR_bin ~ Traitement * categorie + dose,
                  data = df,
                  id = identifiant,
                  family = binomial,
                  corstr = "exchangeable")

emm <- emmeans(mod_gee, ~ Traitement, type = "response")

# Résumé
emm_summary <- summary(emm)
emm_summary
# Extraire les probabilités
probs <- data.frame(
  Traitement = emm_summary$Traitement,
  prob = emm_summary$prob 
)
probs
# Calcul du risk difference vs Placebo
rd_table <- probs %>%
  filter(Traitement != "Placebo") %>%
  mutate(risk_difference = prob - probs$prob[probs$Traitement == "Placebo"])

print(rd_table)

rd_table1 <- probs %>%
  filter(Traitement != "Fluviral") %>%
  mutate(risk_difference = prob - probs$prob[probs$Traitement == "Fluviral"])

rd_
