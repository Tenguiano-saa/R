library(geepack)
library(emmeans)
library(dplyr)

# --- Modèle GEE ---
mod_gee <- geeglm(SOR_bin ~ Traitement + dose + categorie,
                  data = df,
                  id = identifiant,
                  family = binomial,
                  corstr = "exchangeable")

# --- Extraire les probabilités ajustées ---
emm <- emmeans(mod_gee, ~ Traitement, type = "response")

# Résumé
emm_summary <- summary(emm)
emm_summary
# Extraire les probabilités
probs <- data.frame(
  Traitement = emm_summary$Traitement,
  prob = emm_summary$prob 
)
prob
# Calcul du risk difference vs Placebo
rd_table <- probs %>%
  filter(Traitement != "Placebo") %>%
  mutate(risk_difference = prob - probs$prob[probs$Traitement == "Placebo"])

print(rd_table)




contrasts <- contrast(emm, method = "pairwise", adjust = "tukey", type = "response")

print(contrasts)










