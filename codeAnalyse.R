# ==========================
# Chargement des librairies
# ==========================
library(readxl)
library(dplyr)
library(lme4)
library(Matrix)
library(emmeans)
library(ggplot2)
library(binom)

# ==========================
# Charger les données
# ==========================
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

# ==========================
# Calcul des % bruts par produit (première dose)
# ==========================
prop_sor_first_dose <- df %>%
  filter(dose == 1, Traitement %in% c("Fluviral", "Vaxigrip")) %>%
  group_by(Traitement) %>%
  summarise(
    n = n(),
    n_sor = sum(SOR_bin)
  ) %>%
  mutate(risk_pct = 100 * n_sor / n)
prop_sor_prod_cat

# IC exact binomial
ic <- binom.confint(prop_sor_first_dose$n_sor, prop_sor_first_dose$n, method="exact")
prop_sor_first_dose <- cbind(prop_sor_first_dose, ic[,c("lower","upper")])
prop_sor_first_dose <- prop_sor_first_dose %>%
  mutate(lower = 100 * lower, upper = 100 * upper)

print(prop_sor_first_dose)

# ==========================
# Proportions par produit et catégorie
# ==========================
prop_sor_prod_cat <- df %>%
  group_by(produit, categorie) %>%
  summarise(
    n = n(),
    n_sor = sum(SOR_bin),
    prop_sor = mean(SOR_bin)
  )

# ==========================
# Graphique des proportions SOR par produit et catégorie
# ==========================
ggplot(prop_sor_prod_cat, aes(x = produit, y = prop_sor, fill = categorie)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(prop_sor*100,1)), 
            position = position_dodge(width=0.9), vjust=-0.3) +
  labs(title = "Proportion de récidive SOR par produit et catégorie",
       y = "Proportion SOR (%)", x = "Produit") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# ==========================
# Modèle mixte logistique
# ==========================
mod <- glmer(SOR_bin ~ produit + dose + categorie + (1 | identifiant),
             data = df, family = binomial)

summary(mod)

# ==========================
# Comparaison post-hoc des produits (marges ajustées en probabilités)
# ==========================
comp_prod_prob <- emmeans(mod, pairwise ~ produit, type = "response")
print(comp_prod_prob$emmeans)
print(comp_prod_prob$contrasts)

# ==========================
# Comparaison post-hoc des catégories (facultatif)
# ==========================
comp_cat_prob <- emmeans(mod, pairwise ~ categorie, type = "response")
print(comp_cat_prob$emmeans)
print(comp_cat_prob$contrasts)




