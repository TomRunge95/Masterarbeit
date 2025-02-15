library(MASS)  # Für glm.nb
library(pscl)  # Für Pseudo-R²options(scipen = 999)  # Verhindert wissenschaftliche Notation
library(DescTools)

df_links$Regierung[df_links$docid == "vsbericht_Baden-Würtemberg_links_2022.txt"] <- "Bündnis 90/Die Grünen, CDU"
df_links$Innenminister[df_links$docid == "vsbericht_Baden-Würtemberg_links_2022.txt"] <- "CDU"


df_links$Regierung[df_links$docid == "vsbericht_Berlin_links_2022.txt"] <- "SPD, Die Linke, Bündnis 90/Die Grünen"
df_links$Innenminister[df_links$docid == "vsbericht_Berlin_links_2022.txt"] <- "SPD"

df_links <- df_links %>% 
  mutate(im_pol = case_when(
    Innenminister %in% c("CDU", "CSU", "FDP", "PRO") ~ 0,
    Innenminister == "SPD" ~ 1,
    TRUE ~ NA_real_  # Falls andere Werte in der Spalte vorkommen
  ))


df_links$bundesland <- factor(df_links$bundesland, 
                              levels = c("Bund", setdiff(unique(df_links$bundesland), "Bund")))


df_links <- df_links %>% 
  filter(!Jahr == 2000)



model_tokengewalt_links_extended <- glm.nb(token_links ~ `Gewaltaten Links`*Jahr +
                                             `Personenpotential Links`*Jahr +
                                             `Gewaltbereite Links`*Jahr +
                                             im_pol , data = df_links)
summary(model_tokengewalt_links_extended)


PseudoR2(model_tokengewalt_links_extended, c("Nagelkerke"))



model_tokenstraf_links_extended <- glm.nb(token_links ~ `Straftaten Links`*Jahr +
                                            `Personenpotential Links`*Jahr +
                                            `Gewaltbereite Links`*Jahr +
                                            im_pol  , data = df_links)
summary(model_tokenstraf_links_extended)

PseudoR2(model_tokenstraf_links_extended, c("Nagelkerke"))


df_links <- df_links %>% 
  mutate(rel_gewalt = gewalt2 / N_A * 10000)
#
#
# Modelle mit Zielvariable "gewalt2"


model_gewalt_links_extended <- glm.nb(gewalt2 ~ `Gewaltaten Links` *Jahr +
                                        `Personenpotential Links`*Jahr +
                                        `Gewaltbereite Links`*Jahr +
                                        im_pol +
                                        offset(log(token_links)), data = df_links)
summary(model_gewalt_links_extended)

PseudoR2(model_gewalt_links_extended, c("Nagelkerke"))

model_straf_links_extended <- glm.nb(gewalt2 ~ `Straftaten Links`*Jahr +
                                       `Personenpotential Links`*Jahr +
                                       `Gewaltbereite Links`*Jahr +
                                       im_pol+
                                       offset(log(token_links)), data = df_links)
summary(model_straf_links_extended)


PseudoR2(model_straf_links_extended, c("Nagelkerke"))

#
#
#
# Modelle mit Zielvariable "orga"

model_orga_links_extended <- glm.nb(orga ~ `Gewaltaten Links`*Jahr +
                                      `Personenpotential Links`*Jahr +
                                      `Gewaltbereite Links`*Jahr +
                                      im_pol+
                                      offset(log(token_links)) , data = df_links)

PseudoR2(model_orga_links_extended, c("Nagelkerke"))
summary(model_orga_links_extended)


model_straf_orga_links_extended <- glm.nb(orga ~ `Straftaten Links`*Jahr +
                                            `Personenpotential Links`*Jahr +
                                            `Gewaltbereite Links`*Jahr +
                                            im_pol+
                                            offset(log(token_links)) , data = df_links)
summary(model_straf_orga_links_extended)



PseudoR2(model_straf_orga_links_extended, c("Nagelkerke"))

