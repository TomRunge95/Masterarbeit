library(MASS)



df_rechts$Regierung[df_rechts$docid == "vsbericht_Baden-Würtemberg_rechts_2022.txt"] <- "Bündnis 90/Die Grünen, CDU"
df_rechts$Innenminister[df_rechts$docid == "vsbericht_Baden-Würtemberg_rechts_2022.txt"] <- "CDU"


df_rechts$Regierung[df_rechts$docid == "vsbericht_Berlin_rechts_2022.txt"] <- "SPD, Die Linke, Bündnis 90/Die Grünen"
df_rechts$Innenminister[df_rechts$docid == "vsbericht_Berlin_rechts_2022.txt"] <- "SPD"

df_rechts <- df_rechts %>% 
  mutate(im_pol = case_when(
    Innenminister %in% c("CDU", "CSU", "FDP", "PRO") ~ 0,
    Innenminister == "SPD" ~ 1,
    TRUE ~ NA_real_  # Falls andere Werte in der Spalte vorkommen
  ))


df_rechts <- df_rechts %>% 
  filter(!Jahr == 2000)



model_tokengewalt_rechts_extended <- glm.nb(token_rechts ~ `Gewalttaten rechts`*Jahr +
                                              `Personenpotential rechts`*Jahr +
                                              `Gewaltbereite Rechts`*Jahr  +
                                              im_pol , data = df_rechts)
summary(model_tokengewalt_rechts_extended)

PseudoR2(model_tokengewalt_rechts_extended, c("Nagelkerke"))


model_tokenstraf_rechts_extended <- glm.nb(token_rechts ~ `Straftaten rechts`*Jahr +
                                             `Personenpotential rechts`*Jahr +
                                             `Gewaltbereite Rechts`*Jahr +
                                             im_pol , data = df_rechts)
summary(model_tokenstraf_rechts_extended)


PseudoR2(model_tokenstraf_rechts_extended, c("Nagelkerke"))







#
#
# Modelle mit Zielvariable "gewalt2"

model_gewalt_rechts_extended <- glm.nb(gewalt2 ~ `Gewalttaten rechts`*Jahr +
                                         `Personenpotential rechts`*Jahr +
                                         `Gewaltbereite Rechts`*Jahr +
                                         im_pol+
                                         offset(log(token_rechts)) , data = df_rechts)
summary(model_gewalt_rechts_extended)

PseudoR2(model_gewalt_rechts_extended, c("Nagelkerke"))

model_straf_rechts_extended <- glm.nb(gewalt2 ~ `Straftaten rechts`*Jahr +
                                        `Personenpotential rechts`*Jahr +
                                        `Gewaltbereite Rechts`*Jahr +
                                        im_pol+
                                        offset(log(token_rechts))  , data = df_rechts)

summary(model_straf_rechts_extended)
PseudoR2(model_straf_rechts_extended, c("Nagelkerke"))


#
#
#
# Modelle mit Zielvariable "orga"

model_orga_rechts_extended <- glm.nb(orga ~ `Gewalttaten rechts`*Jahr +
                                       `Personenpotential rechts`*Jahr +
                                       `Gewaltbereite Rechts`*Jahr +
                                       im_pol+
                                       offset(log(token_rechts)) , data = df_rechts)
summary(model_orga_rechts_extended)

PseudoR2(model_orga_rechts_extended, c("Nagelkerke"))


model_straf_orga_rechts_extended <- glm.nb(orga ~ `Straftaten rechts`*Jahr +
                                             `Personenpotential rechts`*Jahr +
                                             `Gewaltbereite Rechts`*Jahr +
                                             im_pol+
                                             offset(log(token_rechts)),
                                           data = df_rechts)
summary(model_straf_orga_rechts_extended)


PseudoR2(model_straf_orga_rechts_extended, c("Nagelkerke"))



