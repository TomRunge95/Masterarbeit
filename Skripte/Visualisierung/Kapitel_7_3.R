source("../Analysen/reg_model_links.R")
source("../Analysen/reg_model_rechts.R")
library(stargazer)


source("../Visualisierung/GG_Layout.R")



load("../../Daten/Links_plus.RDATA")
load("../../Daten/Rechts_plus.RDATA")

load("../../Daten/links_parse.RDATA")
load("../../Daten/rechts_parse.RDATA")



# Häufigkeit der Tokens zählen und nach doc_id gruppieren
result_links <- links_parse_filter %>%
  group_by(doc_id, token) %>%   # Gruppiere nach doc_id und token
  summarize(token_count = n()) %>%  # Zähle die Anzahl der Tokens
  ungroup() %>%                  # Ungroup für nachfolgende Operationen
  group_by(doc_id) %>%
  filter(!grepl("2000", doc_id)) %>% 
  summarize(reihen_summe = sum(token_count))  # Summiere die Häufigkeiten

sum(result_links$reihen_summe)

# Häufigkeit der Tokens zählen und nach doc_id gruppieren
result_rechts <- rechts_parse_filter %>%
  group_by(doc_id, token) %>%   # Gruppiere nach doc_id und token
  summarize(token_count = n()) %>%  # Zähle die Anzahl der Tokens
  ungroup() %>%                  # Ungroup für nachfolgende Operationen
  group_by(doc_id) %>%          # Erneute Gruppierung nur nach doc_id
  filter(!grepl("2000", doc_id)) %>% 
  summarize(reihen_summe = sum(token_count))  # Summiere die Häufigkeiten



test <- data.frame(doc_id = Links_plus$doc_id,
                   bundesland = Links_plus$bundesland,
                   jahr = Links_plus$Jahr,
                   Straftaten_Gesamt_Links = Links_plus$`Straftaten Gesamt Links`,
                   Gewalttaten_Links = Links_plus$`Gewaltaten Links`,
                   Straftaten_Links = Links_plus$`Straftaten Links`,
                   Personen_Links = Links_plus$`Personenpotential Links`,
                   Gewaltbereite_Links = Links_plus$`Gewaltbereite Links`,
                   token_Links = result_links$reihen_summe,
                   Straftaten_Gesamt_Rechts = Rechts_plus$`Straftaten gesamt recht`,
                   Gewalttaten_Rechts = Rechts_plus$`Gewalttaten rechts`,
                   Straftaten_Rechts = Rechts_plus$`Straftaten rechts`,
                   Personen_Rechts = Rechts_plus$`Personenpotential rechts`,
                   Gewaltbereite_Rechts = Rechts_plus$`Gewaltbereite Rechts`,
                   token_Rechts = result_rechts$reihen_summe)

test <- test %>%
  mutate(
    token_links_gewalt = if_else(Gewalttaten_Links > 0, token_Links / Gewalttaten_Links, NA_real_),
    token_rechts_gewalt = if_else(Gewalttaten_Rechts > 0, token_Rechts / Gewalttaten_Rechts, NA_real_),
    token_links_straf = if_else(Straftaten_Links > 0, token_Links / Straftaten_Links, NA_real_),
    token_rechts_straf = if_else(Straftaten_Rechts > 0, token_Rechts / Straftaten_Rechts, NA_real_)
  )

bundesländer <- test %>%  
  filter(bundesland != "Bund")


bund <- test %>%  
  filter(bundesland == "Bund")

gg2 <- ggplot(bund, aes(x = jahr)) +
  geom_line(aes(y = Gewalttaten_Links, color = "Gewalttaten Links")) +
  geom_point(aes(y = Gewalttaten_Links, color = "Gewalttaten Links"), size = 1) +
  
  
  geom_line(aes(y = Gewalttaten_Rechts, color = "Gewalttaten Rechts")) +
  geom_point(aes(y = Gewalttaten_Rechts, color = "Gewalttaten Rechts"), size = 1) +
  labs(x = "",
       y = "Gewalttaten",
       color = "") +
  
  scale_color_manual(values = c("Gewalttaten Links" = "red", "Gewalttaten Rechts" = "darkblue"),
                     labels = c("Linksextremismus", "Rechtsextremismus")) +
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 15), angle = 90, size = 8),
        legend.position = "bottom",  # Positioniere die Legende unten
        panel.spacing = unit(2, "lines"),
        legend.key.size = unit(0.5, "cm"),
        legend.box = element_blank(),  # Entfernt den schwarzen Kasten um die Legende
        legend.title = element_text(size = 6),  # Schriftgröße der Legende
        legend.background = element_blank(),  # Entfernt den Hintergrund der Legende
        legend.key = element_blank())


ggsave_pref(filename = "Gewalttaten_Bund.svg",
            device = svg,
            bg = "white",
            height = 9)




gg2 <- ggplot(bund, aes(x = jahr)) +
  geom_line(aes(y = Straftaten_Links, color = "Straftaten_Links")) +
  geom_point(aes(y = Straftaten_Links, color = "Straftaten_Links"), size = 1) +
  geom_line(aes(y = Straftaten_Rechts, color = "Straftaten_Rechts")) +
  geom_point(aes(y = Straftaten_Rechts, color = "Straftaten_Rechts"), size = 1) +
  labs(x = "",
       y = "Straftaten",
       color = "") +
  
  scale_color_manual(values = c("Straftaten_Links" = "red", "Straftaten_Rechts" = "darkblue"),
                     labels = c("Linksextremismus", "Rechtsextremismus")) +
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 15), angle = 90, size = 8),
        legend.position = "bottom",  # Positioniere die Legende unten
        panel.spacing = unit(2, "lines"),
        legend.key.size = unit(0.5, "cm"),
        legend.box = element_blank(),# Optionale Anpassung der Größe der Legenden-Symbole
        legend.title = element_text(size = 6),  # Schriftgröße der Legende
        legend.background = element_blank(),  # Entfernt den Hintergrund der Legende (der schwarze Kasten)
        legend.key = element_blank()) 


ggsave_pref(filename = "Straftaten_Bund.svg",
            device = svg,
            bg = "white",
            height = 9)



gg2 <- ggplot(bund, aes(x = jahr)) +
  geom_line(aes(y = Personen_Links , color = "Personen_Links")) +
  geom_point(aes(y = Personen_Links , color = "Personen_Links"), size = 0.5) +
  geom_line(aes(y = Personen_Rechts , color = "Personen_Rechts")) +
  geom_point(aes(y = Personen_Rechts , color = "Personen_Rechts"), size = 0.5) +
  labs(x = "",
       y = "Personenpotenzial",
       color = "") +
  
  scale_color_manual(values = c("Personen_Links" = "red", "Personen_Rechts" = "darkblue"),
                     labels = c("Linksextremismus", "Rechtsextremismus")) +
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 15), angle = 90, size = 8),
        legend.position = "bottom",  # Positioniere die Legende unten
        panel.spacing = unit(2, "lines"),
        legend.key.size = unit(0.5, "cm"),
        legend.box = element_blank(),  # Entfernt den schwarzen Kasten um die Legende
        legend.background = element_blank(),  # Entfernt den Hintergrund der Legende
        legend.key = element_blank())


ggsave_pref(filename = "Personenpotenzial_Bund.svg",
            device = svg,
            bg = "white",
            height = 10)




gg3 <- ggplot(bund, aes(x = jahr)) +
  geom_line(aes(y = Gewaltbereite_Links , color = "Gewaltbereite_Links")) +
  geom_point(aes(y = Gewaltbereite_Links , color = "Gewaltbereite_Links"), size = 0.5) +
  geom_line(aes(y = Gewaltbereite_Rechts , color = "Gewaltbereite_Rechts")) +
  geom_point(aes(y = Gewaltbereite_Rechts , color = "Gewaltbereite_Rechts"), size = 0.5) +
  labs(x = "",
       y = "Gewaltorientierte",
       color = "") +
  
  scale_color_manual(values = c("Gewaltbereite_Links" = "red", "Gewaltbereite_Rechts" = "darkblue"),
                     labels = c("Linksextremismus", "Rechtsextremismus")) +
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 15), angle = 90, size = 8),
        legend.position = "bottom",  # Positioniere die Legende unten
        panel.spacing = unit(2, "lines"),
        legend.key.size = unit(0.5, "cm"),
        legend.box = element_blank(),# Optionale Anpassung der Größe der Legenden-Symbole
        legend.background = element_blank(),  # Entfernt den Hintergrund der Legende (der schwarze Kasten)
        legend.key = element_blank()) 


ggsave_pref(filename = "Gewaltbereite_Bund.svg",
            device = svg,
            bg = "white",
            height = 10)


gg2 <- ggplot(bund, aes(x = jahr)) +
  geom_line(aes(y = token_links_gewalt, color = "Verhältnis Token Gewalttaten Links")) +
  geom_point(aes(y = token_links_gewalt, color = "Verhältnis Token Gewalttaten Links"), size = 1) +
  geom_line(aes(y = token_rechts_gewalt, color = "Verhältnis Token Gewalttaten Rechts")) +
  geom_point(aes(y = token_rechts_gewalt, color = "Verhältnis Token Gewalttaten Rechts"), size = 1) +
  labs(x = "",
       y = "Verhältnis Token/Gewaltaten",
       color = "") +
  
  scale_color_manual(values = c("Verhältnis Token Gewalttaten Links" = "red", "Verhältnis Token Gewalttaten Rechts" = "darkblue"),
                     labels = c(" Linksextremismus", "Rechtsextremismus")) +
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 15), angle= 90, size = 8),
        legend.position = "bottom",  # Positioniere die Legende unten
        panel.spacing = unit(2, "lines"),
        legend.key.size = unit(0.5, "cm"),
        legend.box = element_blank(),  # Entfernt den schwarzen Kasten um die Legende
        legend.title = element_text(size = 8),  # Schriftgröße der Legende
        legend.background = element_blank(),  # Entfernt den Hintergrund der Legende
        legend.key = element_blank()) +
  coord_cartesian(ylim = c(0, 40)) + # Setzt den Y-Achsenbereich auf 0 bis 500 ohne die Daten zu beschneiden
  theme(strip.background = element_blank()) 

ggsave_pref(filename = "Token_Gewalttaten_Bund.svg",
            device = svg,
            bg = "white",
            height = 10)






gg2 <- ggplot(bund, aes(x = jahr)) +
  geom_line(aes(y = token_links_straf, color = "Verhältnis Token Straftaten Links")) +
  geom_point(aes(y = token_links_straf, color = "Verhältnis Token Straftaten Links"), size = 1) +
  geom_line(aes(y = token_rechts_straf, color = "Verhältnis Token Straftaten Rechts")) +
  geom_point(aes(y = token_rechts_straf, color = "Verhältnis Token Straftaten Rechts"), size = 1) +
  labs(x = "",
       y = "Verhältnis Token/Straftaten",
       color = "") +
  
  scale_color_manual(values = c("Verhältnis Token Straftaten Links" = "red", "Verhältnis Token Straftaten Rechts" = "darkblue"),
                     labels = c("Linksextremismus", "Rechtsextremismus")) +
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 15), angle = 90, size =8),
        legend.position = "bottom",  # Positioniere die Legende unten
        panel.spacing = unit(2, "lines"),
        legend.key.size = unit(0.5, "cm"),
        legend.box = element_blank(),  # Entfernt den schwarzen Kasten um die Legende
        legend.title = element_text(size = 8),  # Schriftgröße der Legende
        legend.background = element_blank(),  # Entfernt den Hintergrund der Legende
        legend.key = element_blank()) +
  
  
  theme(strip.background = element_blank())  +
  coord_cartesian(ylim = c(0, 40))  # Setzt den Y-Achsenbereich auf 0 bis 500 ohne die Daten zu beschneiden


ggsave_pref(filename = "Token_Straftaten_Bund.svg",
            device = svg,
            bg = "white",
            height = 10)



export_transformed_models_with_pseudoR2 <- function(models, filename, intercept_transform = FALSE, type = "latex") {
  # Erstelle eine Liste der Nagelkerke Pseudo-R²-Werte
  pseudo_r2_values <- sapply(models, function(model) {
    # Berechne den Pseudo-R²-Wert für jedes Modell
    pseudo_r2 <- PseudoR2(model, c("Nagelkerke"))
    # Rückgabe des Nagelkerke-Werts, der als erster Wert im Vektor gespeichert wird
    pseudo_r2[1]  # PseudoR2 gibt einen Vektor zurück, der Nagelkerke als ersten Wert hat
  })
  
  
  # Ergebnisse mit stargazer exportieren und Nagelkerke-Werte hinzufügen
  library(stargazer)
  
  # Ergebnisse als latex-Tabelle erzeugen und die Nagelkerke Pseudo-R²-Werte hinzufügen
  stargazer(models, type = "latex", out = filename, single.row = F, style =  "default",
            add.lines = list(c("Nagelkerke Pseudo-R²", round(pseudo_r2_values, 4))), no.space = T)
  
  message("Export completed: ", filename)
}


summary(model_gewalt_links_extended)


export_transformed_models_with_pseudoR2(
  models = list(model_gewalt_links_extended, model_straf_links_extended),
  filename = "../Visualisierung/model_gewaltstraf_links_extended_with_pseudoR2.tex"
)

export_transformed_models_with_pseudoR2(
  models = list(model_tokengewalt_links_extended, model_tokenstraf_links_extended),
  filename = "../Visualisierung/model_tokengewaltstraf_links_extended_with_pseudoR2.tex"
)

export_transformed_models_with_pseudoR2(
  models = list(model_orga_links_extended, model_straf_orga_links_extended),
  filename = "../Visualisierung/model_straf_orga_links_basic_links_extended_with_pseudoR2.tex"
)





a <- export_transformed_models_with_pseudoR2(
  models = list(model_gewalt_rechts_extended, model_straf_rechts_extended),
  filename = "../Visualisierung/model_gewaltstraf_rechts_extended.tex"
)

export_transformed_models_with_pseudoR2(
  models = list(model_tokengewalt_rechts_extended, model_tokenstraf_rechts_extended),
  filename = "../Visualisierung/model_tokengewaltstraf_rechts_extended.tex"
)

export_transformed_models_with_pseudoR2(
  models = list(model_orga_rechts_extended, model_straf_orga_rechts_extended),
  filename = "../Visualisierung/model_orga_rechts_extended_extended.tex"
)



