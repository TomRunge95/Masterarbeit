source("../Analysen/reg_model_links.R")
source("../Analysen/reg_model_rechts.R")
library(stargazer)




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



