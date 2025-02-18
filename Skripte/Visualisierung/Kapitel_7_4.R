source("../Analysen/Keyness.R")

# Erstelle den Plot mit einer definierten Farb-Aesthetik
reference_links_2010 <- textplot_keyness(tstat_key, n = 20, margin = 0.5, labelsize = 2.5, color = c("red", "lightgrey"))
# Zeige den fertigen Plot

# Umrahmung und X-Achse anpassen
reference_links_2010 <- reference_links_2010 +
  xlab("Chi²") +
  theme(
    panel.border = element_blank(),
    legend.position = c(1, 0), # Positionierung: x = 1 (rechts), y = 0 (unten)
    legend.justification = c(1, 0) # Bezugspunkt der Legende (unten rechts)
  ) +
  xlim(c(links, rechts))

print(reference_links_2010)



ggsave_pref(filename = "links_keyness.svg",
            device = svg,
            height = 11)





split_names <- str_split(docnames(dfm_list_rechts$ALL), "_", simplify = TRUE)

# Füge die Dokumentenvariablen hinzu
docvars(dfm_list_rechts$ALL) <- data.frame(
  Bundesland = split_names[, 2],                  # Region
  Kapitel = split_names[, 3],                    # Seite (links/rechts)
  Jahr = str_remove(split_names[, 4], ".txt") # Jahr (ohne .txt)
)

Keyness_DFM_rechts <- dfm_list_rechts$ALL
docvars(Keyness_DFM_rechts)
# Berechnung der Keyness und Erstellen des Plots
period <- ifelse(docvars(Keyness_DFM_rechts, "Jahr") >= 2011, "Nach 2011", "Vor 2011")
docvars(Keyness_DFM_rechts, "period") <- period

Keyness_DFM_rechts <- Keyness_DFM_rechts %>% 
  dfm_group(group = period)
# Korrekte Definition des target-Arguments für textstat_keyness
tstat_key <- textstat_keyness(Keyness_DFM_rechts)


min(tstat_key$chi2) 
max(tstat_key$chi2)
links <- -4000
rechts <- 4000

# Erstelle den Plot mit einer definierten Farb-Aesthetik
# Erstelle den Plot ohne Rahmen und mit angepasster X-Achse
reference_rechts_2010 <- textplot_keyness(tstat_key, n = 20, labelsize = 2.5, margin = 0.5, color = c("darkblue", "lightgrey"))

# Umrahmung und X-Achse anpassen
reference_rechts_2010 <- reference_rechts_2010 +
  xlab("Chi²") +
  theme(
    panel.border = element_blank(),
    legend.position = c(1, 0), # Positionierung: x = 1 (rechts), y = 0 (unten)
    legend.justification = c(1, 0) # Bezugspunkt der Legende (unten rechts)
  ) +
  xlim(c(links, rechts))


ggsave_pref(filename = "rechts_keyness.svg",
            device = svg,
            height = 11)


