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




gg3 <- ggplot(bundesländer, aes(x = jahr)) +
  geom_line(aes(y = Personen_Links , color = "Personen_Links")) +
  geom_point(aes(y = Personen_Links , color = "Personen_Links"), size = 0.5) +
  geom_line(aes(y = Personen_Rechts , color = "Personen_Rechts")) +
  geom_point(aes(y = Personen_Rechts , color = "Personen_Rechts"), size = 0.5) +
  labs(x = "",
       y = "",
       color = "") +
  
  scale_color_manual(values = c("Personen_Links" = "red", "Personen_Rechts" = "darkblue"),
                     labels = c("Personen", "Personen Rechtsextremismus")) +
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        legend.position = "bottom",  # Positioniere die Legende unten
        panel.spacing = unit(2, "lines"),
        legend.key.size = unit(0.5, "cm"),
        legend.box = element_blank(),# Optionale Anpassung der Größe der Legenden-Symbole
        legend.background = element_blank(),  # Entfernt den Hintergrund der Legende (der schwarze Kasten)
        legend.key = element_blank()) +  
  facet_wrap(~bundesland, ncol = 3) +
  theme(strip.background = element_blank())+
  
  theme(strip.background = element_blank()) +
  coord_cartesian(ylim = c(0, 6000))  # Setzt den Y-Achsenbereich auf 0 bis 500 ohne die Daten zu beschneiden
# Entfernt den Hintergrund um die Facettentitel

ggsave_pref(filename = "Personenpotential.svg",
            device = svg,
            bg = "white",
            height = 30)



gg2 <- ggplot(bundesländer, aes(x = jahr)) +
  geom_line(aes(y = Gewaltbereite_Links , color = "Gewaltbereite_Links")) +
  geom_point(aes(y = Gewaltbereite_Links , color = "Gewaltbereite_Links"), size = 0.5) +
  geom_line(aes(y = Gewaltbereite_Rechts , color = "Gewaltbereite_Rechts")) +
  geom_point(aes(y = Gewaltbereite_Rechts , color = "Gewaltbereite_Rechts"), size = 0.5) +
  labs(x = "",
       y = "Anzahl Gewaltorientierte",
       color = "") +
  
  scale_color_manual(values = c("Gewaltorientierte_Links" = "red", "Gewaltorientierte_Rechts" = "darkblue"),
                     labels = c("Gewaltorientierte Linksextremismus", "Gewaltorientierte Rechtsextremismus")) +
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        legend.position = "bottom",  # Positioniere die Legende unten
        panel.spacing = unit(2, "lines"),
        legend.key.size = unit(0.5, "cm"),
        legend.box = element_blank(),# Optionale Anpassung der Größe der Legenden-Symbole
        legend.title = element_text(size = 6),  # Schriftgröße der Legende
        legend.background = element_blank(),  # Entfernt den Hintergrund der Legende (der schwarze Kasten)
        legend.key = element_blank()) +  
  facet_wrap(~bundesland, ncol = 3) +
  theme(strip.background = element_blank())  # Entfernt den Hintergrund um die Facettentitel

ggsave_pref(filename = "Gewaltbereite.svg",
            device = svg,
            bg = "white",
            height = 30)



bund <- test %>%  
  filter(bundesland == "Bund")

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


combined <- cowplot::plot_grid(gg2,gg3, ncol= 2)

