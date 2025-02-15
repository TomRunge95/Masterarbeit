
test <- test %>%
  mutate(
    token_links_gewalt = if_else(Gewalttaten_Links > 0, token_Links / Gewalttaten_Links, NA_real_),
    token_rechts_gewalt = if_else(Gewalttaten_Rechts > 0, token_Rechts / Gewalttaten_Rechts, NA_real_),
    token_links_straf = if_else(Straftaten_Gesamt_Links  > 0, token_Links / Straftaten_Gesamt_Links, NA_real_),
    token_rechts_straf = if_else(Straftaten_Gesamt_Rechts  > 0, token_Rechts / Straftaten_Gesamt_Rechts, NA_real_)
  )

bundesländer <- test %>%  
  filter(bundesland != "Bund")

gg2 <- ggplot(bundesländer, aes(x = jahr)) +
  geom_line(aes(y = token_links_gewalt, color = "Verhältnis Token Gewalttaten Links")) +
  geom_point(aes(y = token_links_gewalt, color = "Verhältnis Token Gewalttaten Links"), size = 0.5) +
  geom_line(aes(y = token_rechts_gewalt, color = "Verhältnis Token Gewalttaten Rechts")) +
  geom_point(aes(y = token_rechts_gewalt, color = "Verhältnis Token Gewalttaten Rechts"), size = 0.5) +
  labs(x = "",
       y = "Verhältnis Token/Gewaltaten",
       color = "") +
  
  scale_color_manual(values = c("Verhältnis Token Gewalttaten Links" = "red", "Verhältnis Token Gewalttaten Rechts" = "darkblue"),
                     labels = c("Verhältnis Token Gewalttaten Linksextremismus", "Verhältnis Token Gewalttaten Rechtsextremismus")) +
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        legend.position = "bottom",  # Positioniere die Legende unten
        panel.spacing = unit(2, "lines"),
        legend.key.size = unit(0.5, "cm"),
        legend.box = element_blank(),  # Entfernt den schwarzen Kasten um die Legende
        legend.title = element_text(size = 6),  # Schriftgröße der Legende
        legend.background = element_blank(),  # Entfernt den Hintergrund der Legende
        legend.key = element_blank()) +
  
  facet_wrap(~bundesland, ncol = 3) +
  
  theme(strip.background = element_blank()) +
  coord_cartesian(ylim = c(0, 1250))  # Setzt den Y-Achsenbereich auf 0 bis 500 ohne die Daten zu beschneiden


ggsave_pref(filename = "Token_Gewalttaten.svg",
            device = svg,
            bg = "white",
            height = 30)

bundesländer <- test %>%  
  filter(bundesland != "Bund")

gg2 <- ggplot(bundesländer, aes(x = jahr)) +
  geom_line(aes(y = token_links_straf, color = "Verhältnis Token Straftaten Links")) +
  geom_point(aes(y = token_links_straf, color = "Verhältnis Token Straftaten Links"), size = 0.5) +
  geom_line(aes(y = token_rechts_straf, color = "Verhältnis Token Straftaten Rechts")) +
  geom_point(aes(y = token_rechts_straf, color = "Verhältnis Token Straftaten Rechts"), size = 0.5) +
  labs(x = "",
       y = "Verhältnis Token/Gewalttaten",
       color = "") +
  
  scale_color_manual(values = c("Verhältnis Token Straftaten Links" = "red", "Verhältnis Token Straftaten Rechts" = "darkblue"),
                     labels = c("Linksextremismus", "Rechtsextremismus")) +
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        legend.position = "bottom",  # Positioniere die Legende unten
        panel.spacing = unit(2, "lines"),
        legend.key.size = unit(0.5, "cm"),
        legend.box = element_blank(),  # Entfernt den schwarzen Kasten um die Legende
        legend.title = element_text(size = 6),  # Schriftgröße der Legende
        legend.background = element_blank(),  # Entfernt den Hintergrund der Legende
        legend.key = element_blank()) +
  
  facet_wrap(~bundesland, ncol = 3) +
  
  theme(strip.background = element_blank()) 

ggsave_pref(filename = "Token_Straftaten.svg",
            device = svg,
            bg = "white",
            height = 30)



bund <- test %>%  
  filter(bundesland == "Bund")




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

