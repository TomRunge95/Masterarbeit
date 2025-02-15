test <- test %>%
  mutate(
    gewaltN_links_gewalt = if_else(Gewalttaten_Links > 0, gewalt_n_links / Gewalttaten_Links, NA_real_),
    gewaltN_rechts_gewalt = if_else(Gewalttaten_Rechts > 0, gewalt_n_rechts / Gewalttaten_Rechts, NA_real_),
    gewaltN_links_straf = if_else(Straftaten_Links > 0, gewalt_n_links / Straftaten_Links, NA_real_),
    gewaltN_rechts_straf = if_else(Straftaten_Rechts > 0, gewalt_n_rechts / Straftaten_Rechts, NA_real_)
  )

bundesländer <- test %>%  
  filter(bundesland != "Bund")

gg2 <- ggplot(bundesländer, aes(x = jahr)) +
  geom_line(aes(y = gewaltN_links_gewalt, color = "Verhältnis Nennung Gewalttaten Links")) +
  geom_point(aes(y = gewaltN_links_gewalt, color = "Verhältnis Nennung Gewalttaten Links"), size = 0.5) +
  geom_line(aes(y = gewaltN_rechts_gewalt, color = "Verhältnis Nennung Gewalttaten Rechts")) +
  geom_point(aes(y = gewaltN_rechts_gewalt, color = "Verhältnis Nennung Gewalttaten Rechts"), size = 0.5) +
  labs(x = "",
       y = "",
       color = "") +
  
  scale_color_manual(values = c("Verhältnis Nennung Gewalttaten Links" = "red", "Verhältnis Nennung Gewalttaten Rechts" = "darkblue"),
                     labels = c("Verhältnis Nennung Gewalttaten Linksextremismus", "Verhältnis Nennung Gewalttaten Rechtsextremismus")) +
  
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


ggsave_pref(filename = "Verhältnis_Nennung_Gewalttaten.png",
            device = png,
            bg = "white",
            height = 30)






bundesländer <- test %>%  
  filter(bundesland != "Bund")

gg2 <- ggplot(bundesländer, aes(x = jahr)) +
  geom_line(aes(y = gewaltN_links_straf, color = "Verhältnis Nennung Straftaten Links")) +
  geom_point(aes(y = gewaltN_links_straf, color = "Verhältnis Nennung Straftaten Links"), size = 0.5) +
  geom_line(aes(y = gewaltN_rechts_straf, color = "Verhältnis Nennung Straftaten Rechts")) +
  geom_point(aes(y = gewaltN_rechts_straf, color = "Verhältnis Nennung Straftaten Rechts"), size = 0.5) +
  labs(x = "",
       y = "",
       color = "") +
  
  scale_color_manual(values = c("Verhältnis Nennung Straftaten Links" = "red", "Verhältnis Nennung Straftaten Rechts" = "darkblue"),
                     labels = c("Verhältnis Nennung Straftaten Linksextremismus", "Verhältnis Nennung Straftaten Rechtsextremismus")) +
  
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

ggsave_pref(filename = "Verhältnis_Nennung_Straftaten.png",
            device = png,
            bg = "white",
            height = 30)








gg2 <- ggplot(bund, aes(x = jahr)) +
  geom_line(aes(y = gewaltN_links_gewalt, color = "Verhältnis Nennung Gewalttaten Links")) +
  geom_point(aes(y = gewaltN_links_gewalt, color = "Verhältnis Nennung Gewalttaten Links"), size = 1) +
  geom_line(aes(y = gewaltN_rechts_gewalt, color = "Verhältnis Nennung Gewalttaten Rechts")) +
  geom_point(aes(y = gewaltN_rechts_gewalt, color = "Verhältnis Nennung Gewalttaten Rechts"), size = 1) +
  labs(x = "",
       y = "",
       color = "") +
  
  scale_color_manual(values = c("Verhältnis Nennung Gewalttaten Links" = "red", "Verhältnis Nennung Gewalttaten Rechts" = "darkblue"),
                     labels = c("Verhältnis Nennung Gewalttaten Linksextremismus", "Verhältnis Nennung Gewalttaten Rechtsextremismus")) +
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        legend.position = "bottom",  # Positioniere die Legende unten
        panel.spacing = unit(2, "lines"),
        legend.key.size = unit(0.5, "cm"),
        legend.box = element_blank(),  # Entfernt den schwarzen Kasten um die Legende
        legend.title = element_text(size = 6),  # Schriftgröße der Legende
        legend.background = element_blank(),  # Entfernt den Hintergrund der Legende
        legend.key = element_blank()) +
  
  theme(strip.background = element_blank()) 

ggsave_pref(filename = "Verhältnis_Nennung_Gewalttaten_Bund.png",
            device = png,
            bg = "white",
            height = 16)






gg2 <- ggplot(bund, aes(x = jahr)) +
  geom_line(aes(y = token_links_straf, color = "Verhältnis Token Straftaten Links")) +
  geom_point(aes(y = token_links_straf, color = "Verhältnis Token Straftaten Links"), size = 1) +
  geom_line(aes(y = token_rechts_straf, color = "Verhältnis Token Straftaten Rechts")) +
  geom_point(aes(y = token_rechts_straf, color = "Verhältnis Token Straftaten Rechts"), size = 1) +
  labs(x = "",
       y = "",
       color = "") +
  
  scale_color_manual(values = c("Verhältnis Token Straftaten Links" = "red", "Verhältnis Token Straftaten Rechts" = "darkblue"),
                     labels = c("Verhältnis Token Straftaten Linksextremismus", "Verhältnis Token Straftaten Rechtsextremismus")) +
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        legend.position = "bottom",  # Positioniere die Legende unten
        panel.spacing = unit(2, "lines"),
        legend.key.size = unit(0.5, "cm"),
        legend.box = element_blank(),  # Entfernt den schwarzen Kasten um die Legende
        legend.title = element_text(size = 6),  # Schriftgröße der Legende
        legend.background = element_blank(),  # Entfernt den Hintergrund der Legende
        legend.key = element_blank()) +
  
  
  theme(strip.background = element_blank()) 

ggsave_pref(filename = "Token_Straftaten_Bund.png",
            device = png,
            bg = "white",
            height = 16)

