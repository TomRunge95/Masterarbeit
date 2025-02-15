setwd("C:/Users/runge/Desktop/Masterarbeit")

library(quanteda)
library(spacyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(Cairo)
library(quanteda.textstats)
library(cowplot)
library(officer)
library(dplyr)
library(stringr)
library(patchwork)
library(spacyr)




source("Funktionen.R")
source("Dezim_Layout.R")



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

gg2 <- ggplot(bundesländer, aes(x = jahr)) +
  geom_line(aes(y = token_links_gewalt, color = "Verhältnis Token Gewalttaten Links")) +
  geom_point(aes(y = token_links_gewalt, color = "Verhältnis Token Gewalttaten Links"), size = 0.5) +
  geom_line(aes(y = token_rechts_gewalt, color = "Verhältnis Token Gewalttaten Rechts")) +
  geom_point(aes(y = token_rechts_gewalt, color = "Verhältnis Token Gewalttaten Rechts"), size = 0.5) +
  labs(x = "",
       y = "",
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



gg2 <- ggplot(bundesländer, aes(x = jahr)) +
  geom_line(aes(y = Straftaten_Links, color = "Gewalttaten_Links")) +
  geom_point(aes(y = Straftaten_Links, color = "Gewalttaten_Links"), size = 0.5) +
  geom_line(aes(y = Straftaten_Rechts, color = "Gewalttaten_Rechts")) +
  geom_point(aes(y = Straftaten_Rechts, color = "Gewalttaten_Rechts"), size = 0.5) +
  labs(x = "",
       y = "",
       color = "") +
  
  scale_color_manual(values = c("Gewalttaten_Links" = "red", "Gewalttaten_Rechts" = "darkblue"),
                     labels = c("Gewalttaten Linksextremismus", "Gewalttaten Rechtsextremismus")) +
  
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
  theme(strip.background = element_blank())+
  
  theme(strip.background = element_blank()) +
  coord_cartesian(ylim = c(0, 500))  # Setzt den Y-Achsenbereich auf 0 bis 500 ohne die Daten zu beschneiden
# Entfernt den Hintergrund um die Facettentitel

ggsave_pref(filename = "Gewalttaten.svg",
            device = svg,
            bg = "white",
            height = 10)
  

  
  gg2 <- ggplot(bundesländer, aes(x = jahr)) +
    geom_line(aes(y = Straftaten_Links, color = "Straftaten_Links")) +
    geom_point(aes(y = Straftaten_Links, color = "Straftaten_Links"), size = 0.5) +
    geom_line(aes(y = Straftaten_Rechts, color = "Straftaten_Rechts")) +
    geom_point(aes(y = Straftaten_Rechts, color = "Straftaten_Rechts"), size = 0.5) +
    labs(x = "",
         y = "",
         color = "") +
    
    scale_color_manual(values = c("Straftaten_Links" = "red", "Straftaten_Rechts" = "darkblue"),
                       labels = c("Straftaten Linksextremismus", "Straftaten Rechtsextremismus")) +
    
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

  ggsave_pref(filename = "Straftaten.svg",
              device = svg,
              bg = "white",
              height = 10)
  
  
  
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
  
  