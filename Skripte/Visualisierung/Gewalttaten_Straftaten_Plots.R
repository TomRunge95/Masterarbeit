
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
  
  