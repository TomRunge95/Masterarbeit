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

source("../Analysen/TokenAnzahl.R")


source("../Visualisierung/GG_Layout.R")


gg_token_mean <- ggplot(mittelwerte_nach_jahr, aes(x = jahr)) +
  geom_hline(yintercept = 10000, color = "white", size = 1.5) +  # Hline zuerst hinzufügen
  geom_line(aes(y = mean_token_Links, color = "Token Links")) +
  geom_point(aes(y = mean_token_Links, color = "Token Links"), size = 0.5) +
  geom_line(aes(y = mean_token_Rechts, color = "Token Rechts")) +
  geom_point(aes(y = mean_token_Rechts, color = "Token Rechts"), size = 0.5) +
  labs(x = "",
       y = "Tokenanzahl",
       color = "") +
  
  scale_color_manual(values = c("Token Links" = "red", "Token Rechts" = "darkblue"),
                     labels = c("Linksextremismus", "Rechtsextremismus")) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 15), angle = 90, size = 8),
        legend.position = "bottom") +
  guides(color = guide_legend(title = "")) 

# Speichern des Plots
ggsave_pref(gg_token_mean, filename = "../Visualisierung/Token_Mean_Links_Rechts.svg",
            device = svg,
            height = 10)

mittelwerte_nach_bund <- mittelwerte_nach_bund %>%
  mutate(difference = abs(mean_token_Links - mean_token_Rechts)) %>%
  group_by(bundesland) %>% 
  summarize(
    mean_token_Links_sum = sum(mean_token_Links, na.rm = TRUE),
    mean_token_Rechts_sum = sum(mean_token_Rechts, na.rm = TRUE),
    difference_sum = sum(difference, na.rm = TRUE)
  ) %>% 
  arrange(desc(difference_sum))

# Reihenfolge im DataFrame beibehalten
library(forcats)

# Bundesländer basierend auf den Werten für mean_token_Rechts_sum sortieren
mittelwerte_nach_bund <- mittelwerte_nach_bund %>%
  mutate(bundesland = fct_reorder(bundesland, difference_sum))

gg_token_länder <- ggplot(mittelwerte_nach_bund) +
  geom_segment(aes(x = mean_token_Links_sum, xend = mean_token_Rechts_sum,
                   y = bundesland, yend = bundesland)) +
  geom_point(aes(x = mean_token_Links_sum, y = bundesland, color = "Token Links"), size = 2) +
  geom_point(aes(x = mean_token_Rechts_sum, y = bundesland, color = "Token Rechts"), size = 2) +
  geom_text(aes(x = (mean_token_Links_sum + mean_token_Rechts_sum) / 2,  # Position zwischen den Punkten
                y = bundesland,
                label = round(difference_sum, -3)),  # Differenz anzeigen, gerundet auf eine Nachkommastelle
            color = "black", size = 2, vjust = -0.75) +  # Textformatierung
  labs(x = "Durchschnittliche Tokenanzahl",
       y = "",
       color = "") +
  
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10),  # Passt die Anzahl der Ticks an
                     labels = scales::label_number(accuracy = 1, big.mark = ".", decimal.mark = ",")) +  # Verhindert wissenschaftliche Notation
  scale_color_manual(values = c("Token Links" = "red", "Token Rechts" = "darkblue"),
                     labels = c("Kapitel Linksextremismus", "Kapitel Rechtsextremismus")) +
  theme(plot.title = element_text(hjust = -1),
        axis.title.y = element_text(margin = margin(r = 15)),
        legend.position = "bottom") +
  guides(color = guide_legend(title = ""))


ggsave_pref(gg_token_länder, filename = "../Visualisierung/Bundesland_Dumbell2.svg",
            device = svg,
            height = 13)