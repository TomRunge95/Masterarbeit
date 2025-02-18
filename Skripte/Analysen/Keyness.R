
library(quanteda)
library(spacyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(Cairo)
library(quanteda.textstats)
library(quanteda.textplots)
library(cowplot)
library(officer)
library(dplyr)
library(stringr)
library(purrr)
library(patchwork)

source("../Visualisierung/GG_Layout.R")

load("../../Daten/links_parse.RDATA")
load("../../Daten/rechts_parse.RDATA")

load("../../Daten/compound_pattern_links.RDATA")
load("../../Daten/compound_pattern_rechts.RDATA")



stops <- readLines("../../Daten/german_stopwords_plain.txt")


parse_tolower <- function(parsed_object){
  parsed_object%>% 
    mutate(lemma = str_to_lower(lemma)) %>% 
    mutate(token = str_to_lower(token))}

Links_Parse <- parse_tolower(links_parse_filter)
Rechts_Parse <- parse_tolower(rechts_parse_filter)

links_parse_filter <- Links_Parse %>% 
  filter(pos != "SPACE" & pos != "PUNCT" & pos != "NUM") %>% 
  filter(!(str_length(token) == 1 | 
             str_detect(token, "^[a-zA-Z]\\."))) %>%
  filter(!grepl("2000", doc_id)) %>%
  mutate(token = str_replace_all(token, "(?<=\\b)[\\x{0080}-\\x{009F}]+|[\\x{0080}-\\x{009F}]+(?=\\b)", ""))



rechts_parse_filter <- Rechts_Parse %>% 
  filter(pos != "SPACE" & pos != "PUNCT" & pos != "NUM") %>% 
  filter(!(str_length(token) == 1 | 
             str_detect(token, "^[a-zA-Z]\\."))) %>%
  filter(!grepl("2000", doc_id)) %>%
  mutate(token = str_replace_all(token, "(?<=\\b)[\\x{0080}-\\x{009F}]+|[\\x{0080}-\\x{009F}]+(?=\\b)", ""))




filter_pos <- function(parsed_object){
  List_pos <- list(
    ALL = parsed_object,
    NOUN = filter(parsed_object, pos %in% c("NOUN", "PROPN")),
    ADJ_ADV = filter(parsed_object, pos %in% c("ADV", "ADJ")),
    ADJ = filter(parsed_object, pos %in% c("ADJ")),
    VERB = filter(parsed_object, pos == "VERB")
  )
  return(List_pos)
}

Links_pos <- filter_pos(links_parse_filter)
Rechts_pos <- filter_pos(rechts_parse_filter)

rm(Links_Parse, Rechts_Parse,Links_Parse_filter,Rechts_Parse_filter)

token_list_links <- list()

for(names in names(Links_pos)) {
  token_links <- as.tokens(Links_pos[[names]], lemma = T) %>% 
    tokens_remove(pattern = stops) %>% 
    tokens_compound(pattern = as.phrase(compound_pattern_links))
  token_links <- tokens_tolower(token_links)
  token_list_links[[names]] <- token_links
}


dfm_list_links <-list(ALL = dfm(token_list_links$ALL),
                      NOUN = dfm(token_list_links$NOUN),
                      ADJ_ADV = dfm(token_list_links$ADJ_ADV),
                      ADJ = dfm(token_list_links$ADJ),
                      VERB = dfm(token_list_links$VERB))





token_list_rechts <- list()

for(names in names(Rechts_pos)) {
  token_rechts <- as.tokens(Rechts_pos[[names]], lemma = T) %>% 
    tokens_remove(pattern = stops) %>% 
    tokens_compound(pattern = as.phrase(compound_pattern_rechts))
  token_rechts <- tokens_tolower(token_rechts)
  
  token_list_rechts[[names]] <- token_rechts
}

dfm_list_rechts <-list(ALL = dfm(token_list_rechts$ALL),
                       NOUN = dfm(token_list_rechts$NOUN),
                       ADJ_ADV = dfm(token_list_rechts$ADJ_ADV),
                       ADJ = dfm(token_list_rechts$ADJ),
                       VERB = dfm(token_list_rechts$VERB))





split_names <- str_split(docnames(dfm_list_links$ALL), "_", simplify = TRUE)

# Füge die Dokumentenvariablen hinzu
docvars(dfm_list_links$ALL) <- data.frame(
  Bundesland = split_names[, 2],                  # Region
  Kapitel = split_names[, 3],                    # Seite (links/rechts)
  Jahr = str_remove(split_names[, 4], ".txt") # Jahr (ohne .txt)
)

Keyness_DFM_links <- dfm_list_links$ALL
docvars(Keyness_DFM_links)
# Berechnung der Keyness und Erstellen des Plots
period <- ifelse(docvars(Keyness_DFM_links, "Jahr") >= 2011, "Nach 2011", "Vor 2011")
docvars(Keyness_DFM_links, "period") <- period





Keyness_DFM_links <- Keyness_DFM_links %>% 
  dfm_group(group = period)
# Korrekte Definition des target-Arguments für textstat_keyness
tstat_key <- textstat_keyness(Keyness_DFM_links)

max(tstat_key$chi2)
min(tstat_key$chi2) 

links <- -4000  # Nächst niedriger Tausender für den Minimum
rechts <- 4000  # Nächst höherer Tausender für das Maximum

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



ggsave_pref(filename = "../Visualisierung/links_keyness.svg",
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


ggsave_pref(filename = "../Visualisierung/rechts_keyness.svg",
            device = svg,
            height = 11)


