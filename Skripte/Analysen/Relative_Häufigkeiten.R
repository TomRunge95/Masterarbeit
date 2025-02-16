
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
library(purrr)
library(patchwork)


stops <- readLines("../../Daten/german_stopwords_plain.txt")


load("../../Daten/compound_pattern_links.RDATA")
load("../../Daten/compound_pattern_rechts.RDATA")

load("../../Daten/Links_plus.RDATA")
load("../../Daten/Rechts_plus.RDATA")


load("../../Daten/links_parse.RDATA")
load("../../Daten/rechts_parse.RDATA")



Corpus_Links <- corpus(Links_plus)
Corpus_Rechts <- corpus(Rechts_plus)


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


links_parse_filter$token <- tolower(links_parse_filter$token) 
links_parse_filter <- links_parse_filter %>%  filter(!grepl("2000", doc_id)) 

rechts_parse_filter$token <- tolower(rechts_parse_filter$token)
rechts_parse_filter <- rechts_parse_filter %>%  filter(!grepl("2000", doc_id)) 



links_parse_filter <- links_parse_filter %>% 
  filter(pos != "SPACE" & pos != "PUNCT" & pos != "NUM") %>% 
  filter(!(str_length(token) == 1 | 
             str_detect(token, "^[a-zA-Z]\\."))) %>%
  # mutate(token = tolower(token)) %>%  # zu Kleinbuchstaben umwandeln
  mutate(token = str_replace_all(token, "(?<=\\b)[\\x{0080}-\\x{009F}]+|[\\x{0080}-\\x{009F}]+(?=\\b)", ""))




rechts_parse_filter <- rechts_parse_filter %>% 
  filter(pos != "SPACE" & pos != "PUNCT" & pos != "NUM") %>% 
  filter(!(str_length(token) == 1 | 
             str_detect(token, "^[a-zA-Z]\\."))) %>%
  # mutate(token = tolower(token)) %>%  # zu Kleinbuchstaben umwandeln
  mutate(token = str_replace_all(token, "(?<=\\b)[\\x{0080}-\\x{009F}]+|[\\x{0080}-\\x{009F}]+(?=\\b)", ""))


Links_pos <- filter_pos(links_parse_filter)
Rechts_pos <- filter_pos(rechts_parse_filter)


token_list_links <- list()

for (names in names(Links_pos)) {
  # Konvertiere die 'lemma'-Spalte in Kleinschreibung innerhalb des jeweiligen Dataframes
  Links_pos[[names]]$token <- tolower(Links_pos[[names]]$lemma)
  
  # Erstelle die Tokens aus dem jeweiligen Dataframe in Links_pos[[names]]
  token_links <- as.tokens(Links_pos[[names]], lemma = TRUE) %>% 
    tokens_remove(pattern = stops) %>% 
    tokens_compound(pattern = as.phrase(compound_pattern_links))
  
  # Wandelt die Tokens in Kleinschreibung um
  token_links <- tokens_tolower(token_links)
  
  # Speichert die Tokens in der Liste 'token_list_links'
  token_list_links[[names]] <- token_links
}


dfm_list_links <-list(ALL = dfm(token_list_links$ALL),
                      NOUN = dfm(token_list_links$NOUN),
                      ADJ_ADV = dfm(token_list_links$ADJ_ADV),
                      ADJ = dfm(token_list_links$ADJ),
                      VERB = dfm(token_list_links$VERB))


list_links_gewichtet <- lapply(dfm_list_links, function(dfm) {
  # Schritt 1: docvars setzen
  docvars(dfm) <- docvars(Corpus_Links)
  
  # Schritt 2: Gruppieren nach Kapitel
  dfm <- dfm_group(dfm, groups = docvars(dfm, "Kapitel"))
  
  # Schritt 3: Berechnung der Frequenzen
  freq_table <- textstat_frequency(dfm)
  
  # Schritt 4: Umrechnung der Häufigkeiten in Prozent
  freq_table$rel <- log(freq_table$frequency / sum(freq_table$frequency)) 
  
  
  freq_table$percent <- (freq_table$frequency / sum(freq_table$frequency)) * 100
  
  # Schritt 5: Berechnung von Mittelwert und Standardabweichung der Häufigkeiten
  mean_freq <- mean(freq_table$frequency)
  sd_freq <- sd(freq_table$frequency)
  
  # Schritt 6: Berechnung der Z-Werte
  freq_table$z_score <- (freq_table$frequency - mean_freq) / sd_freq
  
  # Schritt 7: Signifikanzprüfung (z.B. bei einem 95%-Konfidenzniveau)
  freq_table$significant <- ifelse(abs(freq_table$z_score) > 1.96, TRUE, FALSE)
  
  return(freq_table)
})




token_list_rechts <- list()

for (names in names(Rechts_pos)) {
  # Konvertiere die 'lemma'-Spalte in Kleinschreibung innerhalb des jeweiligen Dataframes
  Rechts_pos[[names]]$token <- tolower(Rechts_pos[[names]]$lemma)
  
  # Erstelle die Tokens aus dem jeweiligen Dataframe in Links_pos[[names]]
  token_rechts <- as.tokens(Rechts_pos[[names]], lemma = TRUE) %>% 
    tokens_remove(pattern = stops) %>% 
    tokens_compound(pattern = as.phrase(compound_pattern_rechts))
  
  # Wandelt die Tokens in Kleinschreibung um
  token_rechts <- tokens_tolower(token_rechts)
  
  # Speichert die Tokens in der Liste 'token_list_rechts'
  token_list_rechts[[names]] <- token_rechts
}


dfm_list_rechts <-list(ALL = dfm(token_list_rechts$ALL),
                      NOUN = dfm(token_list_rechts$NOUN),
                      ADJ_ADV = dfm(token_list_rechts$ADJ_ADV),
                      ADJ = dfm(token_list_rechts$ADJ),
                      VERB = dfm(token_list_rechts$VERB))


list_rechts_gewichtet <- lapply(dfm_list_rechts, function(dfm) {
  # Schritt 1: docvars setzen
  docvars(dfm) <- docvars(Corpus_Rechts)
  
  # Schritt 2: Gruppieren nach Kapitel
  dfm <- dfm_group(dfm, groups = docvars(dfm, "Kapitel"))
  
  # Schritt 3: Berechnung der Frequenzen
  freq_table <- textstat_frequency(dfm)
  freq_table$rel <- log(freq_table$frequency / sum(freq_table$frequency)) 
  
  # Schritt 4: Umrechnung der Häufigkeiten in Prozent
  freq_table$percent <- (freq_table$frequency / sum(freq_table$frequency)) * 100
  
  # Schritt 5: Berechnung von Mittelwert und Standardabweichung der Häufigkeiten
  mean_freq <- mean(freq_table$frequency)
  sd_freq <- sd(freq_table$frequency)
  
  # Schritt 6: Berechnung der Z-Werte
  freq_table$z_score <- (freq_table$frequency - mean_freq) / sd_freq
  
  # Schritt 7: Signifikanzprüfung (z.B. bei einem 95%-Konfidenzniveau)
  freq_table$significant <- ifelse(abs(freq_table$z_score) > 1.96, TRUE, FALSE)
  
  return(freq_table)
})





list_rechts_gewichtet <- lapply(dfm_list_rechts, function(dfm) {
  # Schritt 1: docvars setzen
  docvars(dfm) <- docvars(Corpus_Rechts)

  # Schritt 2: Gruppieren nach Jahr
  dfm <- dfm_group(dfm, groups = docvars(dfm, "Jahr"))

  # Schritt 3: Gewichten des dfm mit dem gewünschten Schema
  dfm <- dfm_weight(dfm, scheme = "prop")

  # Schritt 4: Berechnung der Frequenzen
  freq_table <- textstat_frequency(dfm, groups = docvars(dfm, "Jahr"))

  # Schritt 5: Auswahl der 25 häufigsten Terme pro Jahr

  # Schritt 6: Umrechnung der Häufigkeiten in Prozent pro Gruppe (Jahr)
  freq_table_top <- freq_table %>%
    group_by(group) %>%
    mutate(percent = (frequency / sum(frequency)) * 100) %>%
    slice_max(frequency, n = 25)

  return(freq_table_top)
})

