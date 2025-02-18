
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

source("../Analysen/funktionen_glove.R")
source("../Visualisierung/GG_Layout.R")

stops <- readLines("../../Daten/german_stopwords_plain.txt")



load("../../Daten/links_parse.RDATA")
load("../../Daten/rechts_parse.RDATA")


load("../../Daten/compound_pattern_links.RDATA")
load("../../Daten/compound_pattern_rechts.RDATA")

filter_frequent_words <- function(tokens, pattern, freq_threshold) {
  test <- tokens_keep(tokens, pattern = pattern) %>% 
    dfm() %>% 
    textstat_frequency() %>% 
    filter(frequency >= freq_threshold)
  return(pattern[pattern %in% test$feature])
}



parse_tolower <- function(parsed_object){
  parsed_object%>% 
    mutate(lemma = str_to_lower(lemma)) %>% 
    mutate(token = str_to_lower(token))}

Links_Parse <- parse_tolower(links_parse_filter)
Rechts_Parse <- parse_tolower(rechts_parse_filter)

# Für 'links_parse_filter'
links_parse_filter <- Links_Parse %>% 
  filter(pos != "SPACE" & pos != "PUNCT" & pos != "NUM") %>% 
  filter(!(str_length(token) == 1 | 
             str_detect(token, "^[a-zA-Z]\\."))) %>% 
  filter(!(str_length(lemma) == 1 | 
             str_detect(lemma, "^[a-zA-Z]\\."))) %>% 
  mutate(
    # Entfernen von Sonderzeichen und '--'
    token = str_replace_all(token, "(?<=\\b)[\\x{0080}-\\x{009F}]+|[\\x{0080}-\\x{009F}]+(?=\\b)", ""),
    lemma = str_replace_all(lemma, "(?<=\\b)[\\x{0080}-\\x{009F}]+|[\\x{0080}-\\x{009F}]+(?=\\b)", ""),
    token = str_replace_all(token, "--", ""),  # Entfernt '--' aus 'token'
    lemma = str_replace_all(lemma, "--", "")   # Entfernt '--' aus 'lemma'
  ) %>%
  # Entferne Elemente mit nur 1 Zeichen NACH der gesamten Verarbeitung
  filter(!(str_length(token) == 1)) %>%
  filter(!(str_length(lemma) == 1))

# Für 'rechts_parse_filter'
rechts_parse_filter <- Rechts_Parse %>% 
  filter(pos != "SPACE" & pos != "PUNCT" & pos != "NUM") %>% 
  filter(!(str_length(token) == 1 | 
             str_detect(token, "^[a-zA-Z]\\."))) %>% 
  filter(!(str_length(lemma) == 1 | 
             str_detect(lemma, "^[a-zA-Z]\\."))) %>% 
  mutate(
    # Entfernen von Sonderzeichen und '--'
    token = str_replace_all(token, "(?<=\\b)[\\x{0080}-\\x{009F}]+|[\\x{0080}-\\x{009F}]+(?=\\b)", ""),
    lemma = str_replace_all(lemma, "(?<=\\b)[\\x{0080}-\\x{009F}]+|[\\x{0080}-\\x{009F}]+(?=\\b)", ""),
    token = str_replace_all(token, "--", ""),  # Entfernt '--' aus 'token'
    lemma = str_replace_all(lemma, "--", "")   # Entfernt '--' aus 'lemma'
  ) %>%
  # Entferne Elemente mit nur 1 Zeichen NACH der gesamten Verarbeitung
  filter(!(str_length(token) == 1)) %>%
  filter(!(str_length(lemma) == 1))



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



# Kontextfenster definieren (z. B. 5 Wörter)

dfm_trimmed_links <- dfm(token_list_links$ALL) %>% 
  dfm_trim(min_termfreq = 25)
dfm_trimmed_rechts <- dfm(token_list_rechts$ALL) %>% 
  dfm_trim(min_termfreq = 25)




###Was will ich betrachten? Schlüselbegriffe 
orga_links <- c("^szene*", "\\bpartei*", "^gruppe*", "^gruppierung*" ,"^organisation*",
                "pds", "partei des demokratischen sozialismus", "wasg", "wahlalternative arbeit und soziale gerechtigkeit", "pdl", "partei die linke", "dkp", "deutsche kommunistische partei", "mlpd", "marxistische-leninistische partei deutschland", "\\bil\\b", "interventionistische linke") 
orga_rechts <- c("^szene*", "\\bpartei*", "^gruppe*", "^gruppierung*" ,"^organisation*",
                 "npd", "nationaldemokratische partei deutschlands", "dvu", "deutsche volksunion", "afd", "alternative für deutschland",
                 "ibd", "identitäre bewegung deutschland", "die heimat", "d3w", "der dritte weg") 
orga_links <- grep(paste(orga_links, collapse = "|"), colnames(dfm_trimmed_links), value = TRUE)
orga_links <- orga_links[orga_links != "partei_alternative_für_deutschland"]
orga_rechts <- grep(paste(orga_rechts, collapse = "|"), colnames(dfm_trimmed_rechts), value = TRUE)





gewalt <- c("\\b^gewalt\\b", "gewalttat.*", "gewaltdelikt.*", "gewalttät.*", "gewaltbereit.*", "gewaltorientiert.*")
gewalt_links <- grep(paste(gewalt, collapse = "|"), colnames(dfm_trimmed_links), value = TRUE)
gewalt_rechts <- grep(paste(gewalt, collapse = "|"), colnames(dfm_trimmed_rechts), value = TRUE)



gewalt2 <- c(
  "\\b^gewalt\\b", 
  "gewalttat.*",
  "gewaltdelikt.*", 
  "gewalttät.*", 
  "gewaltbereit.*",
  "gewaltorientiert.*",         # genaues Wort "gewaltbereit"
  "tötungsdelikt.*",        # genaues Wort "tötungsdelikt"
  "körperverletzung*",     # genaues Wort "körperverletzung"
  "landfriedensbruch",     # genaues Wort "landfriedensbruch"
  "gewaltkriminalität",    # genaues Wort "gewaltkriminalität"
  "widerstandsdelikt*",    # genaues Wort "widerstandsdelikt"
  "\\bmord(?:e)?\\b",      # genaues Wort "mord" oder Plural "morde"
  "freiheitsberaubung*",   # genaues Wort "freiheitsberaubung"
  "sexualdelikt*"          # genaues Wort "sexualdelikt"
)
gewalt2_links <- grep(paste(gewalt2, collapse = "|"), colnames(dfm_trimmed_links), value = TRUE)
gewalt2_rechts <- grep(paste(gewalt2, collapse = "|"), colnames(dfm_trimmed_rechts), value = TRUE)

aktionen <- c("aktivität*", "aktion*", "demonstration*", "protest*", "veranstaltung*", "kundgebung*", "versammlung*")
aktionen_links <- grep(paste(aktionen, collapse = "|"), colnames(dfm_trimmed_links), value = TRUE)

aktionen_rechts <- grep(paste(aktionen, collapse = "|"), colnames(dfm_trimmed_rechts), value = TRUE)


ziel <- c("\\b^ziele*")
ziel <- grep(paste(ziel), colnames(dfm_trimmed_links), value = TRUE)



extrem_links <- c("^linksextremist", "^linksextremist*", "\\b^extremismus\\b", "\\b^extremist*")
extrem_rechts <- c("^rechtsextremist", "^rechtsextremist*", "\\b^extremismus\\b", "\\b^extremist*")


extrem_links <- grep(paste(extrem_links, collapse = "|"), colnames(dfm_trimmed_links), value = TRUE)
extrem_rechts <- grep(paste(extrem_rechts, collapse = "|"), colnames(dfm_trimmed_rechts), value = TRUE)


# Angenommene Liste mit Elementen
worte_links <- list(orga = orga_links,
                    gewalt = gewalt_links,
                    gewalt2=gewalt2_links,
                    aktionen = aktionen_links,
                    ziel =ziel,
                    extrem = extrem_links)
worte_rechts <- list(orga = orga_rechts,
                     gewalt = gewalt_rechts,
                     gewalt2=gewalt2_rechts,
                     aktionen = aktionen_rechts,
                     ziel =ziel,
                     extrem = extrem_rechts)


worte_rechts <- lapply(worte_rechts, function(pattern) {
  filter_frequent_words(
    tokens = token_list_rechts$ALL,
    pattern = pattern,
    freq_threshold = 100
  )
})

worte_rechts$extrem <- worte_rechts$extrem[!grepl("musik|verfassung|motiv|spektrum|szene|bestrebungen", worte_rechts$extrem)]

worte_links <- lapply(worte_links, function(pattern) {
  filter_frequent_words(
    tokens = token_list_links$ALL,
    pattern = pattern,
    freq_threshold = 100
  )
})

worte_links$extrem <- worte_links$extrem[!grepl("musik|verfassung|motiv|spektrum|szene", worte_links$extrem)]


# Ergebnisse speichern
ergebnisse <- list(rechts = list(), links = list())

# Schleife für die rechte Liste
for (name in names(worte_rechts)) {
  pattern <- worte_rechts[[name]]  # Aktuelles Muster aus der Liste abrufen
  
  # Für den rechten DataFrame
  # Tokens innerhalb des Fensters für das aktuelle Wort (rechts)
  toks_inside_rechts <- tokens_keep(token_list_rechts$ALL, pattern = pattern, window = 15)
  toks_inside_rechts <- tokens_remove(toks_inside_rechts, pattern = pattern) # Das aktuelle Wort selbst entfernen
  
  # Tokens außerhalb des Fensters für das aktuelle Wort (rechts)
  toks_outside_rechts <- tokens_remove(token_list_rechts$ALL, pattern = pattern, window = 15)
  
  # Dokument-Frequenz Matrizen erstellen (rechts)
  dfmat_inside_rechts <- dfm(toks_inside_rechts)
  dfmat_outside_rechts <- dfm(toks_outside_rechts)
  
  # Keyness-Statistik berechnen (rechts)
  tstat_key_inside_rechts <- textstat_keyness(rbind(dfmat_inside_rechts, dfmat_outside_rechts), 
                                              target = seq_len(ndoc(dfmat_inside_rechts)), measure = "lr")
  
  # Ergebnisse für den rechten DataFrame speichern
  ergebnisse$rechts[[name]] <- tstat_key_inside_rechts
}

# Schleife für die linke Liste
for (name in names(worte_links)) {
  pattern <- worte_links[[name]]  # Aktuelles Muster aus der Liste abrufen
  
  # Für den linken DataFrame
  # Tokens innerhalb des Fensters für das aktuelle Wort (links)
  toks_inside_links <- tokens_keep(token_list_links$ALL, pattern = pattern, window = 15)
  toks_inside_links <- tokens_remove(toks_inside_links, pattern = pattern) # Das aktuelle Wort selbst entfernen
  
  # Tokens außerhalb des Fensters für das aktuelle Wort (links)
  toks_outside_links <- tokens_remove(token_list_links$ALL, pattern = pattern, window = 15)
  
  # Dokument-Frequenz Matrizen erstellen (links)
  dfmat_inside_links <- dfm(toks_inside_links)
  dfmat_outside_links <- dfm(toks_outside_links)
  
  # Keyness-Statistik berechnen (links)
  tstat_key_inside_links <- textstat_keyness(rbind(dfmat_inside_links, dfmat_outside_links), 
                                             target = seq_len(ndoc(dfmat_inside_links)), measure = "lr")
  
  # Ergebnisse für den linken DataFrame speichern
  ergebnisse$links[[name]] <- tstat_key_inside_links
}

# Getrimmte Ergebnisse anzeigen (z. B. die Top 200 Einträge für jede Kategorie)
list_df_trimmed <- list(
  rechts = lapply(ergebnisse$rechts, function(df) head(df, 200)),
  links = lapply(ergebnisse$links, function(df) head(df, 200))
)


# Aktiv
aktiv <- as.data.frame(
  cbind(list_df_trimmed$rechts$aktionen$feature, 
        list_df_trimmed$rechts$aktionen$G2,  # G2 direkt nach feature
        list_df_trimmed$links$aktionen$feature,
        list_df_trimmed$links$aktionen$G2)   # G2 direkt nach feature
)
colnames(aktiv) <- c("Rechte_Kooks_aktiv", "Rechte_Kooks_aktiv_G2", 
                     "Linke_Kooks_aktiv", "Linke_Kooks_aktiv_G2")

# Orga
orga <- as.data.frame(
  cbind(list_df_trimmed$rechts$orga$feature, 
        list_df_trimmed$rechts$orga$G2,  # G2 direkt nach feature
        list_df_trimmed$links$orga$feature,
        list_df_trimmed$links$orga$G2)   # G2 direkt nach feature
)
colnames(orga) <- c("Rechte_Kooks_Orga", "Rechte_Kooks_Orga_G2", 
                    "Linke_Kooks_Orga", "Linke_Kooks_Orga_G2")

# Gewalt
gew <- as.data.frame(
  cbind(list_df_trimmed$rechts$gewalt$feature, 
        list_df_trimmed$rechts$gewalt$G2,  # G2 direkt nach feature
        list_df_trimmed$links$gewalt$feature,
        list_df_trimmed$links$gewalt$G2)   # G2 direkt nach feature
)
colnames(gew) <- c("Rechte_Kooks_gew", "Rechte_Kooks_gew_G2", 
                   "Linke_Kooks_gew", "Linke_Kooks_gew_G2")

# Gewalt2
gew2 <- as.data.frame(
  cbind(list_df_trimmed$rechts$gewalt2$feature, 
        list_df_trimmed$rechts$gewalt2$G2,  # G2 direkt nach feature
        list_df_trimmed$links$gewalt2$feature,
        list_df_trimmed$links$gewalt2$G2)   # G2 direkt nach feature
)
colnames(gew2) <- c("Rechte_Kooks_gew2", "Rechte_Kooks_gew2_G2", 
                    "Linke_Kooks_gew2", "Linke_Kooks_gew2_G2")

# Ziel
ziel <- as.data.frame(
  cbind(list_df_trimmed$rechts$ziel$feature, 
        list_df_trimmed$rechts$ziel$G2,  # G2 direkt nach feature
        list_df_trimmed$links$ziel$feature,
        list_df_trimmed$links$ziel$G2)   # G2 direkt nach feature
)

colnames(ziel) <- c("Rechte_Kooks_ziele", "Rechte_Kooks_ziele_G2", 
                    "Linke_Kooks_ziele", "Linke_Kooks_ziele_G2")

extrem <- as.data.frame(
  cbind(list_df_trimmed$rechts$extrem$feature, 
        list_df_trimmed$rechts$extrem$G2,  # G2 direkt nach feature
        list_df_trimmed$links$extrem$feature,
        list_df_trimmed$links$extrem$G2)   # G2 direkt nach feature
)
colnames(extrem) <- c("Rechte_Kooks_extrem", "Rechte_Kooks_extrem_G2", 
                    "Linke_Kooks_extrem", "Linke_Kooks_extrem_G2")