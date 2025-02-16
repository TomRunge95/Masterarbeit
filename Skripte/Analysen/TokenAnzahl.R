
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



spacy_initialize(model = "de_core_news_lg")



load("../../Daten/Links_plus.RDATA")
load("../../Daten/Rechts_plus.RDATA")

load("../../Daten/links_parse.RDATA")
load("../../Daten/rechts_parse.RDATA")



links_parse_filter$token <- tolower(links_parse_filter$token) 
links_parse_filter <- links_parse_filter %>%  filter(!grepl("2000", doc_id)) 

rechts_parse_filter$token <- tolower(rechts_parse_filter$token)
rechts_parse_filter <- rechts_parse_filter %>%  filter(!grepl("2000", doc_id)) 


stops <- readLines("../../Daten/german_stopwords_plain.txt")

links_parse_filter <- links_parse_filter %>% 
  filter(pos != "SPACE" & pos != "PUNCT" & pos != "NUM") %>% 
  filter(!(str_length(token) == 1 | 
             str_detect(token, "^[a-zA-Z]\\."))) %>%
  filter(!token %in% stops) %>%
  # mutate(token = tolower(token)) %>%  # zu Kleinbuchstaben umwandeln
  mutate(token = str_replace_all(token, "(?<=\\b)[\\x{0080}-\\x{009F}]+|[\\x{0080}-\\x{009F}]+(?=\\b)", ""))



rechts_parse_filter <- rechts_parse_filter %>% 
  filter(pos != "SPACE" & pos != "PUNCT" & pos != "NUM") %>% 
  filter(!(str_length(token) == 1 | 
             str_detect(token, "^[a-zA-Z]\\."))) %>%
  filter(!token %in% stops) %>%
  # mutate(token = tolower(token)) %>%  # zu Kleinbuchstaben umwandeln
  mutate(token = str_replace_all(token, "(?<=\\b)[\\x{0080}-\\x{009F}]+|[\\x{0080}-\\x{009F}]+(?=\\b)", ""))



# 
# compound_token_links <- links_parse_filter %>% 
#   as.tokens()
# 
# tstat_col_caps <- tokens_select(compound_token_links, pattern = "^[A-Z0-9-]", 
#                                 valuetype = "regex", 
#                                 case_insensitive = T, 
#                                 padding = TRUE) %>% 
#   textstat_collocations(size = 2, min_count = 100)
# 
# tstat_col_caps_links <-  tstat_col_caps %>% 
#   filter(lambda >=5) %>% 
#   filter(count >=200)
# 
# 
# compound_pattern_links <- phrase(tstat_col_caps_links$collocation)  
# 



# Häufigkeit der Tokens zählen und nach doc_id gruppieren
result_links <- links_parse_filter %>%
  group_by(doc_id, token) %>%   # Gruppiere nach doc_id und token
  summarize(token_count = n()) %>%  # Zähle die Anzahl der Tokens
  ungroup() %>%                  # Ungroup für nachfolgende Operationen
  group_by(doc_id) %>%
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

sum(result_rechts$reihen_summe)


sum(result_links$reihen_summe) + sum(result_rechts$reihen_summe)

Links_plus <- Links_plus %>% 
  filter(Jahr!="2000")

Rechts_plus <- Rechts_plus %>% 
  filter(Jahr!="2000")



token_df <- data.frame(doc_id = Links_plus$doc_id,
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
# 
# save(Links_plus, file ="Links_plus.RDATA")
# save(Rechts_plus, file= "Rechts_plus.RDATA")
# 
# save(token_df, file= "token_df.RDATA")


sum(mean(token_df$token_Links))
sum(mean(token_df$token_Rechts))
        


# Kombiniere die Plots

# Füge eine gemeinsame Legende hinzu

# ggplots anzeigen


mittelwerte_nach_jahr <- token_df %>%
  group_by(jahr) %>%
  summarize(mean_token_Links = mean(token_Links, na.rm = TRUE),
            mean_token_Rechts = mean(token_Rechts, na.rm = TRUE))

mittelwerte_nach_bund <- token_df %>%
  group_by(bundesland) %>%
  summarize(mean_token_Links = mean(token_Links, na.rm = TRUE),
            mean_token_Rechts = mean(token_Rechts, na.rm = TRUE))

mittelwerte_nach_bund <- token_df %>%
  group_by(bundesland, jahr) %>%
  summarize(mean_token_Links = mean(token_Links, na.rm = TRUE),
            mean_token_Rechts = mean(token_Rechts, na.rm = TRUE),
            mean_verhältnis_token = mean(token_Rechts / token_Links, na.rm = TRUE))


            median_nach_jahr <- token_df %>%
              group_by(jahr) %>%
              summarize(median_token_Links = median(token_Links, na.rm = TRUE),
                        median_token_Rechts = median(token_Rechts, na.rm = TRUE))            
            
            median_nach_bund <- token_df %>%
              group_by(bundesland) %>%
              summarize(median_token_Links = median(token_Links, na.rm = TRUE),
                        median_token_Rechts = median(token_Rechts, na.rm = TRUE))
            
            
df_ohne_bund <- filter(token_df, !bundesland == "Bund")



mittelwerte_nach_jahr <- mittelwerte_nach_jahr %>% 
  mutate(mean_gesamt = mean_token_Links + mean_token_Rechts)




