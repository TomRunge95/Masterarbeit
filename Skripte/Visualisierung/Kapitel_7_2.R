
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


source("../Analysen/Relative_Häufigkeiten.R")




dfm <- rbind(dfm_list_links$ALL, dfm_list_rechts$ALL) 



docvars(dfm, "group") <- ifelse(grepl("links", docnames(dfm)), "links", "rechts")
docvars(dfm, "Jahr") <- as.numeric(gsub("\\D", "", docnames(dfm)))



dfm_grouped <- dfm_group(dfm, 
                        groups = paste(docvars(dfm, "group")))


aaa <- textstat_frequency(dfm_grouped, 
                          groups = paste(docvars(dfm_grouped, "group")))

a <- textstat_frequency(dfm, groups = docvars(dfm, "group"))

a_wide <- a %>% 
  select(feature, group, frequency) %>% 
  pivot_wider(names_from = group, values_from = frequency, values_fill = 0)



N_A <- sum(a_wide$links)
N_B <- sum(a_wide$rechts)


links <- aaa %>%
  filter(grepl("links", group)) %>% 
  mutate(rel_links = frequency / N_A * 1000)

rechts <- aaa %>%
  filter(grepl("rechts", group)) %>% 
  mutate(rel_rechts = frequency / N_B * 1000)


dfm_grouped <- dfm_group(dfm, groups = docvars(dfm, "group"))





log_likelihood_test <- function(O_A, O_B, N_A, N_B) {
  E_A <- (O_A + O_B) * (N_A / (N_A + N_B))  # Erwartungswert für Korpus A
  E_B <- (O_A + O_B) * (N_B / (N_A + N_B))  # Erwartungswert für Korpus B
  
  LL <- 2 * ((O_A * log(O_A / E_A)) + (O_B * log(O_B / E_B)))
  LL[is.na(LL)] <- 0  # Falls 0-Häufigkeiten auftreten, Fehler vermeiden
  
  return(LL)
}


word_data <- a_wide %>%
  mutate(LL = log_likelihood_test(links, rechts, N_A, N_B))

# Setze einen Signifikanzschwellenwert (z. B. 3.84 für p < 0.05)
word_data <- word_data %>%
  mutate(Signifikant = ifelse(LL > 3.84, "Ja", "Nein"))




word_data <- word_data %>%
  mutate(
    Rel_A = links / N_A * 1000,  # Normierung auf 1 Mio Wörter
    Rel_B = rechts / N_B * 1000
  )


Final_Tab <- subset(word_data, select = -c(LL, Signifikant)) %>% 
  slice(1:25) %>% 
  mutate(Rel_A = round(Rel_A,1),
         Rel_B = round(Rel_B,1))

stargazer::stargazer(Final_Tab, type = "latex", out = "../Visualisierung/top25.tex", summary = FALSE, digits = 2)

