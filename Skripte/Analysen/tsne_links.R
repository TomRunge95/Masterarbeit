library(quanteda)
library(quanteda.sentiment)
library(quanteda.textstats)
library(word2vec)
library(dplyr)
library(stringr)
library(textstem)
library(data.table)
library(text2vec)
library(Rtsne)
library(RColorBrewer)
library(tibble)

source("funktionen_glove.R")
source("Dezim_Layout.R")

modell_links <- readRDS("../../Daten/modell_links.RDS")



filter_frequent_words <- function(tokens, pattern, freq_threshold) {
  test <- tokens_keep(tokens, pattern = pattern) %>% 
    dfm() %>% 
    textstat_frequency() %>% 
    filter(frequency >= freq_threshold)
  return(pattern[pattern %in% test$feature])
}

find_similar_words <- function(wort_filter, modell, exclusion_list, threshold) {
  test <- list()
  
  # Überprüfen, ob die Wörter im Modell enthalten sind
  wort_filter <- wort_filter[wort_filter %in% rownames(modell)]
  
  if(length(wort_filter) == 0) {
    message("Keine Wörter aus wort_filter im Modell gefunden.")
  }
  
  for (i in wort_filter) {
    b <- get_similar_words(i, modell)
    test[[i]] <- as.data.frame(b) %>% 
      rownames_to_column()
  }
  
  final_df <- bind_rows(test) %>%
    distinct(rowname, .keep_all = TRUE) %>%
    filter(b > threshold,
           !rowname %in% exclusion_list)
  return(final_df)
}




tsne_and_plot <- function(word_vectors, title, perplexity, k_centers, max_iter = 5000, category_name, similar_words_df) {
  # Hilfsfunktion, um t-SNE mit Fallback zu berechnen
  calculate_tsne <- function(word_vectors, perplexity, max_iter) {
    tryCatch({
      Rtsne(word_vectors, perplexity = perplexity, pca = T, verbose = TRUE, max_iter = max_iter)
    }, error = function(e) {
      message("Fehler bei Perplexity ", perplexity, ": ", e$message)
      if (perplexity != 30) {
        message("Versuche mit Perplexity = 30 erneut")
        Rtsne(word_vectors, perplexity = 25, pca = T, verbose = TRUE, max_iter = max_iter)
      } else {
        stop("Berechnung fehlgeschlagen, auch mit Perplexity = 30.")
      }
    })
  }
  
  # t-SNE-Modell berechnen (mit Fallback)
  tsne_result <- calculate_tsne(word_vectors, perplexity, max_iter)
  
  # t-SNE-Daten in DataFrame umwandeln
  tsne_data <- tsne_result$Y %>%
    as.data.frame() %>%
    mutate(word = rownames(word_vectors))
  
  # Similarity-Werte aus similar_words_df hinzufügen
  tsne_data <- tsne_data %>%
    left_join(similar_words_df %>% select(rowname, b), by = c("word" = "rowname"))
  
  # Fehlende Similarity-Werte mit NA füllen
  tsne_data$b[is.na(tsne_data$b)] <- 0
  
  # Cluster mit k-means erstellen
  clusters <- kmeans(tsne_result$Y, centers = k_centers, nstart = 100, iter.max = 300)$cluster
  
  # t-SNE-Ergebnisse mit Cluster-Labels anreichern
  tsne_data <- tsne_data %>%
    mutate(cluster = as.factor(clusters))
  
  tsne_data_filtered <- tsne_data %>%
    group_by(cluster) %>%
    slice_max(order_by = b, n = 50) 

  
  
  # t-SNE-Plot erstellen
  tsne_plot <- tsne_data %>%
    ggplot(aes(x = V1, y = V2, label = word, color = cluster)) + 
    geom_text(data = tsne_data_filtered, aes(alpha = b), size = 3) + 
    scale_alpha_continuous(range = c(0.33, 1)) + # Transparenzbereich
    scale_color_brewer(palette = "Dark2") +  # Farbpalette "Spectral"
    labs(title = title, color = "Cluster", alpha = "Relevance") +
    theme(
      legend.position = "none",          # Entferne die Legende
      axis.title.x = element_blank(),    # Entferne Achsentitel (x)
      axis.title.y = element_blank(),    # Entferne Achsentitel (y)
      axis.text.x = element_blank(),     # Entferne Achsenticks (x)
      axis.text.y = element_blank(),     # Entferne Achsenticks (y)
      axis.ticks = element_blank(),      # Entferne Achsenticks
      panel.grid = element_blank(),
      panel.grid.major.y =  element_blank(), # Entferne alle Gitterlinien
      plot.title = element_text(hjust = 0.5) # Zentriere den Titel
    )
  
  # Plot speichern
  ggsave_pref(
    filename = paste0(category_name, "_links", ".png"), 
    plot = tsne_plot, 
    bg = "white",
    height = 14
  )
  
  return(list(plot = tsne_plot, data = tsne_data, clusters = clusters))
}

categories <- list(
  gewalt = list(pattern = worte_links$gewalt, threshold = 0.3, k = 3),
  gewalt2 = list(pattern = worte_links$gewalt2, threshold = 0.3, k = 3),
  orga = list(pattern = worte_links$orga, threshold = 0.3, k = 3),
  aktionen = list(pattern = worte_links$aktionen, threshold = 0.3, k = 3),
  extrem = list(pattern = worte_links$extrem, threshold = 0.3, k = 3),
  ziel = list(pattern = worte_links$ziel, threshold = 0.3, k = 3)
)
results_tsne_links <- list()

for (category_name in names(categories)) {
  # Schritt 1: Häufige Wörter filtern
  filtered_words <- filter_frequent_words(
    tokens = token_list_links$ALL,
    pattern = categories[[category_name]]$pattern,
    freq_threshold = 100
  )
  
  # Schritt 2: Ähnliche Wörter finden
  similar_words_df <- find_similar_words(
    wort_filter = filtered_words,
    modell = modell_links,
    exclusion_list = categories[[category_name]]$pattern,
    threshold = categories[[category_name]]$threshold)
  
  # Schritt 3: Wortvektoren abrufen
  word_vectors <- modell_links[similar_words_df$rowname, , drop = FALSE]
  
  tsne_result <- tsne_and_plot(
    word_vectors = word_vectors,
    title = "",
    perplexity = 50,
    k_centers = categories[[category_name]]$k,
    category_name = category_name,
    similar_words_df = similar_words_df
  )
  
  
  # Ergebnisse speichern
  results_tsne_links[[category_name]] <- tsne_result
}

# Alle Plots anzeigen
for (category_name in names(results_tsne_links)) {
  print(results_tsne_links[[category_name]]$plot)
}
