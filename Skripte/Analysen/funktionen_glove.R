get_similar_words <- function(reference_word, word_embeddings) {
  
  # Find closest aligned word embeddings based on cosine similarity
  tryCatch({
    word <- word_embeddings[reference_word, , drop = FALSE]
  },
  error = function(e) {
    stop("The supplied word (", reference_word, ") is not part of the created vocabulary.")
  })
  
  cos_sim <- text2vec::sim2(
    x = word_embeddings, 
    y = word, 
    method = "cosine", 
    norm = "l2"
  )
  
  # Speichern der ähnlichen Wörter in einem Objekt und Rückgabe
  similar_words <- head(sort(cos_sim[,1], decreasing = TRUE), 50)
  return(similar_words)
}


remove_urls <- function(text) {
  str_replace_all(text, "http[s]?://\\S+|www\\.\\S+", "")
}


check_spelling <- function(tokens) {
  # Konvertiere Tokens in einen Vektor
  token_vector <- unlist(tokens)
  
  # Finde falsche Wörter
  incorrect_words <- token_vector[!hunspell_check(token_vector, dict = "de_DE")]
  
  # Rückgabe der falschen Wörter
  return(unique(incorrect_words))
}



findCloseWords = function(w,d,n) {
  words = rownames(d)
  i = which(words==w)
  if (length(i) > 0) {
    res = sort(d[i,])
    print(as.matrix(res[2:(n+1)]))
  } 
  else {
    print("Word not in corpus.")
  }
}

