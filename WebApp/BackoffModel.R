library(quanteda)
library(stringr)

unigram <- readRDS(file="data/unigram.rds")
bigram <- readRDS(file="data/bigram.rds")
trigram <- readRDS(file="data/trigram.rds")

text_cache <- "initialize_cache"

GetPrediction <- function(txt, gram) {
  pred <- data.frame(ngrams=vector(mode = 'character', length = 0),
                     freq=vector(mode = 'integer', length = 0))
  regex <- sprintf("%s%s%s", "^", txt, "_")
  gram_indices <- grep(regex, gram$ngram)
  if(length(gram_indices) > 0) {
    pred <- gram[gram_indices, ]
  }
  
  return(pred)
}

preprocess <- function(input_text){
  output <- gsub("[[:punct:]]+", "", input_text)
  output <- gsub('[0-9]+', '', output)
  output <- tolower(output)
  
  last_char <- str_sub(output,-1)
  words <- strsplit(output, " ")[[1]]
  
  if(length(words)>1){
    
    if (last_char == " "){
      output_1 <- words[length(words)]
      output_2 <- paste(words[length(words)-1], words[length(words)], sep="_")
      filter <- ""
    } else {
      output_1 <- words[length(words)-1]
      output_2 <- paste(words[length(words)-2], words[length(words)-1], sep="_")
      filter <- words[length(words)]
    }
  } else if(length(words) == 0){
    output_1 <- "xplqnxush"
    output_2 <- "ksnsuwbaasdw_xsdplqnxush"
    filter <- ""
    
  } else if(length(words) == 1){
    output_1 <- "xplqnxush"
    output_2 <- paste("ksnsuwbaasdw", "xplqnxush", sep = "_")
    if(last_char == " "){
      output_1 <- words
      output_2 <- paste("ksnsuwbaasdw", words, sep = "_")
      filter <- ""
    } else{
      filter <- words
    }
  }
  return(list(output_1, output_2, filter))
}

GetList <- function(preprocessed_text){
  
  text_cache <<- preprocessed_text[[2]]
  trigram_input <- preprocessed_text[[2]]
  bigram_input <- preprocessed_text[[1]]
  filter <- preprocessed_text[[3]]
  nblank <- 0
  
  pattern <- paste("^",filter, sep="")
  
  list <- GetPrediction(trigram_input, trigram)
  
  Top4Predictions <- function(possible_prediction){
    index_filtered <- grep(pattern, possible_prediction)
    prediction_filtered <- possible_prediction[index_filtered]
    predictions <- prediction_filtered[1:4]
    predictions[is.na(predictions)] <- ""
    return(predictions)
  }
  
  if (length(list$ngram) > 0){
    state <<- 1
    possible_prediction <- word(list$ngram, 3, sep="_")
    predictions <- Top4Predictions(possible_prediction)
    nblank <- sum(predictions == (c("", "", "", "")))
    possible_prediction_cache <<- possible_prediction
            if(nblank==0){
              return(predictions)
            }
  } 
  
  if(length(list$ngram) == 0 | nblank > 0){
        
    state <<- 2
    list <- GetPrediction(bigram_input, bigram)
    possible_prediction <- word(list$ngram, 2, sep="_")
    possible_prediction_cache <<- possible_prediction
          if(nblank > 0){
            fromprevious <- predictions[predictions!=""]
            fillblankwords <- Top4Predictions(possible_prediction)[1:4]
            predictions <- c(fromprevious, fillblankwords)
            predictions <- unique(predictions)[1:4]
            predictions[is.na(predictions)] <- ""
            nblank <- sum(predictions == (c("", "", "", "")))
          }  else{
              predictions <- Top4Predictions(possible_prediction)
              nblank <- sum(predictions == (c("", "", "", "")))
          }

    if(nblank==0){
      return(predictions)
    }
  }
  
  if(nblank > 0){
    state <<- 3
    possible_prediction <- unigram$ngram
    possible_prediction_cache <<- possible_prediction
    
    fromprevious <- predictions[predictions!=""]
    fillblankwords <- Top4Predictions(possible_prediction)[1:4]
    predictions <- c(fromprevious, fillblankwords)
    predictions <- unique(predictions)[1:4]
    predictions[is.na(predictions)] <- ""
  }
  return(predictions)
}


GetListCache <- function(possible_prediction_cache. = possible_prediction_cache, state. = state, filter. = txt[[3]]){
  pattern <- paste("^",filter., sep="")
  
  Top4Predictions <- function(wordlist){
    index_filtered <- grep(pattern, wordlist)
    prediction_filtered <- wordlist[index_filtered]
    top4 <- prediction_filtered[1:4]
    top4[is.na(top4)] <- ""
    return(top4)
  }
  
  if(state. == 1){
    predictions <- Top4Predictions(possible_prediction_cache.)
    nblank <- sum(predictions == (c("", "", "", "")))
            if(nblank==0){
              return(predictions)
            }
    
    if(nblank > 0){
      state <<- 2
      list <- GetPrediction(txt[[1]], bigram)
      possible_prediction <- word(list$ngram, 2, sep="_")
      possible_prediction_cache <<- possible_prediction
      
      fromprevious <- predictions[predictions!=""]
      fillblankwords <- Top4Predictions(possible_prediction)[1:4]
      predictions <- c(fromprevious, fillblankwords)
      predictions <- unique(predictions)[1:4]
      predictions[is.na(predictions)] <- ""
      nblank <- sum(predictions == (c("", "", "", "")))
            if(nblank==0){
              return(predictions)
            }
      
      if (nblank > 0){
        state <<- 3
        possible_prediction <- unigram$ngram
        possible_prediction_cache <<- possible_prediction
        
        fromprevious <- predictions[predictions!=""]
        fillblankwords <- Top4Predictions(possible_prediction)[1:4]
        predictions <- c(fromprevious, fillblankwords)
        predictions <- unique(predictions)[1:4]
        predictions[is.na(predictions)] <- ""
        
        return(predictions)
        
      }
    }
  }
  if(state.==2){
    predictions <- Top4Predictions(possible_prediction_cache.)
    nblank <- sum(predictions == (c("", "", "", "")))
          if(nblank==0){
            return(predictions)
          }

    if(nblank>0){
      state <<- 3
      possible_prediction <- unigram$ngram
      possible_prediction_cache <<- possible_prediction
      
      fromprevious <- predictions[predictions!=""]
      fillblankwords <- Top4Predictions(possible_prediction)[1:4]
      predictions <- c(fromprevious, fillblankwords)
      predictions <- unique(predictions)[1:4]
      predictions[is.na(predictions)] <- ""
      
      return(predictions)
    }
  }
  
  if(state.==3){
    predictions <<- Top4Predictions(possible_prediction_cache.)
  }
  return(predictions)
}

WordsSuggest <- function(text_cache. = text_cache, preprocessed_text = txt){
  if(preprocessed_text[[2]] != text_cache.){
    suggestions <- GetList(preprocessed_text)
  } else{
    suggestions <- GetListCache()
  }
  return(suggestions)
}

next_suggestion<- function(input_text){
  txt <<- preprocess(input_text)
  words_suggestion <- WordsSuggest()
  return(words_suggestion)
}

show_filter <- function(txt. = txt){
  return(txt.[[3]])
}

