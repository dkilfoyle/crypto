library(ggplot2)
library(stringr)
library(purrr)
library(profvis)

letters_space = c(letters, " ")

encipher = function(plaintext, encrypt_key) {
  x = unlist(str_split(plaintext,""))
  for (i in 1:length(x)) {
    x[i] = encrypt_key[match(x[i], letters_space)]
  }
  return(x)
}

decipher = function(ciphertext, decrypt_key) {
  guesstext = ciphertext
  for (i in 1:length(ciphertext)) {
    guesstext[i] = letters_space[match(guesstext[i], decrypt_key)]
  }
  return(guesstext)
}

find_key = function(ciphertext, trials=10, swaps=3000) {
  key = letters_space
  best_score = 0
  for (i in 1:trials) {
    cat("Trial ",i," ")
    key = c(key[sample.int(26)], " ")
    
    best_trial_score = 0
    
    for (j in 1:swaps) {
      
      if (j %% 100 == 0) cat(".")
      
      # swap two randomly chosen letters in the key
      swap = sample.int(26,2)
      new_key = key
      new_key[swap[1]] = key[swap[2]]
      new_key[swap[2]] = key[swap[1]]
      
      # score the new key based on frequency of produced ngrams
      new_score = score_key(ciphertext, new_key)
      if (new_score > best_trial_score) {
        key = new_key
        best_trial_score = new_score
      }
      else if (new_score == best_trial_score) {
        # randomly choose whether to swap
        if (sample.int(2,1)==2)
          key=new_key
      }
    }
    cat("\n")
    
    if (best_trial_score > best_score) {
      best_key = key
      best_score = best_trial_score
      cat("Best score so far: ", best_trial_score,"\n")
      cat(paste(decipher(ciphertext, best_key),collapse=""),"\n")
    }
  }
  return (best_key)
}

score_key = function(ciphertext, key) {
  guesstext = paste(decipher(ciphertext, key),collapse="")
  
  score = 0
  for (i in 1:nrow(ngrams)) {
    score = score + str_count(guesstext, ngrams$gram[i]) * ngrams$score[i]
  }
  
  return(score)
}

ngrams = read.csv("ngrams.csv", header=T, stringsAsFactors = F)
ngrams$length = nchar(ngrams$gram)

plaintext = "If a man is offered a fact which goes against his instincts, he will scrutinize it closely, and unless the evidence is overwhelming, he will refuse to believe it. If, on the other hand, he is offered something which affords a reason for acting in accordance to his instincts, he will accept it even on the slightest evidence. The origin of myths is explained in this way."

# remove punctuations, spaces and convert to lower case
plaintext = str_replace_all(plaintext, "[[:punct:]]", "")
plaintext = str_to_lower(str_replace_all(plaintext, " ", " "))

encrypt_key = c(letters[sample.int(26)], " ")
ciphertext = encipher(plaintext, encrypt_key)

# profvis(find_key(ciphertext, trials=1))
