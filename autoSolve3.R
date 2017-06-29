# v3 - use austen digrams and guess starting key based on frequency analysis
# v2 = 4510ms

library(ggplot2)
library(stringr)
library(purrr)
library(profvis)

letters_space = c(letters, " ")

M = read.table("AustenCount.txt",header=F)
rownames(M) = letters_space
colnames(M) = letters_space
logM = log(M+1)

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
  # return(sapply(ciphertext, function (x) letters_space[match(x, decrypt_key)])) # actually slightly slower
}

guess_key = function(ciphertext) {
  
  # assign the most common single letter word to 'a'
  # assign the most common letter to a random choice of 't'
  # if there is a recurring 3 letter word assign it to 'the'
  # if there are doublets assign to random choice
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
      new_score = score_text(decipher(ciphertext, new_key))
      
      if (runif(1) > exp(new_score - best_trial_score)) {
        # reject this swap
      }
      else {
        # accept this swap - if the new score is better OR random chance (to avoid local maxima)
        key = new_key
        best_trial_score = new_score
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

score_text = function(guesstext) {
  sum(sapply(i:(length(guesstext)-1), function (ii) logM[guesstext[ii],guesstext[ii+1]]))
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
