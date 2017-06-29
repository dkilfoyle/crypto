library(ggplot2)
library(stringr)

ngrams = read.csv("ngrams.csv", header=T, stringsAsFactors = F)
ngrams$length = nchar(ngrams$gram)

plain_letter_frequencies = c(8.12,1.49,2.71,4.32,12.02,2.30,2.03,5.92,7.31,0.10,0.69,3.98,2.61,6.95,7.68,1.82,0.11,6.02,6.28,9.10,2.88,1.11,2.09,0.17,2.11,0.07)
plain_letters = LETTERS[order(plain_letter_frequencies, decreasing = T)]

# ggplot(data.frame(x=LETTERS, y=frequencies), aes(x=x,y=y)) + geom_col()

plain_words = list()
plain_words[[1]] = c("A", "I")
plain_words[[2]] = unlist(str_split(toupper("of, to, in, it, is, be, as, at, so, we, he, by, or, on, do, if, me, my, up, an, go, no, us, am"), ", "))
plain_words[[3]] = unlist(str_split(toupper("the, and, for, are, but, not, you, all, any, can, had, her, was, one, our, out, day, get, has, him, his, how, man, new, now, old, see, two, way, who, boy, did, its, let, put, say, she, too, use"), ", "))
plain_words[[4]] = unlist(str_split(toupper("that, with, have, this, will, your, from, they, know, want, been, good, much, some, time"), ", "))

get_letter_counts = function(cipher) {
  sapply(LETTERS, function(curLetter) {
    sum(str_count(cipher, curLetter))
  })
}

get_frequent_letters = function(cipher) {
  names(sort(get_letter_counts(cipher), decreasing=T))
}

get_frequent_words = function(cipher) {
  words = str_replace_all(cipher, "[[:punct:]]", "") %>%    # remove punctuation characters 
    paste(collapse="") %>%                                  # convert to string
    str_split(" ") %>%                                      # separate into words
    unlist()

  lapply(1:4, function(x) { 
    tbl = table(words[which(str_length(words)==x)])
    names(tbl[order(tbl, decreasing = T)])
  })
}

PUNC = c(" ",",",".","-","!","?")
LETTERS_PUNC = c(LETTERS, PUNC)
encrypt_key=c(LETTERS[sample.int(26)], PUNC)
decrypt_key=LETTERS_PUNC

as.letters = function(x) {
  unlist(strsplit(str_to_upper(x), ""))
}

encipher = function(x) {
  for (i in 1:length(x)) {
    x[i] = encrypt_key[match(x[i], LETTERS_PUNC)]
  }
  return(x)
}

cipher_replace = function(cipher, cipher_txt, plain_txt, undo=T, log=T) {
  if (undo) 
    undo_buffer <<- c(undo_buffer, list(c(cipher_txt, plain_txt)))
  x = gsub(cipher_txt, plain_txt, cipher)
  if (log) cat(paste(x,collapse=""))
  return(x)
}

undo_buffer <<- list()
undo = function(cipher) {
  undo_action = undo_buffer[[length(undo_buffer)]]
  cipher_replace(cipher, undo_action[2], undo_action[1])
}

plaintext = as.letters("If a man is offered a fact which goes against his instincts, he will scrutinize it closely, and unless the evidence is overwhelming, he will refuse to believe it. If, on the other hand, he is offered something which affords a reason for acting in accordance to his instincts, he will accept it even on the slightest evidence. The origin of myths is explained in this way.")
cipher= encipher(plaintext)

cipher_letters = get_frequent_letters(cipher)
cipher_words = get_frequent_words(cipher)

cipher_replace(cipher, cipher_words[[1]][1], plain_words[[1]][1]) # replace most frequent 1 letter word (A)
# cipher_replace(cipher, cipher_letter_frequencies[1], letter_frequences_sorted[1]) # replace most frequent letter (T)
