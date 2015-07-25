setwd('~/ocw/data_science/dsscapstone');
library(data.table)
library(ggplot2)

library('tm')
tokenizeLines <- function(lines, lang) {
  # TODO end of line token
  lines <- removePunctuation(tolower(lines));
  #lines <- removeWords(lines, stopwords(kind = lang));
  toks <- MC_tokenizer(lines)
  
  filterTok <- function(tok) {
    tok[nchar(tok)>0]
  }
  filterTok(toks);
}