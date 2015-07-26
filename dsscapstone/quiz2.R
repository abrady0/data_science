setwd('~/ocw/data_science/dsscapstone');
library(data.table)
library(ggplot2)
source('util.R')

oneGram <- fread('oneGramFreqDT.RData')
twoGram <- fread('twoGramFreqDT.RData')
oneGram <- oneGram[order(-freq)]
twoGram <- twoGram[order(-freq)]

predictMatches <- function(sentence, potentialAnswers) {
  toks <- tokenizeLines(sentence, 'en');
  lastTok <- toks[[length(toks)]]
  DT <- twoGram[tok0==lastTok & tok1 %in% potentialAnswers];    
  DT[order(-freq)]
}

# q1
s <- 'The guy in front of me just bought a pound of bacon, a bouquet, and a case'
ans <- c('beer','cheese','soda','pretzels')
predictMatches(s,ans)

# q2
s <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
ans <- c('best','most','world','universe');
predictMatches(s,ans)

# q3
s <- "Hey sunshine, can you follow me and make me the"
ans <- c('happiest','saddest','bluest','smelliest')
predictMatches(s,ans)
# no matches???
