setwd('~/ocw/data_science/dsscapstone');
library(data.table)
library(ggplot2)
source('util.R')

oneGram <- fread('oneGramFreqDT.RData')
twoGram <- fread('twoGramFreqDT.RData')
oneGram <- oneGram[order(-freq)]
twoGram <- twoGram[order(-freq)]

q0 <- tokenizeLines('The guy in front of me just bought a pound of bacon, a bouquet, and a case of', 'en')
twoGram[tok0==q0[length(q0)]]
predict <- function(sentence) {
  twoGram[s]
}