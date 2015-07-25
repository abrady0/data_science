
setwd('~/ocw/data_science/dsscapstone');
library(data.table)
twoGramFreqDT <- fread('twoGramFreqDT.RData')[order(-freq)]