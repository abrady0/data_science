# x open connection
# x read block of lines
# x tokenize
# factorize tokens?
# accumulate frequencies
# normalize
# save

setwd('~/ocw/data_science/dsscapstone');
set.seed(1234);
library(data.table)
library(ggplot2)
#library(parallel)
#library("foreach")
#install.packages('doParallel')
#library("doParallel")
library(data.table)
# vignette("parallel")
source('util.R')

#cl <- makeCluster(detectCores() - 1)
#registerDoParallel(cl, cores = detectCores() - 1)

toksToFreqs <- function(toks, oneGramFreq, twoGramFreq) {
  updateCount <- function (env, tok) {
    total <- 0;
    if (!is.null(env[[tok]])) {
      total <- env[[tok]];
    }
    env[[tok]] <- total + 1;
  }
  for (i in 1:length(toks)) {
    tok <- toks[[i]];
    updateCount(oneGramFreq, tok);
    if (i < length(toks)) {
      env <- twoGramFreq[[tok]];
      if (is.null(env)) {
        env <- twoGramFreq[[tok]] <- new.env();
      }
      updateCount(env, toks[[i+1]]);
    }
  }
  c(oneGramFreq, twoGramFreq);
}

filterLowFrequencyWords <- function(e) {
  toRemove <- c();
  lapply(ls(e), function(tok) {
    elt <- e[[tok]];
    if (is.environment(elt)) {
      filterLowFrequencyWords(elt);
      if (length(ls(elt)) < 5) {
        toRemove <<- c(toRemove, tok);
      }
    } else {
      if (!is.numeric(elt) || elt < 5) {
        toRemove <<- c(toRemove, tok);
      }
    }
  })
  print(paste('removing "',length(toRemove),'words'));
  rm(list=toRemove,envir=e);
}

processFile <- function(fileName, numLinesInFile, oneGramFreq, twoGramFreq) {
  print(paste('processFile',fileName));
  numLinesToRead <- 20000;
  numProcessed <- 0
  con <- file(fileName,open="r");
  repeat {
    lines <- readLines(con, numLinesToRead);
    #lines <- paste(lines,'EOL'); more trouble than worth
    toks <- tokenizeLines(lines, 'en');
    toksToFreqs(toks, oneGramFreq, twoGramFreq);
    
    numProcessed <- numProcessed + length(lines);
    print(paste('processed',numProcessed,'of',numLinesInFile,numProcessed/numLinesInFile,'%'));
    #if (numProcessed >= 2*numLinesToRead) {
    #  print('early out, testing, REMOVE THIS!');
    #  break;
    #}
    if (length(lines) < numLinesToRead) {
      break;
    }
  }
  close(con);
}

twitFN <- './data/en_US/en_US.twitter.txt';
twitNumLines <- 2360148;
blogsFN <- './data/en_US/en_US.blogs.txt';
blogsNumLines <- 899288;
newsNumLines <- 1010242;
newsFN <- './data/en_US/en_US.news.txt';

print('starting')
if (!file.exists('gramFreq.RData')) {
  print('gramFreq not found, re-creating');
  oneGramFreq <- new.env();
  twoGramFreq <- new.env();
  processFile(twitFN, twitNumLines, oneGramFreq, twoGramFreq);
  processFile(blogsFN, blogsNumLines, oneGramFreq, twoGramFreq);
  processFile(newsFN, newsNumLines, oneGramFreq, twoGramFreq);
  print('save freq')
  save(oneGramFreq,twoGramFreq,file='gramFreq.RData')  
} else {
  print('loading gramFreq');
  load('gramFreq.RData');
}

if (is.null(oneGramFreq) || is.null(twoGramFreq)) {
  stop('oneGram or twoGram missing');
}

#print('remove low frequency tokens twoGram');
#filterLowFrequencyWords(twoGramFreq);
#print('remove low frequency tokens oneGram');
#filterLowFrequencyWords(oneGramFreq);

oneGramFreqToDT <- function (e) {
  freqs <- lapply(ls(e), function(tok) { e[[tok]] })
  DT <- (data.table(tok=ls(e), freq=as.numeric(freqs)));
}

writeOneGramDT <- function(e, filename) {
  if (!file.exists(filename)) {
    print(paste('writing ',filename));
    DT <- oneGramFreqToDT(e);
    write.table(DT, file=filename, row.names=F, col.names = T);
  } else {
    print(paste(filename,'already exists, skipping'))
  }
}

writeTwoGramDTFile <- function(twoGram, filename) {
  if (file.exists(filename)) {
    print(paste(filename,'already exists, aborting'));
    return();
  } 
  print(paste('writing', filename));
  tok0s <- ls(twoGram)
  writeColNames <- T;
  numWritten <- 0;
  writeTwoGram <- function(tok0, e) {
    tok1s <- ls(e);
    freqs <- lapply(tok1s, function(tok1) { e[[tok1]]; });
    #print(paste(numWritten,'of',length(tok0s),'writing table',tok0,'with freq',oneGramFreq[[tok0]],'length',length(tok1s)));
    if (length(tok1s) > 0) {
      DT <- data.table(tok0=rep(tok0,length(tok1s)),tok1=tok1s,freq=as.numeric(freqs));
      write.table(DT, file=filename, append=!writeColNames, col.names = writeColNames, row.names = F);
      writeColNames <<- F;
    }
    numWritten <<- numWritten + 1
  }
  lapply(tok0s, function(tok0){
    if (numWritten %% 100 == 0) {
      print(paste(numWritten,'of',length(tok0s)));
    }
    writeTwoGram(tok0, twoGram[[tok0]]);
  }); 
}



freqsToPctEnv <- function(e) {
  nWords <- NULL;
  nOccurances <- 0;
  lapply(ls(e), function(tok) { 
    if (is.numeric(e[[tok]])) {
      nOccurances <<- nOccurances + e[[tok]]; 
    }
  })
  lapply(ls(e), function(tok){
    if (is.environment(e[[tok]])) {
      freqsToPctEnv(e[[tok]]);
    } else {      
      e[[tok]] = e[[tok]]/nOccurances;
    }
  });
}

oneGramFreqDTFilename <- 'oneGramFreqDT.RData';
twoGramFreqDTFilename <- 'twoGramFreqDT.RData';
oneGramPctDTFilename <- 'oneGramPctDT.RData';
twoGramPctDTFilename <- 'twoGramPctDT.RData';
writeOneGramDT(oneGramFreq, oneGramFreqDTFilename);
writeTwoGramDTFile(twoGramFreq, twoGramFreqDTFilename);

oneGramPct <- oneGramFreq;
twoGramPct <- twoGramFreq;

print('freqsToPctEnv(oneGramPct)');
freqsToPctEnv(oneGramPct);
print('freqsToPctEnv(twoGramPct)');
freqsToPctEnv(twoGramPct);

writeOneGramDT(oneGramPct, oneGramPctDTFilename);
writeTwoGramDTFile(twoGramPct, twoGramPctDTFilename)


#twoGramFreqToDT <- function(e) {
#  dts <- lapply(ls(e), function(tok) {
#    a <- oneGramFreqToDT(e[[tok]])
#    data.table(tok0=rep(tok,nrow(a)),tok1=a$t,f=a$f)
#  });
#  rbindlist(dts);
#}

#print('save')
#save(oneGramPct,twoGramPct,file='gramPct.RData')
# system.time(load('grams.RData'))