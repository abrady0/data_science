# observation: 
# - we keep running out of memory trying to build ngrams beyond two, and even one and two grams take forever
# - reading a datatable in is very fast
# - data.table lookups are fast as well
# - adding rows to data.tables is expensive

# because R is slow at hash tables, and because R data.table updates are slow, we should process our data with another
# language built for this:
# - R loads and cleans the text lines then writes them out
# - external process turns the lines into fread-able files of 1,2,3, etc. grams
# - these files are used for our engine

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

processFile <- function(fileName, numLinesInFile, nGramEnvs) {
  print(paste('processFile',fileName));
  numLinesToRead <- 20000;
  numProcessed <- 0
  con <- file(fileName,open="r");
  repeat {
    lines <- readLines(con, numLinesToRead);
    toks <- tokenizeLines(lines, 'en');
    toksToFreqs(toks, nGramEnvs);
    
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
oneGramFreq <- new.env();
twoGramFreq <- new.env();
threeGramFreq <- new.env();
nGramFreqs <- c(oneGramFreq,twoGramFreq,threeGramFreq);
processFile(twitFN, twitNumLines, nGramFreqs);
print('testing, uncomment!')
#processFile(blogsFN, blogsNumLines, nGramFreqs);
#processFile(newsFN, newsNumLines, nGramFreqs);
print('save freq');
for(i in 1:length(nGramFreqs)) {
  fn <- paste('n',i,'gramFreq.table',sep='');
  saveNgramEnv(nGramFreqs[[i]], fn);
}
#  save(oneGramFreq,twoGramFreq,file='gramFreq.RData')  

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