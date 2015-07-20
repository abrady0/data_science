setwd('~/ocw/data_science/dsscapstone');
set.seed(1234);
library(data.table)
library(ggplot2)

readLinesFromFile <- function(fileName, numLines = -1L) {
  con <- file(fileName,open="r");
  lines <- readLines(con, numLines);
  close(con);
  lines;
}

readRandomLinesFromFile = function(fileName, numLinesInFile, numToSample) {
  linesToGet <- sample(numLinesInFile, numToSample);
  lines <- readLinesFromFile(fileName, max(linesToGet));
  lines[linesToGet];
}

library('tm')
tokenizeLines <- function(lines, lang) {
  lines <- removeWords(
    removePunctuation(tolower(lines)),
    stopwords(kind = lang)
  );
  MC_tokenizer(lines)
}

twitFN <- './data/en_US/en_US.twitter.txt';
twitNumLines <- 2360148;
blogsFN <- './data/en_US/en_US.blogs.txt';
blogsNumLines <- 899288;
newsNumLines <- 1010242;
newsFN <- './data/en_US/en_US.news.txt';

twitLines <- readLinesFromFile(twitFN);
blogsLines <- readLinesFromFile(blogsFN);
newsLines <- readLinesFromFile(newsFN);

allLines <- c(twitLines, blogsLines, newsLines)
allToks <- tokenizeLines(allLines, 'en');

filterTok <- function(tok) {
  tok[nchar(tok)>0]
}

toks <- filterTok(allToks)

toksToFreqs <- function(toks) {
  oneGram <- new.env();
  twoGram <- new.env();
  threeGram <- new.env();
  updateCount <- function (env, tok) {
    total <- 0;
    if (!is.null(env[[tok]])) {
      total <- env[[tok]];
    }
    env[[tok]] <- total + 1;
  }
  for (i in 1:length(toks)) {
    tok <- toks[[i]];
    updateCount(oneGram, tok);
    if (i < length(toks)) {
      env <- twoGram[[tok]];
      if (is.null(env)) {
        env <- twoGram[[tok]] <- new.env();
      }
      updateCount(env, toks[[i+1]]);
    }
  }
  c(oneGram, twoGram);
}
tokFreqEnv <- toksToFreqs(toks)
oneGramFreq <- tokFreqEnv[[1]]
twoGramFreq <- tokFreqEnv[[2]]

flattenNGram <- function(e) {
  res <- new.env();
  flattenRec <- function(prefix, e) {
    lapply(ls(e), function(var) {
      val <- e[[var]];
      newPrefix = var;
      if (!is.null(prefix)) {
        newPrefix = paste(prefix,sep='_',var);          
      }
      if (is.environment(val)) {
        flattenRec(newPrefix, val);
      } else {
        res[[newPrefix]] <- val; 
      }
    });
  }
  flattenRec(NULL, e);
  res;
}
freqsToDT <- function(e) {
  vars <- ls(e)
  dt <- data.table(names=vars, freq=as.numeric(lapply(vars, function(var){ e[[var]]})))
  res <- dt[order(freq,decreasing = T),]
  # reorder factors to be in the alphabetical order
  res$names <- factor(res$names, levels=res$names);
  res;
}

oneGramFreqDT <- freqsToDT(oneGramFreq)
twoGramFreqDT <- freqsToDT(flattenNGram(twoGramFreq))

#barplot(s0$freq, names.arg = s0$names)
s0 <- oneGramFreqDT[freq>200,]
qplot(s0$names, s0$freq, geom='bar', stat='identity')
s1 <- twoGramFreqDT[freq>15,]
qplot(s1$names, s1$freq, geom='bar', stat='identity')
s1 <- twoGramFreqDT[freq<=15 & freq > 12,]
qplot(s1$names, s1$freq, geom='bar', stat='identity')

freqsToPctEnv <- function(e) {
  res <- new.env();
  nWords <- NULL;
  lapply(ls(e), function(tok){
    if (is.environment(e[[tok]])) {
      res[[tok]] <- freqsToPctEnv(e[[tok]]);
    } else {      
      if (is.null(nWords)) {
        DT <- freqsToDT(e);
        nWords <- sum(DT$freq);
      }
      res[[tok]] = e[[tok]]/nWords
    }
  });
  res;
}
oneGramPct <- freqsToPctEnv(oneGramFreq)
twoGramPct <- freqsToPctEnv(twoGramFreq)
oneGramPctDT <- freqsToDT(oneGramPct)
twoGramPctDT <- freqsToDT(flattenNGram(twoGramPct))
s0 <- oneGramPctDT[freq>.002,]
qplot(s0$names, s0$freq, geom='bar', stat='identity')
s1 <- twoGramPctDT[freq>15,]
qplot(s1$names, s1$freq, geom='bar', stat='identity')
s1 <- twoGramPctDT[freq<=15 & freq > 12,]
qplot(s1$names, s1$freq, geom='bar', stat='identity')

# save(oneway)

# open connection
# read block of lines
# tokenize
# factorize tokens?
# accumulate frequencies
# normalize
# save


