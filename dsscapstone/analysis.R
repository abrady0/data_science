# project data analysis
# install.packages('tm')
# install.packages('parser')
setwd('~/ocw/data_science/dsscapstone');
set.seed(1234);
library(data.table)
library(ggplot2)

#twitLines <- readRandomLinesFromFile(twitFN, twitNumLines/20, 10000);
#twitLines <- readLinesFromFile(twitFN, -1)
#loveLineNums <- grep('love', twitLines)
#hateLineNums <- grep('hate', twitLines)
#length(loveLineNums)/length(hateLineNums)

# i know how you feel.. i have biostats on tuesday and i have yet to study =/
#grep('biostats', twitLines)
#grep('^A computer once beat me at chess, but it was no match for me at kickboxing$',twitLines,perl = T)


readLinesFromFile <- function(fileName, numLines) {
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

twitSampleLines <- readRandomLinesFromFile(twitFN, twitNumLines/20, 2000);
blogsSampleLines <- readRandomLinesFromFile(blogsFN, blogsNumLines/20, 2000);
newsSampleLines <- readRandomLinesFromFile(newsFN, newsNumLines/20, 2000);

allSampleLines <- c(twitSampleLines, blogsSampleLines, newsSampleLines)
allSampToks <- tokenizeLines(allSampleLines, 'en');

filterTok <- function(tok) {
  tok[nchar(tok)>0]
}

sampToks <- filterTok(allSampToks)

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
sampTokFreqEnv <- toksToFreqs(sampToks)
oneGram <- sampTokFreqEnv[[1]]
twoGram <- sampTokFreqEnv[[2]]

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
  dt[order(freq,decreasing = T),]
}
sampOneGramFreq <- freqsToDT(oneGram)
sampTwoGramFreq <- freqsToDT(flattenNGram(twoGram))

mean(sampOneGramFreq$freq)
sd(sampOneGramFreq$freq)
s0 <- sampOneGramFreq[freq>50,]
barplot(s0$freq, names.arg = s0$names)
max(sampTwoGramFreq$freq)
mean(sampTwoGramFreq$freq)
median(sampTwoGramFreq$freq)
s1 <- sampTwoGramFreq[freq>4,]
barplot(s1$freq, names.arg = s1$names)

