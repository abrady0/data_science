# project data analysis
# install.packages('tm')
# install.packages('parser')
setwd('~/ocw/data_science/dsscapstone');

set.seed(1234);

readLinesFromFile <- function(fileName, numLines) {
  con=file(fileName,open="r");
  lines=readLines(con, numLines);
  close(con);
  # lines <- tolower(lines);
  lines
}

readRandomLinesFromFile = function(fileName, numLinesInFile, numToSample) {
  linesToGet <- sample(numLinesInFile, numToSample);
  lines <- readLinesFromFile(fileName, max(linesToGet));
  lines[linesToGet];
}

#library('tm')
#twitDoc <- PlainTextDocument(twitLines,description='twitter lines');
#twitTermFreq <- termFreq(twitDoc);

twitFN <- './data/en_US/en_US.twitter.txt'
twitNumLines <- 2360148 #system(paste('wc -l ',twitFN))
#twitLines <- readRandomLinesFromFile(twitFN, twitNumLines/20, 10000);
twitLines <- readLinesFromFile(twitFN, -1)
loveLineNums <- grep('love', twitLines)
#hateLineNums <- grep('hate', twitLines)
#length(loveLineNums)/length(hateLineNums)

# i know how you feel.. i have biostats on tuesday and i have yet to study =/
#grep('biostats', twitLines)
#grep('^A computer once beat me at chess, but it was no match for me at kickboxing$',twitLines,perl = T)

