
# For our current analysis, we're interested in the total 
# number of occurrences of each letter in the first and 
# second letter positions and not in the words themselves.
# compute aggregates
first.letter.counts <- ddply(letters, c('FirstLetter'), 
  nrow)
second.letter.counts <- ddply(letters, c('SecondLetter'), 
  nrow)
