# TEST
install.packages('RUnit')
require(RUnit)

df <- read.table('http://data.princeton.edu/eco572/datasets/preston21long.dat', col.names = c('country', 'ageg', 'pop', 'deaths'))
df$deaths
pop=df$pop
stdpop=stnd
rates1<-ageadjust.direct(count=df$deaths,pop=df$pop,stdpop=stnd)

rates2 <- dstdize(data=df, charvar = 'deaths', popvar = 'pop', stratavars = 'ageg', by = 'country', using = NA)

# rates1 == rates2
expect_that(rates1$adj.rate,equals(rates2$adj.rate))
?expect_that
