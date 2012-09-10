# Project: dstdize.r
# Author: ivanhanigan and sarahhenderson
# Maintainer: Who to complain to <ivan.hanigan@gmail.com>

dstdize <- function(data, charvar, popvar, stratavars,by, using = NA,print=T, time = NA){
  # This is called dstdize because it replicates the output of the 'dstdize' command in stata.
  # TODO
  # refactor naming and number of variables  
  
  #   data    = a pop with fields outcome, pop, agegrp and optional date
  #   charvar = the outcome
  #   popvar  = the study pop
  #   stratavars = stratifying variables ie age or sex
  #   by = potentially different study sites ie countries
  #   using = NA
  #   print=T
  #   time = NA
  #   
  # tried to replicate 
  # using data from 
  
  if (!require(plyr)) install.packages('plyr', repos='http://cran.csiro.au'); require(plyr)
  d <- data
  country <- by
  pop <- popvar
  deaths <- charvar
  ageg <- stratavars
  if(!is.na(time)){
    N = ddply(d, c(country, time), function(df) return(c(N = sum(df[,pop]),cases=sum(df[,deaths])))) 
  } else {
    N = ddply(d, c(country), function(df) return(c(N = sum(df[,pop]),cases=sum(df[,deaths]))))
  }
  d <- merge(d,N)
  d$popdist = d[,pop]/d[,'N']
  d$loc_crude = d[,deaths]/d[,pop]
  names(d) <- gsub('pop', 'loc_pop', names(d))
  names(d) <- gsub('loc_popdist', 'popdist', names(d)) # also changed loc_pop
  # TASK need to failsafe for timeseries if using is NA then the rates are standardised to moving target rather than standard
  if(length(!is.na(using))>1){
    standard <- using
  } else {
    standard <- ddply(d, ageg, function(df) return(c(population = sum(df$loc_pop))))
  }
  standard <- cbind(standard, sum(standard$population))
  d <- merge(d, standard, by = ageg)
  names(d)[ncol(d)] <- 'sum_n'
  
  # generate crude rate 
  d$crude = (d$cases/d$N)
  
  # and the adjusted rate
  d$product = d$loc_crude * d$population
  if(!is.na(time)){
    out <- ddply(d, c(country,time), function(df) return(c(adj_rate = sum(df$product))))
  } else {
    out <- ddply(d, c(country), function(df) return(c(adj_rate = sum(df$product))))        
  }        
  # and the Upper and lower CIs
  d$prod2 = (d$population^2*d$loc_crude*(1-d$loc_crude)/d$loc_pop)
  out <- merge(out, ddply(d, country, function(df) return(c(se = sum(df$prod2)))))
  out$se = sqrt(out$se)        
  out$ub = out$adj_rate + 1.96*out$se 
  out$lb = out$adj_rate - 1.96*out$se
  out <- merge(out, sum(standard$population))
  out$adjustedRate <- out$adj_rate / sum(standard$population)
  out$lci <- out$lb / sum(standard$population)
  out$uci <- out$ub / sum(standard$population)
  
  out<-merge(N,out)
  out$crudeRate <- out$cases/out$N
  if(!is.na(time)){
    out <- out[,c(country,time,'cases','N','crudeRate','adjustedRate','lci','uci')]
  } else {
    out <- out[,c(country,'cases','N','crudeRate','adjustedRate','lci','uci')]
  }        
  if(print==T){print(out)}
  return(out)
}

# TEST
# df <- read.table('http://data.princeton.edu/eco572/datasets/preston21long.dat', col.names = c('country', 'ageg', 'pop', 'deaths'))
# rates <- dstdize(data=df, charvar = 'deaths', popvar = 'pop', stratavars = 'ageg', by = 'country', using = NA)
