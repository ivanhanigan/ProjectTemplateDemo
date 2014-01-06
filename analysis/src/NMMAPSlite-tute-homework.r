
setwd('analysis')

require(splines)
if(!require(NMMAPSlite)) install.packages('NMMAPSlite');require(NMMAPSlite)
initDB('data')

cities <- getMetaData('cities')
head(cities)
# if we use a variable to set a city we can loop thru a list of city's later
city_i <- 'Chicago'
city <- subset(cities, cityname == city_i)$city
data <- readCity(city)
# we know from the NMMAPS papers that a smooth on time will control
# for unmeasured temporal confounders such as season, trend and flu epidemics
data$yy <- substr(data$date,1,4)
numYears<-length(names(table(data$yy)))
str(data)

fit2 <- glm(cvd ~ agecat + pm10tmean +
 ns(tmax, df = 8) + ns(dptp, df = 4) + ns(time, df = 7*numYears),
 data = data, family = poisson)

# plot responses
par(mfrow=c(2,2))
termplot(fit2, se =T,terms='agecat')
termplot(fit2, se =T,terms='pm10tmean')
termplot(fit2, se =T,terms='ns(tmax, df = 8)')
attr(terms(fit2),'term.labels')
termplot(fit2, se =T,terms='ns(time, df = 7 * numYears)')
dev.off()

fmtSignif <- function(x,signifpl=signifpls){
 # a function to set decimal places with trailing zero for labels
 sapply(signif(as.numeric(x),signifpl), sprintf, fmt=paste("%#.",signifpls,"g",sep=""))
}

modout <- summary(fit2)$coeff

i <- which(row.names(modout) == 'pm10tmean')
# for a standardised amount of variation in exposure use the IQR
delta <- IQR(data$pm10tmean, na.rm=T)
RR <- exp(modout[i,1] * delta)
RRlci <- exp((modout[i,1] - 1.96 * modout[i,2]) * delta)
RRuci <- exp((modout[i,1] + 1.96 * modout[i,2]) * delta)

print(paste('RR = ',fmtSignif(RR,5,5),' (',fmtSignif(RRlci,5,5),', ',fmtSignif(RRuci,5,5),')',sep=''))

cround = function(x,n){
# R documentation for round says for rounding off a 5, the IEC 60559 standard is expected to be used, go to the even digit.
# We think most people expect numbers ending in .5 to round up, not the nearest even digit.
# We decided that we'd round up from .5
# http://alandgraf.blogspot.com.au/2012/06/rounding-in-r.html
# It is a little comforting knowing that there is a logic behind it and that R is abiding to some standard. 
# But why isn't MATLAB abiding by the same standard? Also, I think most people expect numbers ending in .5 to round up, not the nearest even digit.  
# from comments
# Andrew wrote "Also, I think most people expect numbers ending in .5 to round up (not the nearest even digit)". This kind of rounding is in German #called "kaufmännische Rundung" (rounding in commerce). For this purpose I use the following function:
# Definition of a function for "rounding in commerce"
vorz = sign(x)
z = abs(x)*10^n
z = z + 0.5
z = trunc(z)
z = z/10^n
z*vorz
}

print(paste('RR = ',cround(RR,4),' (',cround(RRlci,4),', ',cround(RRuci,4),')',sep=''))

###########################################################################
# newnode: other show RR
             
  RR; RRlci; RRuci
  round(RR,4)
  round(RRlci,6)
  ?round

######################################################
# get coefficients and RRs
collectResults <- function(fit, name, covar, modeloutputsTable = NA){
  # a tool for extracting the results from our models
  # in to a table for the document
  modout <- data.frame(name,
   t(
    summary(fit)$coeff[
     which(row.names(summary(fit)$coeff) == covar),]
    ))

  modout$RR <- exp(modout[,2])
  modout$RRlci <- exp(modout[,2] - 1.96 * modout[,3])
  modout$RRuci <- exp(modout[,2] + 1.96 * modout[,3])

  modout$RRPct <- (exp(modout[,2])-1)*100
  modout$RRlciPct <- (exp(modout[,2] - 1.96 * modout[,3])-1)*100
  modout$RRuciPct <- (exp(modout[,2] + 1.96 * modout[,3])-1)*100
  colnames <- c('model', 'beta', 'se', 'z', 'p', 'RR','RRlci','RRuci','RRPct','RRlciPct','RRuciPct')
  names(modout) <-  colnames

  if(exists('modeloutputsTable')){
   modeloutputs <- rbind(modeloutputsTable,modout)
  } else {
   modeloutputs <- as.data.frame(matrix(nrow=0,ncol=11))
   names(modeloutputs) <-  colnames
   modeloutputs <- rbind(modeloutputs,modout)
  }

 return(modeloutputs)

 }

######################################################
# get the RRs
results_out <- as.data.frame(matrix(nrow=0,ncol=11))
names(results_out) <- c('model', 'beta', 'se', 'z', 'p', 'RR','RRlci','RRuci','RRPct','RRlciPct','RRuciPct')
results_out <- collectResults(fit=fit2, name='nmmaps',
                          covar='pm10tmean',
                          modeloutputsTable = results_out)
t(as.data.frame(results_out))
results_out
