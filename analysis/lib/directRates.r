
directRates <- function(analyte, standard_pop, stratify.var = c('dthdate')){       
 #  analyte = time series of outcomes abd populations, by age and sex
 #  standard_pop = standard
 # stratify.var = c('dthdate','sex') # by sex if wanted age rates for each sex, could also be by zone?
 # TODO 
 #  make this work with multiple study populations?
 # if study_pop = NA then will check if multiple study zones, will use the total population, if by time then will use mid point?
 if(!require(plyr)) install.packages('plyr',repos='http://cran.csiro.au'); require(plyr)

 # step 1 get the standard population
 # TODO generalise to the optional inclusion of a standard

 # step 2 for each time step calc the age specific rates in study, apply to standard pops
 # need to merge        
 analyte <- merge(analyte, standard_pop, all.x = T) #, by.x= 'age', by.y ='age')
 
 # get the daily age specific rates of the ROS and apply to standard
 # this is the expected number of deaths if the standard had had the same health experience as the study
 analyte$allcause_asr <- (analyte$allcause/analyte$pop) * analyte$standard_pop
 analyte$resp_asr <- (analyte$resp/analyte$pop) * analyte$standard_pop
 analyte$cvd_asr <- (analyte$cvd/analyte$pop) * analyte$standard_pop

        
 # step 3 sum expected deaths over age, stratify by stratify.var      
 dailystandard <- ddply(analyte, stratify.var, function(df) return(c(
  standard_pop_summed = sum(df$standard_pop),
  allcause_asr_summed = sum(df$allcause_asr),
  resp_asr_summed = sum(df$resp_asr), 
  cvd_asr_summed = sum(df$cvd_asr))))

 # and divide by standard population x 100,000 
 dailystandard$allcause_stndrate <- (dailystandard$allcause_asr_summed/dailystandard$standard_pop_summed) * 100000
 dailystandard$resp_stndrate <- (dailystandard$resp_asr_summed/dailystandard$standard_pop_summed) * 100000
 dailystandard$cvd_stndrate <- (dailystandard$cvd_asr_summed/dailystandard$standard_pop_summed) * 100000

 return(dailystandard)
 }
