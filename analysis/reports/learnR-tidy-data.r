
################################################################
# name:load
install.packages('languageR')
require(languageR)
data(lexdec)
head(lexdec)
full_list <- lexdec

################################################################
#ddply
install.packages('plyr')
require(plyr)
head( ddply(full_list, c("Subject","Class"), function(df)mean(df$RT)))
# let’s get the column labelled:
head(ddply(full_list, c("Subject","Class"), function(df)
           return(c(AVERAGE=mean(df$RT)))))
# Great! Now let’s add some more columns to output
head(ddply(full_list, c("Subject","Class"), function(df)
           return(c(AVERAGE=mean(df$RT),
                    MEDIAN=median(df$RT),
                    SE=sqrt(var(df$RT)/length(df$RT))
                    )
                  )
           )
     )

################################################################
# sqldf
install.packages('sqldf')
require(sqldf)
head(
sqldf("SELECT SUBJECT, CLASS, AVG(RT) AS AVERAGE,MEDIAN(RT) AS MEDIAN,
         SQRT((VARIANCE(RT)/COUNT(RT))) AS SE
       FROM full_list
       GROUP BY SUBJECT, CLASS
      ")
)

################################################################
# data.table
install.packages('data.table')
require(data.table)
dps_dt = data.table(full_list)
head(
dps_dt[,list(AVERAGE=.Internal(mean(RT)),
             MEDIAN=median(RT),
             SE= sqrt(var(RT)/length(RT))),
             by=list(Subject,Class)
             ]
     )

################################################################
# tapply
head(
  tapply(as.numeric(full_list$RT),
         list(full_list$Subject,  full_list$Class),
         median)
  )

################################################################
# aggregate
aggregated_output <- aggregate(RT ~ Subject * Class, data=full_list,
                               FUN=median)
head(arrange(aggregated_output,Subject,Class))
