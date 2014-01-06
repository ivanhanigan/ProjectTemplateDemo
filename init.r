
####
# MAKE SURE YOU HAVE THE CORE LIBS
if (!require(ProjectTemplate)) install.packages('ProjectTemplate', repos='http://cran.csiro.au'); require(ProjectTemplate)
if (!require(lubridate)) install.packages('lubridate', repos='http://cran.csiro.au'); require(lubridate)
if (!require(reshape)) install.packages('reshape', repos='http://cran.csiro.au'); require(reshape)
if (!require(plyr)) install.packages('plyr', repos='http://cran.csiro.au'); require(plyr)
if (!require(ggplot2)) install.packages('ggplot2', repos='http://cran.csiro.au'); require(ggplot2)
if(!require(mgcv)) install.packages('mgcv', repos='http://cran.csiro.au');require(mgcv);
require(splines)
if(!require(NMMAPSlite)) install.packages('NMMAPSlite', repos='http://cran.csiro.au');require(NMMAPSlite)
rootdir <- getwd()

cat('
 #######################################################################
 ## The R code is free software; please cite this paper as the source.  
 ## Copyright 2012, Ivan C Hanigan <ivan.hanigan@gmail.com> 
 ## This program is free software; you can redistribute it and/or modify
 ## it under the terms of the GNU General Public License as published by
 ## the Free Software Foundation; either version 2 of the License, or
 ## (at your option) any later version.
 ## 
 ## This program is distributed in the hope that it will be useful,
 ## but WITHOUT ANY WARRANTY; without even the implied warranty of
 ## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ## GNU General Public License for more details.
 ## Free Software
 ## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 ## 02110-1301, USA
 #######################################################################
')

####
# init
require('ProjectTemplate')
create.project('analysis',minimal=TRUE)

####
# init dir
dir('analysis')

####
# init reports
dir.create('analysis/reports')

####
# init additional directories for project management
analysisTemplate <- function(rootdir = getwd()){
 setwd(rootdir)
 # first dir
 dir.create(file.path(rootdir,'analysis'))
 dir.create(file.path(rootdir,'analysis','reports'))
 dir.create(file.path(rootdir,'data'))
 dir.create(file.path(rootdir,'document'))
 dir.create(file.path(rootdir,'metadata'))
 dir.create(file.path(rootdir,'references'))
 # dir.create(file.path(rootdir,'tools'))
 # dir.create(file.path(rootdir,'versions'))
 dir.create(file.path(rootdir,'admin'))
 dir.create(file.path(rootdir,'admin','proposal'))
 dir.create(file.path(rootdir,'admin','budget'))
 file.create(file.path(rootdir,'admin','DataManagementPlan.txt'))
 }

####
# init additional directories for project management
analysisTemplate()

dir()
