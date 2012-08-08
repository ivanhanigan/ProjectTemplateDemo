
####
# this is the start of the analysis, 
# assumes the init.r file has been run
if(file.exists('analysis')) setwd('analysis')  
Sys.Date()
# keep a track of the dates the analysis is rerun
getwd()
# may want to keep a reference of the directory 
# the project is in so we can track the history

####
# analysis get tutorial data
download.file('http://projecttemplate.net/letters.csv.bz2', 
  destfile = 'data/letters.csv.bz2', mode = 'wb')

####
# analysis load
require(ProjectTemplate)
load.project()

tail(letters)

# edit the config file and turn munge on
 # load.project()
 # edit the config file and turn munge off
 # or my preference
 source('munge/01-A.r')
# which can be included in our first analysis script
# but subsequent analysis scripts can just call load.project() 
# without touching the config file

cache('first.letter.counts')
cache('second.letter.counts')

source('src/generate_plots.r')

# now run LaTeX on the file in reports/letters.tex
