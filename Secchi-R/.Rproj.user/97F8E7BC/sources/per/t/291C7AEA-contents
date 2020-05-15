# Plots secchi depth versus bottom and surface P
# DJW 12/31/19

library(stringr)

lake <- 'East Pond'
site <- 'EPDEP1'

# Load secchi data
datapath <- paste("~/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/",lake,sep="","/RawData/Secchi/")
filenames <- dir(datapath, pattern=paste(site,'_20',sep = ""))

i = 1
for(i in 1:length(filenames)){
  fullpath <- paste(datapath,filenames[i],sep = "")
  dat <- read.csv(fullpath, header = TRUE, stringsAsFactors = FALSE)
}