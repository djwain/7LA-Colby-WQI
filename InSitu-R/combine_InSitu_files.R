# This script takes all the In Situ files in a given year folder, trims the upcast, and combines them
# into one big csv file for contouring and other analyses
# DJW 7/25/19

library(stringr)

lake <- 'Salmon'
site <- 'SPDEP1'
year <- '2018'

basefile <- paste(site,year,sep="_")

# Load data
folder <- paste("~/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/",lake,sep="","/RawData/InSitu/")
datapath <- paste(folder,year,sep = "","/") 
filenames <- dir(datapath, pattern=paste(basefile,'-',sep = ""))

outfile <-""
outfilename <- paste(datapath,paste(basefile,"_InSitu.csv",sep = ""),sep = "")

datapath2 <- paste(folder,"All/",sep= "")
outfilename2 <- paste(datapath2,paste(basefile,"_InSitu.csv",sep = ""),sep = "")

for(i in 1:length(filenames)){
  fullpath <- paste(datapath,filenames[i],sep = "")
  
  # Some In Situ files have an extra header line for GPS so we need to cut different number of header files
  suppressWarnings({
    junk <- read.csv(fullpath, header = FALSE, stringsAsFactors = FALSE)
    if(str_detect(junk,"GPS") == TRUE) {
      skipno = 10
    }else {
      skipno = 9
    }
  })
  dat <- read.csv(fullpath, skip = skipno, header = TRUE, stringsAsFactors = FALSE)
  
  # Going to put dates as Year Day (day of the year), e.g. Jan 1 = 1, etc. This is because the serial dates
  # in Excel for Mac, Excel for Windows, and Matlab all use different origins and I want this script to work
  # across platforms. The In Situ files have two different date formats in the data column: YYYY-MM-DD and
  # MM/DD/YYYY, so going to try extracting the date from the file name or the header.
  pos = regexpr('_', filenames[i])
  dyr <- substr(filenames[i],pos+1,pos+4)
  dmo <- substr(filenames[i],pos+6,pos+7)
  dd <- substr(filenames[i],pos+9,pos+10)
 newdate <- paste(dyr,dmo,dd,sep = "/")
 yr0 = as.numeric(year) - 1
 dat$YearDay <- as.numeric(as.Date(newdate)-as.Date(0, origin=paste(yr0,"12-31",sep = '-')))
  
  
  # Trim data so only downcast is used
  maxdepi <-which.max(dat$Depth..m.)
  dat <- dat[1:maxdepi,]
  
  
  
 
  outfile <- rbind(outfile, dat)
  
}

write.csv(outfile, outfilename)
write.csv(outfile, outfilename2)

