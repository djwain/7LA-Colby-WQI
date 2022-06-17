# This script takes all the In Situ files in a given year folder, trims the upcast, and combines them
# into one big csv file for contouring and other analyses
# DJW 7/25/19

library(stringr)
library(dplyr)

lake <- 'Messalonskee'
site <- 'MESSDEP1'
year <- '2019'

basefile <- paste(site,year,sep="_")

# Load data
folder <- paste("~/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/",lake,sep="","/RawData/InSitu/")
datapath <- paste(folder,year,sep = "","/") 
filenames <- dir(datapath, pattern=paste(basefile,'-',sep = ""))

outfile <- ""
outfilename <- paste(datapath,paste(basefile,"_InSitu.csv",sep = ""),sep = "")

datapath2 <- paste(folder,"All/",sep= "")
outfilename2 <- paste(datapath2,paste(basefile,"_InSitu.csv",sep = ""),sep = "")

for(i in 1:length(filenames)){
  fullpath <- paste(datapath,filenames[i],sep = "")
  
  
  suppressWarnings({
   if(str_detect(fullpath,"log") == TRUE) {
  # Some In Situ files have an extra header line for GPS so we need to cut different number of header files
 
     junk <- read.csv(fullpath, header = FALSE, stringsAsFactors = FALSE)
  
      skipno <- which(junk == "Created") - 1
    
  
      dat <- read.csv(fullpath, skip = skipno, header = TRUE, stringsAsFactors = FALSE)
  
  
  
 
  } else {
    junk <- read.csv(fullpath, header = FALSE, stringsAsFactors = FALSE)
    
    skipno <- which(junk == "Date Time") - 1
    skipno <-18
    dat <- read.csv(fullpath, skip = skipno, header = TRUE, stringsAsFactors = FALSE)  
    
    
    
    
      
  }
  })
  
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
  
  # Rename columns with data I need so that it doesn't matter what instrument is being used
  z_m <- dat[,grepl( "Depth" , names( dat ) ) ]
  
  # Trim data so only downcast is used
  maxdepi <-which.max(z_m)
  dat <- dat[1:maxdepi,]
  z_m <- z_m[1:maxdepi]
  
  T_C <-  dat[,grepl( "Temp" , names( dat ) ) ]
  T_C <- T_C[,1]
  
  DO_ppm <-  dat[,grepl( "RDO" , names( dat ) ) ]
  DO_ppm <- DO_ppm[,1]
  
  YearDay <- dat$YearDay
  
  dat1 <- cbind(YearDay,z_m,T_C,DO_ppm)

  
  outfile <- rbind(outfile, dat1)
  
} 
  
rownames(outfile) <- NULL

write.csv(outfile, outfilename)
write.csv(outfile, outfilename2)

