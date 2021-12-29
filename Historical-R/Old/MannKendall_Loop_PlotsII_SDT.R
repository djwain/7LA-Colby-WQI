

### FOR MANN KENDALL

library(Kendall)
## GETTING LIST OF UNIQUE LAKE NAMES
lakenames<-unique(SDT_LS$MTB)  # change the field to be the one for your data
num<-length(lakenames)


## A NEW DATAFRAME FOR MannKendall results
SDT_LS_res<-data.frame(matrix(ncol=3,nrow=num))
colnames(SDT_LS_res)<-c('MTB','tau','pval')
w=1
## A NEW LOOP



for(w in 1:num)
{
  s<-subset(SDT_LS,MTB==lakenames[w], select=c(Year,SDT_LS))  # enter your data set name in the code
  mk<-MannKendall(s[,2])
  SDT_LS_res[w,1]<-lakenames[w]
  SDT_LS_res[w,2]<-round(mk$tau[1],3)
  SDT_LS_res[w,3]<-round(mk$sl[1],3)
}

#create single pdf with all graphs:

pdf(file='SDT_LS_Graphs.pdf', paper='letter', width=7, height=7.5, onefile=T)
par(mfrow=c(2,2)) 


for(w in 1:num)
{
  s<-subset(SDT_LS, MTB==lakenames[w], select=c(Year,SDT_LS))  # enter your data set name in the code
  mk<-MannKendall(s[,2])
  
  yhigh<-max(s$SDT_LS)+2
  
  plot(s$Year,s$SDT_LS, las=1, ylim=c(0,yhigh),ylab="Average Late Summer SDT (m)",xlab="Year")   # INSERT YOUR PLOT STATEMENT
  
  lines(lowess(s$Year,s$SDT_LS),col=4)
  titles<-SDT_LS$LakeName[which(SDT_LS$MTB==lakenames[w])] [1]
  titles2<-SDT_LS$MIDAS[which(SDT_LS$MTB==lakenames[w])] [1]
  titles3<-SDT_LS$TB[which(SDT_LS$MTB==lakenames[w])][1]
  mtext(text=paste(titles, "\n", " (MIDAS ",titles2," - Basin ",titles3,")",sep = ""), side=3, line=1.9, font=2)
  mtext(text=sprintf("tau=%1.3f",(mk$tau[1])),adj=0, side=3, line=0.6, cex=.8)
  mtext(text=sprintf("p=%1.3f",(mk$sl[1])),adj=1, side=3,line=0.6, cex=.8)
  
  
}

dev.off()


#create individual tiff files for all graphs:

w=1

for(w in 1:num)

{
  s<-subset(SDT_LS, MTB==lakenames[w], select=c(Year,SDT_LS))  # enter your data set name in the code
  mk<-MannKendall(s[,2])
  
  yhigh<-max(s$SDT_LS)+2
  
  
  png(file=paste("SDT_LS_MK_",lakenames[w],".png", sep=""),
       units="in",width=5, height=5.5, res=400)
  
  
  plot(s$Year,s$SDT_LS, las=1, ylim=c(0,yhigh),ylab="Average Late Summer SDT (m)",xlab="Year")   # INSERT YOUR PLOT STATEMENT
  
  lines(lowess(s$Year,s$SDT_LS),col=4)
  titles<-SDT_LS$LakeName[which(SDT_LS$MTB==lakenames[w])] [1]
  titles2<-SDT_LS$MIDAS[which(SDT_LS$MTB==lakenames[w])] [1]
  titles3<-SDT_LS$TB[which(SDT_LS$MTB==lakenames[w])][1]
  mtext(text=paste(titles, "\n", " (MIDAS ",titles2," - Basin ",titles3,")",sep = ""), side=3, line=1.9, font=2)
  mtext(text=sprintf("tau=%1.3f",(mk$tau[1])),adj=0, side=3, line=0.6, cex=.8)
  mtext(text=sprintf("p=%1.3f",(mk$sl[1])),adj=1, side=3,line=0.6, cex=.8)

dev.off()

 }



