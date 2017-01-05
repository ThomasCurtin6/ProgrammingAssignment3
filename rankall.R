# Gets a list of hospitals organized by state which are the best, worst or specific index value


# basic reads for assignment
rankall<-function(condition,num="best")
  {
  #read the file
outc1<-read.csv("outcome-of-care-measures.csv",colClasses="character",na.strings="Not Available",stringsAsFactors=FALSE)
#convert outcomes to numeric

#check conditions and assign code to variable cc
cc<-0
if(condition=="heart attack")
  {
  cc<-11
  }

if(condition=="heart failure")
  {
  cc<-17
  }

if(condition=="pneumonia")
  {
  cc<-23
  }

  if(cc==0)
  {
    return(paste(condition," not found"))
  }

stlist<-unique(outc1[,7])
rvec<-matrix(nrow=length(stlist),ncol=3)
for( i in 1:length(stlist))
{
st=stlist[i]
  

outc1[,cc]<-as.numeric(outc1[,cc])
# use dplyr function filter to get the records for the state

outfilt<-filter(outc1,outc1[,7]==st)
outfilt2<-outfilt

if(num=="worst")
   {
    mx<-max(outfilt[,cc],na.rm=TRUE)
    outfilt2<-subset(outfilt,outfilt[,cc]==mx)
    num=which(outfilt[,cc]==mx)
    }

if(num=="best")
  {
  mn<-min(outfilt[,cc],na.rm=TRUE)
  outfilt2<-filter(outfilt,outfilt[,cc]==mn)
  num=1
  }

# find the best (lowest 30 day mortality)
final<-outfilt2[,c(7,2,cc)]
# set the names you want
names(final)<-(c("State","Hospital.Name","Rate")) #set the name of the 
#sort
#final<-final[order(-Rate),]
#get the alphabetized first/top row
#final<-arrange(final,final[,3],final[,2]) #sort the values
rvec[i,1]<-st  #load the state abbrev
rvec[i,2]<-final[num,2]   #load the hospital name
rvec[i,3]<-final[num,3]
#View(final)
}

r<-as.data.frame((rvec))
names(r)<-(c("state","hospital","rate"))
r
}

  
  




