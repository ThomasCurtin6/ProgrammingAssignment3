# best.r takes the arguments of state abbreviation and one of three conditions.


# basic reads for assignment
best<-function(st,condition)

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
# check state against unique list of available states
  if(!st %in% stlist)
    {
    return("Bad State")
    }

outc1[,cc]<-as.numeric(outc1[,cc])
# use dplyr function filter to get the records for the state
outfilt<-filter(outc1,outc1[,7]==st)
# filter the records again for the min value without the N/A values
outfilt2<-filter(outfilt,outfilt[,cc]==min(outfilt[,cc],na.rm=TRUE))
# find the best (lowest 30 day mortality)
final<-outfilt2[,c(7,2,cc)]
# set the names you want
names(final)<-(c("State","Hospital.Name","Rate"))
#sort
arrange(final,final[,2])
#get the alphabetized first/top row
final[1,]
}

#test scripts
  best("TX","heart attack")
  best("TX","heart failure")
  best("MD","heart attack")
  best("MD","pneumonia")
  best("BB","heart attack")
  best("NY","hert attack")
  
  




