# rankhospital selects a given state and based a a condition returns the 
# hosital which is best, worst a particular number in the ranking.  in the event of ties
# the winner is determined alphabetically.

# basic reads for assignment
rankhospital<-function(st,condition,num="best")
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
na.omit(outfilt)
outfilt2=outfilt
if(num=="worst"){
  outfilt2<-filter(outfilt,outfilt[,cc]==max(outfilt[,cc],na.rm=TRUE))
  num=1}
if(num=="best"){
  outfilt2<-filter(outfilt,outfilt[,cc]==min(outfilt[,cc],na.rm=TRUE))
  num=1}
# find the best (lowest 30 day mortality)
final<-na.omit(outfilt2[,c(7,2,cc)])
# set the names you want
names(final)<-(c("State","Hospital.Name","Rate"))
#sort
#final<-final[order(-Rate),]
#get the alphabetized first/top row
final<-arrange(final,final[,3],final[,2])
return(final[num,])
#View(final)
}
#test scripts
  
  rankhospital("TX","heart failure",4)
  rankhospital("MD","heart attack","worst")
  rankhospital("MN","heart attack",5000)
  rankhospital("NY","heart attack","best")
  
  




