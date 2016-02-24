pollutantmean<-function(directory,pollutant,id)
{
  #Setting the working directory
  setwd("/Users/gaurav.tiwari/Documents/Coursera_Data_Science/Lectures/Week 2")
  setwd(paste(getwd(),"/",directory,sep=""))
  s=data.frame()
  for(i in 1:length(id))
  {
    
    a=read.csv(paste(formatC(id[i], width=3, flag=0),".csv",sep = ""),header = TRUE)
    s=rbind(s,a)
    rm(a)
  }
  z=na.omit(s[pollutant])
  add=sum(z)
  count=nrow(z)
  return(add/count)
}

