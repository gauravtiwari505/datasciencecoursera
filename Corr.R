corr <- function(directory, threshold = 0) 
  {
  #Setting the working directory
  setwd("/Users/gaurav.tiwari/Documents/Coursera_Data_Science/Lectures/Week 2")
  setwd(paste(getwd(),"/",directory,sep=""))
  s=data.frame()
  correlationVector = NULL
  len = length(list.files())
  for(i in 1:len)
  {
    
    a=read.csv(paste(formatC(i, width=3, flag=0),".csv",sep = ""),header = TRUE)
    s=rbind(s,a)
    rm(a)
  }
  s = na.omit(s)
  
  if (nrow(s)>threshold) 
     {
    correlationVector = c(correlationVector, cor(s[,2], s[,3]))
      }
  return(correlationVector)
  }
