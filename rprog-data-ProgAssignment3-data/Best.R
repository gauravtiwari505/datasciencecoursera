best=function(state,outcome)
{
        illness=NULL
        state_out =NULL
        ## Read outcome Data in the variable ocm
        ocm<-read.csv("outcome-of-care-measures.csv",colClasses = "character",header=TRUE)
        
        ## Check that state and outcome are valid
        if( outcome == "heart attack")
        {
         illness=as.numeric(ocm[,11])       
        }
        else if(outcome == "heart failure")
        {
         illness = as.numeric(ocm[,17])                
        }
        else if(outcome == "pneumonia")
        {
         illness = as.numeric(ocm[,23])
        }
        else
        stop("Invalid Disease parameter passed")
        if(is.element(state,unique(ocm[,7]))==0)
        {
                stop("Invalid State Entered")
        }
        else
        {
                ## Return hospital name in that state with lowest 30-day death rate
                ##ocm=ocm[ocm$State == state,]
                ##head(illness)
                ds=suppressWarnings(cbind(ocm[,7],ocm[,2],illness))
                ds=ds[ds[,1]==state,]
                ##data=data[data$State==state,] ##Filter the dataset by selecting row as the state
                colnames(ds)=c("State","Hospital Name","Mortality")
                ds = ds[!is.na(ds[,3]),]
                ds=ds[order(as.numeric(ds[,3]),ds[,2]),]
                return(ds[1,2])
                
        }
        
        
       
       
       
}