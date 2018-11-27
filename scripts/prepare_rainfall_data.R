
prepare_rainfall_data <- function() {
        #### Download the data - takes time to run
        myDF <- download_rainfall_data()
        
        #### Assign ring information
        myDF$Ring <- sub("FACE_R", "", myDF$Source)
        myDF$Ring <- sub("_T1.*", "", myDF$Ring)
        myDF$Ring <- as.numeric(myDF$Ring)  
        myDF <- myDF[order(myDF$DateTime),]
        
        ## We have two record per measurement time, need to average them hout
        aDF <- aggregate(Rain_mm_Tot~DateTime, FUN=mean, na.rm=T, keep.names=T, data=myDF)
        
        aDF$Month <- format(as.Date(aDF$DateTime), "%Y-%m")
        aDF$Month <- as.Date(paste0(aDF$Month,"-1"), format = "%Y-%m-%d") 
        aDF$Date <- as.Date(aDF$DateTime)
        aDF$DateHour <- as.POSIXct(paste0(aDF$Date, " ", hour(aDF$DateTime), ":00:00"),format = "%Y-%m-%d %H:%M:%S")
        
        ### Calculate monthly sum
        mDF <- aggregate(Rain_mm_Tot~Month, FUN=sum, na.rm=T, data=aDF)
    
    return(mDF)

        
}