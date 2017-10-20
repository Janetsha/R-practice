setwd("C:/Users/FLI_B_120/Desktop/ABCvalue_CIF")


if ( (Sys.time() < paste(Sys.Date(), "15:00:00" )) & ( Sys.time() > paste(Sys.Date(), "09:00:00" ) ) ) {
  
  print("please wait unitl 15:00:00")
  
} else {
  
  
  library(xts)
  library(lubridate)
  
  setwd("C:/Users/FLI_B_120/Desktop/ABCvalue_CIF")
  
  Allmindata<- read.table("Allmindata.txt",header = T, sep = ",")
  Allmindata$Time<- as.POSIXct(Allmindata$Time)
  Allmindata_Time<- as.character(unique(as.Date(Allmindata$Time))) 
  
  setwd("C:/Users/FLI_B_120/Desktop/ABCvalue_CIF")
  source("wd.R", encoding = "UTF-8")
  
  Allmindata_tmp<- NULL
  
  
  for (z in wd) {
    
    setwd(z)
    foldernames <- list.files()
    foldernames<- subset(foldernames, as.Date(foldernames,format = "%Y%m%d")> "2017-08-29")
    foldernames <- subset(foldernames,as.Date(foldernames,format = "%Y%m%d") > max(Allmindata_Time))
    
    for (a in foldernames) {
      
      folderpath<- paste0(z,"/", a,"/" )
      
    }
    
    
    for (b in folderpath) {
      
      data_list <- lapply(foldernames,function(b)read.table(file.path(b,"quote.log"), header = F, sep=","))
      data <- do.call(rbind, data_list)
    
    }
    
    
    data_Time<- as.POSIXct(paste(data$V1,data$V2,sep = ""),format = "%Y/%m/%d %H:%M:%S")
    rowdata<- as.xts(as.numeric(data[,3]),data_Time)
    
    #change time format#
    minData<- NULL
    
    
    tmp_data<- to.period(rowdata, period = "minutes", k =1, OHLC = F)
    index(tmp_data)<- floor_date(index(tmp_data),unit = 'minutes')
    index(tmp_data)<- as.POSIXct(index(tmp_data))
    tmp_data1<- tmp_data['T09:30/T11:30']
    tmp_data2<- tmp_data['T13:00/T15:00']
    tmp_data<-rbind(tmp_data1,tmp_data2)
    
    dataID<- as.character(unique(as.Date(index(tmp_data)))) 
    framesnumber<- length(dataID)
    startTime_m<- as.POSIXct(paste(dataID, "09:30",sep = " "))  
    endTime_m<- as.POSIXct(paste(dataID, "11:30", sep = " "))
    startTime_a<- as.POSIXct(paste(dataID, "13:00",sep = " "))  
    endTime_a<- as.POSIXct(paste(dataID, "15:00", sep = " "))
    
    
    fullTimeXts <- NULL
    
    for (c in 1:framesnumber) {
      fullTimeID_1<- seq(from = startTime_m[c], to = endTime_m[c], by = "mins")
      fullTimeID_2<- seq(from = startTime_a[c], to = endTime_a[c], by = "mins")
      fullTimeNum_1<- 1:length(fullTimeID_1)
      fullTimeNum_2<- 1:length(fullTimeID_2)
      fullTimeXts_1<- as.xts(fullTimeNum_1,fullTimeID_1)
      fullTimeXts_2<- as.xts(fullTimeNum_2,fullTimeID_2)
      
      tmp_fullTimeXts <- rbind(fullTimeXts_1, fullTimeXts_2)
      fullTimeXts <- rbind(fullTimeXts, tmp_fullTimeXts)
    }
    
    fullTimedata<- cbind(tmp_data, fullTimeXts)
    fullTimedata1<- fullTimedata[ ,1]
    fullTimedata2<- na.locf(fullTimedata1)
    fullTimedata3<- na.locf(fullTimedata2, fromLast = T)
    minData<- rbind(minData, fullTimedata3)
    
    Allmindata_tmp<- cbind(Allmindata_tmp,minData)
    Allmindata_tmp<- na.omit(Allmindata_tmp)
  }
  
  
  
  Allmindata_tmp <- data.frame(date=index(Allmindata_tmp), coredata(Allmindata_tmp))
  colnames(Allmindata_tmp) <- c("Time", "CIF01", "CIF00", "A5001", "A5000","CNY")
  
  Allmindata <- rbind(Allmindata , Allmindata_tmp ,  make.row.names = F )
  
  setwd("C:/Users/FLI_B_120/Desktop/ABCvalue_CIF")
  write.table(Allmindata, file = "Allmindata.txt", sep = ",",col.names = T, row.names = F)
  
  
  
}
