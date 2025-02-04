library(RonFHIR)
library(ggplot2)

client <- fhirClient$new("http://hapi.fhir.org/baseR4")

bundle <- client$search("Patient",   c("gender=female", "birthdate=gt1900-06-27"))

namePrinter <- function (namelist) {
  for (name in namelist){
    if (!is.null(name$text)){
      return (name$text)
    } else {
      return(name$given)
    }
  }
}
#List patients
for(row in 1:nrow(bundle$entry)) {
  res <- bundle$entry[row, "resource"]
  print(paste(res$id, res$gender, res$birthDate,namePrinter(res$name),  sep = ", "))
}


#Get heart rate
getHeartRate <- function(patientId, client){
  observations <- client$search(
    resourceType = "Observation",
    criteria = c(paste("patient=", patientId, sep = ""), "code=8867-4")
  )
  
  dateList <- list()
  valueList <- list()
  unitList <- list()
  patientIdList <- list()
  index <- 1
  
  while (!is.null(observations)) {
    if(is.null(observations$entry)){
      return()
    }
    
    
    for (row in 1:nrow(observations$entry)) {
      res <-observations$entry[row, "resource"]
      dateList[index] = res$effectiveDateTime
      valueList[index] = res$valueQuantity$value
      unitList[index] = res$valueQuantity$unit
      patientIdList[index] = patientId
      index = index + 1
      
    }  
    observations <- client$continue(observations)
  }
  
  df <- do.call(rbind, 
                Map(data.frame,
                    DATE=dateList,
                    RESP=valueList,
                    UNIT=unitList,
                    PATIENTID=patientIdList))
  return (df) 
}
#Save heart rate in df
patientResp <- getHeartRate("1584778", client)

#Display in plot
ggplot(data = patientResp, aes(x = as.Date(DATE), y = RESP)) + 
  geom_point() + 
  geom_line() +
  labs(x = "Date", y = "Beats per minute", title = "Heart rate")



#########################Print for all patients

#Create empty dataframe
allPatients <- 
  data.frame(DATE=as.Date(character()),
             RESP=numeric(),
             UNIT=character(),
             PATIENTID= factor(),
             stringsAsFactors=FALSE) 

#Add to dataframe
for(row in 1:nrow(bundle$entry)) {
  res <- bundle$entry[row, "resource"]
  allPatients <- rbind(allPatients, getHeartRate(res$id, client))
}

#Print plot
ggplot(data = allPatients, aes(x = as.Date(DATE), y = RESP, color=PATIENTID)) + 
  geom_point() + 
  geom_line() +
  scale_x_date(
    #limits = c(Sys.Date() - 200, NA),
    date_labels = "%d-%m-%Y")+
  labs(x = "Date", y = "Beats per minute", title = "Heart rate")

#Get patients and id
getPatientList <- function(client){
  bundle <- client$search("Patient",   c("gender=female", "birthdate=gt1900-06-27"))
  patients <- c()
  for(row in 1:nrow(bundle$entry)) {
    res <- bundle$entry[row, "resource"]
    patients <- c(patients, res$id)
  }
  return(patients)
}
