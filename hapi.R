library(RonFHIR)
library(ggplot2)

client <- fhirClient$new("http://hapi.fhir.org/baseR4")

namePrinter <- function (namelist) {
  for (name in namelist) {
    if (!is.null(name$text)) {
      return (name$text)
    } else {
      return(name$given)
    }
  }
}

#Get respiratory rate
getPatientRespiatoryRate <- function(patientId, client) {
  observations <- client$search(resourceType = "Observation",
                                criteria = c(paste("patient=", patientId, sep = ""), "code=9279-1"))
  
  dateList <- list()
  valueList <- list()
  unitList <- list()
  patientIdList <- list()
  index <- 1
  
  while (!is.null(observations)) {
    if (is.null(observations$entry)) {
      return()
    }
    
    
    for (row in 1:nrow(observations$entry)) {
      res <- observations$entry[row, "resource"]
      dateList[index] = res$effectiveDateTime
      valueList[index] = res$valueQuantity$value
      unitList[index] = res$valueQuantity$unit
      patientIdList[index] = patientId
      index = index + 1
      
    }
    observations <- client$continue(observations)
  }
  
  df <- do.call(
    rbind,
    Map(
      data.frame,
      DATE = dateList,
      RESP = valueList,
      UNIT = unitList,
      PATIENTID = patientIdList
    )
  )
  return (df)
}


#Get respiratory rate for all
getPatientRespiatoryRateAll <- function(client) {
  allPatientsId <- getPatientList(client)
  
  #Create empty dataframe
  df <-
    data.frame(
      DATE = as.Date(character()),
      RESP = numeric(),
      UNIT = character(),
      PATIENTID = factor(),
      stringsAsFactors = FALSE
    )
  
  #Add to dataframe
  for (id in allPatientsId) {
    allPatients <-
      rbind(allPatients, getPatientRespiatoryRate(id, client))
  }
  
  return(df)
}


#Get patients and id
getPatientList <- function(client) {
  bundle <-
    client$search("Patient",   c("gender=female", "birthdate=gt1900-06-27"))
  patients <- c()
  for (row in 1:nrow(bundle$entry)) {
    res <- bundle$entry[row, "resource"]
    patients <- c(patients, res$id)
  }
  return(patients)
}

