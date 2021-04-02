# AMIA workshop FHIR/graphQL/R med Grahame Grieve:
install.packages("RonFHIR")
install.packages("ggplot2")

library(RonFHIR)
library(ggplot2)

getBDateGraphData <- function(bundle, lowerBoundNBDate, upperBoundNBDate) {
  lbd = as.POSIXct(lowerBoundNBDate,tz=Sys.timezone())
  ubd = as.POSIXct(upperBoundNBDate,tz=Sys.timezone())
  
  dates <- list()
  while(!is.null(bundle)) {
    for(row in 1:nrow(bundle$entry)) {
      res <-bundle$entry[row, "resource"]
      if (!is.na(res$birthDate)) {
        dt <- as.POSIXct(res$birthDate,tz=Sys.timezone())
        year <- format(dt, format="%Y")
        
        if (dt >= lbd && dt <= ubd) {
          if (is.null(dates[[year]])) {
            dates[[year]] <- 1
          } else {
            dates[[year]] <- dates[[year]] + 1
          } 
        }
      }
    }
    bundle <- client$continue(bundle)
  }
  
  return (dates)
}



client <- fhirClient$new("https://server.fire.ly/")
bundle <- client$search("Patient", c("_count=30"))

c("Number of matching patients=", bundle$total)
c("Number of records=", dim(bundle$entry))

lowerBoundNBDate <- "1960-01-01"
upperBoundNBDate <- "1980-01-01"

dates <- getBDateGraphData(bundle, lowerBoundNBDate, upperBoundNBDate)

df <- do.call(rbind,Map(data.frame, DATE=names(dates), COUNT=unlist(dates,use.names=F)))
ggplot(data = df, aes(x = DATE, y = COUNT)) + geom_point() + geom_line() + labs(x = "Date", y = "Count", title = "Numbers of patients borned between 1960 and 1980")

