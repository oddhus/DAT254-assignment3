install.packages("RonFHIR")

library(RonFHIR)

#client <- fhirClient$new("http://hapi.fhir.org/baseR4")

client <- fhirClient$new("https://r4.test.pyrohealth.net/fhir")

client <- fhirClient$new("https://api.logicahealth.org/DAT254test/open")





bundle <- client$search("Patient",   c("gender=female", "birthdate=gt1900-06-27"))
c("Number of records=", dim(bundle$entry))
bundle <- client$search("Patient", c("name=Peter", "address-postalcode=3999"))
bundle <- client$search("Patient", c("gender=male", "birthdate=gt1900-06-27"))
bundle <- client$search("Patient", c("_count=30"))

bundle <-
  client$search("Observation",     c("code=2708-6", "subject=Patient/30831"))
while (!is.null(bundle)) {
  for (row in 1:nrow(bundle$entry)) {
    res <- bundle$entry[row, "resource"]
    print(paste(res$effectiveDateTime, res$valueQuantity$value,  sep = ","))
  }
  bundle <- client$continue(bundle)
}

getReligion <-
  function(extl) {
    for (io in extl) {
      if (length(io) > 0) {
        
        
        for (i in 1:nrow(io)) {
          if (io[i, "url"] == "http://hl7.org.au/fhir/StructureDefinition/indigenous-status") {
            v <- io[i, "valueCoding"]
            return (v$display)
          }
        }
      }
    } 
    return ("??")
  }

for(row in 1:nrow(bundle$entry)) {
  res <- bundle$entry[row, "resource"]
  print(paste(res$gender, res$birthDate, getReligion(res$extension),  sep = ","))
}

