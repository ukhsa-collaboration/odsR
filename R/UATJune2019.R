## UAT new functionality in test directory til 14th june

library(httr)
library(jsonlite)
library(dplyr)



# set config
set_config(config(ssl_verifypeer = 0L))

#All Roles
url <- "https:/uat.directory.spineservices.nhs.uk/ORD/2-0-0/roles"
#https://uat.directory.spineservices.nhs.uk/ORD/2-0-0/roles?_format=xml

# Get API response
httpResponse <- GET(url, accept_json())
results <- fromJSON(content(httpResponse, as="text", encoding="UTF-8"))
output <- results$Roles





#Single Role
url <- "https://uat.directory.spineservices.nhs.uk/ORD/2-0-0/roles/RO197"
#https://uat.directory.spineservices.nhs.uk/ORD/2-0-0/roles/RO197?_format=xml


# Get API response
httpResponse <- GET(url, accept_json())
results <- fromJSON(content(httpResponse, as="text", encoding="UTF-8"))
output <- results$Roles




#All Rels
url <- "https://uat.directory.spineservices.nhs.uk/ORD/2-0-0/rels"
#https://uat.directory.spineservices.nhs.uk/ORD/2-0-0/rels?_format=xml


# Get API response
httpResponse <- GET(url, accept_json())
results <- fromJSON(content(httpResponse, as="text", encoding="UTF-8"))
output <- results$Relationships




#Single Rel
url <- "https://uat.directory.spineservices.nhs.uk/ORD/2-0-0/rels/RE5"
#https://uat.directory.spineservices.nhs.uk/ORD/2-0-0/rels/RE5?_format=xml


# Get API response
httpResponse <- GET(url, accept_json())
results <- fromJSON(content(httpResponse, as="text", encoding="UTF-8"))
output <- results$Relationships




#All Record Classes
url <- "https://uat.directory.spineservices.nhs.uk/ORD/2-0-0/recordclasses"
#https://uat.directory.spineservices.nhs.uk/ORD/2-0-0/recordclasses?_format=xml


# Get API response
httpResponse <- GET(url, accept_json())
results <- fromJSON(content(httpResponse, as="text", encoding="UTF-8"))
output <- results$RecordClasses




#Single Record Class
url <- "https://uat.directory.spineservices.nhs.uk/ORD/2-0-0/recordclasses/RC2"
#https://uat.directory.spineservices.nhs.uk/ORD/2-0-0/recordclasses/RC2?_format=xml


# Get API response
httpResponse <- GET(url, accept_json())
results <- fromJSON(content(httpResponse, as="text", encoding="UTF-8"))
output <- results$RecordClasses




