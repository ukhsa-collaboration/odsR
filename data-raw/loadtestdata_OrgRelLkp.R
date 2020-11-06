# load the EPRACCUR testdata and save the output of the OrgRelLkp function (so data fixed for testing)


######### EPRACCUR (GP Practices) ###############

# get testfiles
testfiles <- list.files(path = "./tests/testthat/", pattern="^epraccur", full.names=TRUE, ignore.case=TRUE)

#### load EPRACCUR data
colnames <- c("OrganisationCode", "Name", "NationalGrouping", "HighLevelhealthGeography",
              "AddressLine1", "AddressLine2", "AddressLine3", "AddressLine4",
              "AddressLine5", "Postcode", "OpenDate", "CloseDate", "StatusCode",
              "OrganisationSubTypeCode", "Commissioner", "JoinProviderPurchaserDate",
              "LeftProviderPurchaserDate", "ContactTelephoneNumber", "Null1", "Null2",
              "Null3", "AmendedRecordIndicator", "Null4", "ProviderPurchaser", "Null5",
              "PrescribingSetting", "Null6")

testdata <- do.call("rbind", sapply(testfiles, read.csv, stringsAsFactors=FALSE,
                                    header=FALSE, col.names=colnames, simplify = FALSE)) %>%
    mutate(Effdate = sub(".csv.*","",
                     sub("./tests/testthat/epraccur_","",row.names(.), ignore.case=TRUE))) %>%
    filter(StatusCode != "C" & PrescribingSetting == 4) %>%
    select(OrganisationCode, Name, Postcode, OpenDate, CloseDate, StatusCode, Commissioner,
           JoinProviderPurchaserDate, LeftProviderPurchaserDate,
           ProviderPurchaser, PrescribingSetting, Effdate)

# reformat date fields
testdata$OpenDate   <- as.Date(as.character(testdata$OpenDate),format="%Y%m%d")
testdata$CloseDate  <- as.Date(as.character(testdata$CloseDate),format="%Y%m%d")


#### Create function output for saving
GP_CCG <- OrgRelLkpGP("RO177","RO76","RE4","RO98","2013-04-01")




######### ETR (NHS Trusts) ###############

# get testfiles
testfiles2 <- list.files(path = "./tests/testthat/", pattern="^etr_", full.names=TRUE, ignore.case=TRUE)

#### load EPRACCUR data
colnames2 <- c("OrganisationCode", "Name", "NationalGrouping", "HighLevelHealthGeography",
              "AddressLine1", "AddressLine2", "AddressLine3", "AddressLine4",
              "AddressLine5", "Postcode", "OpenDate", "CloseDate", "Null1",
              "Null2", "Null3", "Null4",
              "Null5", "ContactTelephoneNumber", "Null6", "Null7",
              "Null8", "AmendedRecordIndicator", "Null9", "GORCode", "Null10",
              "Null11", "Null12")

testdata2 <- do.call("rbind", sapply(testfiles2, read.csv, stringsAsFactors=FALSE,
                                    header=FALSE, col.names=colnames2, simplify = FALSE)) %>%
    mutate(Effdate = sub(".csv.*","",
                         sub("./tests/testthat/etr_","",row.names(.), ignore.case=TRUE))) %>%
 #   filter(StatusCode != "C" & PrescribingSetting == 4) %>%
    select(OrganisationCode, Name, NationalGrouping, HighLevelHealthGeography,
           Postcode, OpenDate, CloseDate, GORCode, Effdate)

# reformat date fields
testdata2$OpenDate   <- as.Date(as.character(testdata2$OpenDate),format="%Y%m%d")
testdata2$CloseDate  <- as.Date(as.character(testdata2$CloseDate),format="%Y%m%d")


#### Create function output for saving
NHSTrust_NHSRLO <- OrgRelLkpNHSTrust("RO197","All","RE5","RO210","L","2013-04-01")
NHSTrust_NoRels <- OrgRelLkpNHSTrust("RO197","All","None","None","L","2013-04-01")



########## LOAD TEST DATA IN PACKAGE #################


# use this devtools code to save testdata to package (hidden from users)
usethis::use_data(GP_CCG,testdata,NHSTrust_NHSRLO,testdata2,NHSTrust_NoRels,
                     internal = TRUE, overwrite = TRUE)
