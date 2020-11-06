context("test_OrgRelLkp")

#### test lookup as at 2018-08-31

GP_CCG_20180831 <- GP_CCG %>%
    filter(OrgRoleStart <= "2018-08-31" &
               RelStart <= "2018-08-31" &
            (OrgRoleEnd >= "2018-08-31" | is.na(OrgRoleEnd)) &
               (RelEnd >= "2018-08-31" | is.na(RelEnd)))

#### Make comparisons

# records in both but discrepant
# postcodes checked manually with commented out code.
# several typos in EPRACCUR were identified and also
# where branch surgeries exist EPRACCUR uses the postcode of the first branch alphabetically
# whereas the API uses the main site and registered postcode for the group
QA_discreps_20180831 <- testdata %>%
    filter(Effdate == "20180831") %>%
    inner_join(GP_CCG_20180831,by = c("OrganisationCode" = "OrgId")) %>%
    filter(Commissioner != RelOrgId)
#   filter(Commissioner != RelOrgId | Postcode != OrgPostCode)


# records in testdata only
QA_epraccur_only_20180831 <- testdata %>%
    filter(Effdate == "20180831") %>%
    anti_join(GP_CCG_20180831,by = c("OrganisationCode" = "OrgId"))


# records in function output only
QA_GP_CCG_only_20180831 <- GP_CCG_20180831 %>%
    anti_join(filter(testdata,Effdate == "20180831"),
              by = c("OrgId" = "OrganisationCode"))

#### test lookup as at 2015-05-26

GP_CCG_20150526 <- GP_CCG %>%
    filter(OrgRoleStart <= "2015-05-26" &
               RelStart <= "2015-05-26" &
               (OrgRoleEnd >= "2015-05-26" | is.na(OrgRoleEnd)) &
               (RelEnd >= "2015-05-26" | is.na(RelEnd)))


#### Make comparisons - postcode not compared as only most recent kept on ODS API record

# records in both but discrepant
QA_discreps_20150526 <- testdata %>%
    filter(Effdate == "20150526") %>%
    inner_join(GP_CCG_20150526,by = c("OrganisationCode" = "OrgId")) %>%
    filter(Commissioner != RelOrgId)


# records in epraccur only
QA_epraccur_only_20150526 <- testdata %>%
    filter(Effdate == "20150526") %>%
    anti_join(GP_CCG_20150526,by = c("OrganisationCode" = "OrgId"))


# records in GP_CCG only
QA_GP_CCG_only_20150526 <- GP_CCG_20150526 %>%
    anti_join(testdata,by = c("OrgId" = "OrganisationCode"))


#### Perform tests

# exclusions - manual QA has confirmed function output correct and testdata incorrect where
# discrepacnies exist as at 2018-05-26 as follows, according to current API output:
# Y01792 - should be in function output for 2015-05-26, missing from testdata
# H85039 - should be in function output for 2015-05-26, missing from testdata
# Y04694 - shouldn't be output in testdata - does not have NonPrimaryRole of RO76



test_that("function output matches testdata",{
    expect_equal(nrow(QA_discreps_20180831),
                 0,check.attributes=FALSE, check.names=FALSE,info="test discreps 20180831")
    expect_equal(nrow(QA_discreps_20150526),
                 0,check.attributes=FALSE, check.names=FALSE,info="test discreps 20150526")
    expect_equal(nrow(QA_epraccur_only_20180831),
                 0,check.attributes=FALSE, check.names=FALSE,info="test epraccur only 20180831")
    expect_equal(nrow(filter(QA_epraccur_only_20150526, !(OrganisationCode %in% c("Y04694")))),
                 0,check.attributes=FALSE, check.names=FALSE,info="test epraccur only 20150526")
    expect_equal(nrow(QA_GP_CCG_only_20180831),
                 0,check.attributes=FALSE, check.names=FALSE,info="test default")
    expect_equal(nrow(filter(QA_GP_CCG_only_20150526, !(OrgId %in% c("Y01792","H85039")))),
                 0,check.attributes=FALSE, check.names=FALSE,info="test default")

})



