context("test_OrgRelLkpNHSTrust")

#### test lookup as at 2018-08-31

NHSTrust_20180831 <- NHSTrust_NHSRLO %>%
    filter(OrgRoleStart <= "2018-08-31" &
               (RelStart <= "2018-08-31" | is.na(RelStart)) &
            (OrgRoleEnd >= "2018-08-31" | is.na(OrgRoleEnd)) &
               (RelEnd >= "2018-08-31" | is.na(RelEnd)))

# Make Corrections to testdata2
# QA shows that the CSV file from 31-08-2018 contains 7 records for Trusts
# which closed legally before the snapshot date, but because the closure dates
# were added to the ODS data after the snapshot date, these trusts were incorrectly
# included in the static etr.CSV file published for Aug 2018.
# This script removes these trust records from the testdata2 file
to_correct <- c("R1E","RGQ","RJF", "RM2","RR1","RW3","RY1")

testdata_corrected <- testdata2 %>%
    filter(!OrganisationCode %in% to_correct)



#### Make comparisons

# records in both but discrepant
QA_discreps_20180831 <- testdata_corrected %>%
    filter(Effdate == "20180831") %>%
    inner_join(NHSTrust_20180831,by = c("OrganisationCode" = "OrgId")) %>%
    filter(HighLevelHealthGeography != RelOrgId)
#   filter(HighLevelHealthGeography != RelOrgId | Postcode != OrgPostCode)


# records in testdata only
QA_etr_only_20180831 <- testdata_corrected %>%
    filter(Effdate == "20180831") %>%
    anti_join(NHSTrust_20180831,by = c("OrganisationCode" = "OrgId"))


# records in function output only
QA_NHSTrust_only_20180831 <- NHSTrust_20180831 %>%
    anti_join(filter(testdata_corrected,Effdate == "20180831"),
              by = c("OrgId" = "OrganisationCode"))



#### Perform tests

test_that("function output matches testdata2",{
    expect_equal(nrow(QA_discreps_20180831),
                 0,check.attributes=FALSE, check.names=FALSE,info="test discreps 20180831")
    expect_equal(nrow(QA_etr_only_20180831),
                 0,check.attributes=FALSE, check.names=FALSE,info="test epraccur only 20180831")
    expect_equal(nrow(QA_NHSTrust_only_20180831),
                 0,check.attributes=FALSE, check.names=FALSE,info="test default")

})
