context("test_getODS")

# urls 2 and 6 removed after execution as they become invalid over time and cause travis and testthat to fail
# (as can only use a date within the last 6 months)
url1 <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?Name=Woodseats&Limit=1000"
#url2 <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?PostCode=S8&LastChangeDate=2018-03-31&Limit=1000"
url3 <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?Name=Woodseats&Status=Active&Limit=1000"
url4 <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?Status=Active&PrimaryRoleId=RO197&OrgRecordClass=RC1&Limit=1000"
url5 <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?Name=Lancaster&PrimaryRoleId=RO177&NonPrimaryRoleId=RO76&Limit=1000"
#url6 <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?LastChangeDate=2018-06-13&Limit=1000"
url7 <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?NonPrimaryRoleId=RO256&Limit=1000" # STP


#test function returns correct results with selection of arguments passed
test_that("getODS - produces correct output when correct arguments specified",{

    expect_equal(getODS(Name="Woodseats"),
                 fromJSON(content(GET(url1, accept_json()), "text", encoding="UTF-8"))$Organisations,
                 check.attributes=FALSE, check.names=FALSE, info="test1")
#    expect_equal(getODS(PostCode="S8",LastChangeDate="2018-03-31"),
#                 fromJSON(content(GET(url2, accept_json()), "text", encoding="UTF-8"))$Organisations,
#                 check.attributes=FALSE, check.names=FALSE, info="test2")
    expect_equal(getODS(Name="Woodseats", Status="Active"),
                 fromJSON(content(GET(url3, accept_json()), "text", encoding="UTF-8"))$Organisations,
                 check.attributes=FALSE, check.names=FALSE, info="test3")
    expect_equal(getODS(Status="Active", PrimaryRoleId = "RO197", OrgRecordClass="RC1"),
                 fromJSON(content(GET(url4, accept_json()), "text", encoding="UTF-8"))$Organisations,
                 check.attributes=FALSE, check.names=FALSE, info="test4")
    expect_equal(getODS(Name="Lancaster", PrimaryRoleId = "RO177", NonPrimaryRoleId="RO76"),
                 fromJSON(content(GET(url5, accept_json()), "text", encoding="UTF-8"))$Organisations,
                 check.attributes=FALSE, check.names=FALSE, info="test5")
 # Run this test at time of release but then comment out as number of records returned will
 # increase to >1000 over time

 #   expect_equal(getODS(LastChangeDate="2018-06-13"),
 #                fromJSON(content(GET(url6, accept_json()), "text", encoding="UTF-8"))$Organisations,
 #                check.attributes=FALSE, check.names=FALSE, info="test6")
    expect_equal(getODS(NonPrimaryRoleId="RO256"),
                 fromJSON(content(GET(url7, accept_json()), "text", encoding="UTF-8"))$Organisations,
                 check.attributes=FALSE, check.names=FALSE, info="test7")
})


# test error handling
test_that("getODS - errors are generated when invalid arguments are used",{
    expect_error(getODS(),
                 "ERROR: at least one organisational parameter must be specified", info="error nothing specified1")
    expect_error(getODS(LastChangeDate="AAAABBBB"),
                 "ERROR: LastChangeDate is not a valid date", info="error invalid LastChangeDate")
    expect_error(getODS(Status = "Live"),
                 "ERROR: Status is invalid - valid values are All (default), Active, Inactive", info="invalid Status", fixed=TRUE)
    expect_error(getODS(PrimaryRoleId = "XXX"),
                 "ERROR: PrimaryRoleId is invalid - valid values begin with RO followed by a number, or specify All",
                 info="invalid PrimaryRoleid")
    expect_error(getODS(NonPrimaryRoleId = "XXX"),
                 "ERROR: NonPrimaryRoleId is invalid - valid values begin with RO followed by a number, or specify All",
                 info="invalid NonPrimaryRoleId")
    expect_error(getODS(OrgRecordClass = "RC22"),
                 "ERROR: OrgRecordClass is invalid - valid values are All (default), RC1 (Health and Social Care Organisation), RC2 (Health and Social Care Organisation Site)",
                 info="invalid OrgRecordClass", fixed=TRUE)
})

