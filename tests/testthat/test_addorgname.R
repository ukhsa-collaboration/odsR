context("test_addorgname")

# generate some test data with nulls and invalid OrgIds
data     <- getODS(PostCode = "S8")
testdata <- bind_rows(slice(data,20:49),slice(data,30:59))

testdata$OrgId[testdata$Status == "Inactive"] <- NA
testdata$OrgId[testdata$PostCode == "S1 2PJ"] <- "XXXXXXX"
testdata$NameNA <- testdata$Name
testdata$NameNA[testdata$Status == "Inactive"] <- NA
testdata$NameNA[testdata$PostCode == "S1 2PJ"] <- NA

testdata2 <- testdata[,c(1,3:10,2)] %>%
    rename(OrganisationCode = OrgId)




#test function returns correct results with selection of arguments passed
test_that("addorgname - produces correct output when correct arguments specified",{

    expect_equal(addorgname(testdata)[2:3],
                 testdata[,c(2,10)],
                 check.attributes=FALSE, check.names=FALSE, info="test1 defaults")
    expect_equal(addorgname(testdata,insert=FALSE)[,c(2,11)],
                 testdata[,c(2,10)],
                 check.attributes=FALSE, check.names=FALSE, info="test2 insert FALSE")
    expect_equal(addorgname(testdata2, CodeCol = "OrganisationCode")[,c(10,11)],
                 testdata2[,c(10,9)],
                 check.attributes=FALSE, check.names=FALSE, info="test3 OrgId as end col and specify CodeCol")
    expect_equal(addorgname(testdata2, CodeCol = "OrganisationCode", insert=FALSE)[,c(10,11)],
                 testdata2[,c(10,9)],
                 check.attributes=FALSE, check.names=FALSE, info="test4 OrgId as end col")
    expect_equal(names(addorgname(testdata, NameCol = "UseMyName")[3]),
                 "UseMyName",
                 check.attributes=FALSE, check.names=FALSE, info="test5 NameCol specified")
    })
