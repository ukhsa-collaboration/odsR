#' getODS
#'
#' Extracts summary ODS data for multiple organisations from the NHS Digital ODS ORD API into a data frame.
#'
#' @param Name     Search organisations based on name. Organisations that contain the argument string in their name are returned.;
#'                 quoted string; default "All" applies no filter
#' @param PostCode Search organisations based on postcode. Organisations that contain the argument string in their postcode are returned.;
#'                 quoted string; default "All" applies no filter
#' @param LastChangeDate Search for organisations based on their last changed date. Date must be in format "YYYY-MM-DD".
#'                       The search is greater than or equal to. Dates are restricted to 185 days from present.;
#'                 quoted string; default "All" applies no filter
#' @param Status   Search for organisations based on their active status. Arguments can be "Active" or "Inactive".;
#'                 quoted string; default "All" applies no filter
#' @param PrimaryRoleId Search for organisations based on their primary role codes.;
#'                 quoted string; default "All" applies no filter
#' @param NonPrimaryRoleId Search for organisations based on their non primary role codes.;
#'                 quoted string; default "All" applies no filter
#' @param OrgRecordClass Search for organisations based on their record class. Arguments can be "RC1" or "RC2".;
#'                 quoted string; default "All" applies no filter
#'
#' @return returns a data.frame containing the following details for the organisations that meet the filter specifications:
#'         Name, Organisation ID, Status, Organisation Record Class, Postcode, Last Change Date, Primary Role ID,
#'         Non primary Role ID, Primary Role Description, Organisation Link (API endpoint URL for full organisation record)
#'
#' @section Notes: View the NHS Digital ODS API Implementation Guide at
#'          \url{https://developer.nhs.uk/library/identifiers/ods-ord-api-implementation-guide/} \cr \cr
#'          View the NHS Digital ODS API Suite at \url{https://directory.spineservices.nhs.uk/ODSAPISuite}
#'
#' @examples
#'
#' # return summary organisation data for all organisations with 'Woodseats' in their name
#' getODS(Name="Woodseats")
#'
#' # return summary organisation data for all organisations
#' # with 'Woodseats Medical Centre' in their name - two options to handle spaces:
#' getODS(Name="Woodseats_Medical_Centre")
#' getODS(Name="Woodseats Medical Centre")
#'
#' # return summary organisation data for all currently active GP practices:
#' # commented out as takes too long to run with package build
#' # getODS(Status="Active", PrimaryRoleId = "RO177", NonPrimaryRoleId = "RO76")
#'
#' @import dplyr
#' @import jsonlite
#' @import httr
#' @importFrom utils URLencode
#'
#' @export
#'
#' @family odsR package functions
# -------------------------------------------------------------------------------------------------

# create function to allow user to specify parameters to input to ODS API call
getODS <- function(Name              = "All",
                    PostCode         = "All",
                    LastChangeDate   = "All",
                    Status           = "All",
                    PrimaryRoleId    = "All",
                    NonPrimaryRoleId = "All",
                    OrgRecordClass   = "All") {

 # error checks
    if (Name           == "All" & PostCode         == "All" &
        LastChangeDate == "All" & Status           == "All" &
        PrimaryRoleId  == "All" & NonPrimaryRoleId == "All" &
        OrgRecordClass == "All") {
          stop("ERROR: at least one organisational parameter must be specified")
    } else if (LastChangeDate != "All" &
                 is.na(as.Date(LastChangeDate,"%Y-%m-%d"))) {
          stop("ERROR: LastChangeDate is not a valid date")
    } else if (!(tolower(Status) %in% c("all", "active","inactive"))) {
          stop("ERROR: Status is invalid - valid values are All (default), Active, Inactive")
    } else if (!(PrimaryRoleId == "All" |
                 tolower(substr(PrimaryRoleId,1,2)) == "ro")) {
          stop("ERROR: PrimaryRoleId is invalid - valid values begin with RO followed by a number, or specify All")
    } else if (!(NonPrimaryRoleId == "All" |
                 tolower(substr(NonPrimaryRoleId,1,2)) == "ro")) {
          stop("ERROR: NonPrimaryRoleId is invalid - valid values begin with RO followed by a number, or specify All")
    } else if (!(tolower(OrgRecordClass) %in% c("all", "rc1","rc2"))) {
          stop("ERROR: OrgRecordClass is invalid - valid values are All (default), RC1 (Health and Social Care Organisation), RC2 (Health and Social Care Organisation Site)")
    }

# define organisation search endpoint URL
    url <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?"

# complete URL using specified parameters
    if (!Name=="All") {
      url <- paste0(url,"&Name=", Name)
    }

    if (!PostCode=="All") {
      url <- paste0(url,"&PostCode=", PostCode)
    }

    if (!LastChangeDate=="All") {
      url <- paste0(url,"&LastChangeDate=", LastChangeDate)
    }

    if (!Status=="All") {
      url <- paste0(url,"&Status=", Status)
    }

    if (!PrimaryRoleId=="All") {
      url <- paste0(url,"&PrimaryRoleId=", PrimaryRoleId)
    }

    if (!NonPrimaryRoleId=="All") {
        url <- paste0(url,"&NonPrimaryRoleId=", NonPrimaryRoleId)
    }

    if (!OrgRecordClass=="All") {
      url <- paste0(url,"&OrgRecordClass=", OrgRecordClass)
    }

    # append offset, limit and format to URL
    url <- utils::URLencode(paste0(url,"&_format=application/json&Limit=1000"))

  # set config
  set_config(config(ssl_verifypeer = 0L))

  # Get API response
  httpResponse <- GET(url, accept_json())
  #httpResponse <- GET(url,use_proxy(ie_get_proxy_for_url(.), username = "", password = "", auth = "ntlm"))

  # identify pages returned - 1000 record per page (floor as will loop from 0)
  npages <- floor(as.double(httpResponse$headers$`x-total-count`)/1000)

  # Loop through responses to retrieve all data

  pages <- data.frame()

  for (i in 0:npages) {
        if (i == 0) {
            urlpages  <- url
        } else
            urlpages  <- paste0(url,"&Offset=",i*1000,sep="")
        httpResponse1 <- GET(urlpages, accept_json())
        results <- fromJSON(content(httpResponse1, as="text", encoding="UTF-8"))
        pages <- bind_rows(pages,results$Organisations)
  }

  return(pages)
}


