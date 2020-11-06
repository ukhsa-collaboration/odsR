#' addorgname
#'
#' Adds Organisation Names to a data.frame containing Organisation Codes using the NHS Digitial ODS ORD API.
#'
#' @param data    data.frame containing the Organisation Codes to add Organisation Names to, unquoted string, no default
#' @param CodeCol the column name from data containing the organisation codes, quoted string, default "OrgId"
#' @param NameCol the name of the column to be added holding organisation names, quoted string, default "OrgName"
#' @param insert  whether the new OrgName column should be positioned immediately after the CodeCol column (TRUE
#'                or appended to the end of the data frame (FALSE), logical, default TRUE
#'
#' @return returns the original data.frame with Organisation Name column added
#'
#' @section Notes: View the NHS Digital ODS API Implementation Guide at
#'          \url{https://developer.nhs.uk/library/identifiers/ods-ord-api-implementation-guide/} \cr \cr
#'          View the NHS Digital ODS API Suite at \url{https://directory.spineservices.nhs.uk/ODSAPISuite}
#'
#' @examples
#' # add OrgNm column to a data.frame containing a single column (OrgId) of Organisation Codes
#' codelist <- getODS(Status="Active",PostCode="S80B")[2]
#' addorgname(codelist)
#' addorgname(codelist, NameCol="MyNewColumnName", insert=FALSE)
#'
#' @import dplyr
#' @import jsonlite
#' @import httr
#'
#' @export
#'
#' @family odsR package functions
# -------------------------------------------------------------------------------------------------

addorgname <- function (data, CodeCol = "OrgId", NameCol = "OrgName", insert = TRUE) {

    # create list of unique codes
    codes <- data[CodeCol] %>%
        unique()
    names(codes) <- "codes"

    names <- data.frame()

    # loop through codes to populate code - name lookup table
    for (i in 1:nrow(codes)) {

        if (is.na(slice(codes,i))) {
            thisname <- NA
        } else {
            thiscode <- slice(codes,i)
            thisname <- getODSfull(thiscode)$Organisation$Name
            if (is.null(thisname)) {
                thisname <- NA
            }
        }
    names(thisname) <- "names"
    names <- bind_rows(names,thisname)
    }
    lkp <- bind_cols(codes, names)


    # vector showing which df rows to join to which lkp rows
    matches <- match(data[[CodeCol]],lkp$codes)

    # gather column names before new column is added
    orignames <- names(data)

    # add new column of organisation names
    data$AddedCol <- lkp[matches,2]

    # rename the new column
    names(data) <- c(orignames,NameCol)

    # if specified, reorder columns so new Name column appears after specified code column
    # this code doesn't work if few columns or if code column is close to end of data.frame
    if (insert == TRUE) {
        codecol  <- which(match(names(data),CodeCol) == T)
        if (!(codecol+1 == ncol(data))) {
            neworder <- c(1:codecol,ncol(data),(codecol+1):(ncol(data)-1))
            data <- data[,neworder]
        }
    }

    return(data)
}
