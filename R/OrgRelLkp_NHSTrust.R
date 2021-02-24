#' OrgRelLkp
#'
#' Generates an organisational lookup table from the NHS Digital ODS ORD API
#' based on organisational relationship data.
#'
#' @param PrimaryRole     The Primary Role code for organisations to be included in the lookup;
#'                        quoted string, no default
#' @param NonPrimaryRole  The Non Primary Role code for organisations to be included in the lookup,
#'                        or 'All' to include all Non Primary Role Codes;
#'                        quoted string, default = "All"
#' @param RelTypes The Named Relationship Types for related organisations to be included in the lookup,
#'                 or 'None' to exclude Relationships from output;
#'                 character vector, default = "None"
#' @param RelPrimaryRoles The Primary Role Ids for related organisations to be included in the lookup,
#'                        or 'None' to exclude relationships from output;
#'                        character vector, default = "None"
#' @param FromDate The effective date from which to include Organisations operational/legal on or after;
#'                 character string in the format "yyyy-mm-dd", no default
#' @inheritParams getODS
#'
#' @return returns a data.frame of Organisation codes, names, start dates and end dates plus the start and end dates
#' that the organisation was in the specified role and related organisation codes & names and associated start and end dates
#' for the relationship.
#'
#'
#' @section Notes: This function is experimental and has only been thoroughly QAd for use with the arguments
#'          presented in the example code in order to generate NHS Trust to NHS Regional local Office Lookup Tables. \cr \cr
#'          View the NHS Digital ODS API Implementation Guide at
#'          \url{https://developer.nhs.uk/library/identifiers/ods-ord-api-implementation-guide/} \cr \cr
#'          View the NHS Digital ODS API Suite at \url{https://directory.spineservices.nhs.uk/ODSAPISuite}
#'
#'
#' @examples
#'
#' # return basic NHS Trust lookup without relationships to other organisation types
#' \dontrun{
#' #' OrgRelLkpNHSTrust(PrimaryRole = "RO197",FromDate="2013-04-01")
#' }
#'
#' # return NHS Trust to NHS Regional Local Office Lookup to include all organisations effective on or after 01-04-2013
#' \dontrun{
#' OrgRelLkpNHSTrust(PrimaryRole     = "RO197",
#'                   NonPrimaryRole  = "All",
#'                   RelTypes        = "RE5",
#'                   RelPrimaryRoles = "RO210",
#'                   FromDate        = "2013-04-01")
#' }
#'
#'
#' @import dplyr
#' @import jsonlite
#' @import httr
#' @importFrom tidyr separate
#' @importFrom purrr set_names
#'
#' @export
#'
#' @family odsR package functions
# -------------------------------------------------------------------------------------------------

# create function to generate Organisation lookup data.frame
OrgRelLkpNHSTrust <- function(PrimaryRole, 
                              NonPrimaryRole = "All", 
                              RelTypes = "None",
                              RelPrimaryRoles = "None", 
                              OpLegal, 
                              FromDate,
                              UseProxy = FALSE) {

    # display experimental message
    message("Please Note that this function is experimental and has not been thoroughly QAd
            for every possible set of arguments. It has been written to enable the generation of NHS Trust
             to NHS Regional Local Office tables with the following arguments:
            PrimaryRole RO197, NonPrimaryRole All, Reltypes RE5, RelPrimaryRoles RO210, FromDate 2013-04-01")

    # error handling for valid arguments
    if (!OpLegal %in% c("O","L")) {
        stop ("Invalid value for OpLegal argument - please enter O or L to prioritise operational or
              legal dates respectively.")
    }

    # retrieve all organisations to include records for
    if(NonPrimaryRole == "All") {
        allorgs <- getODS(PrimaryRoleId = PrimaryRole,
                          UseProxy      = UseProxy) %>%
            unique()
    } else {
        allorgs <- getODS(PrimaryRoleId    = PrimaryRole,
                          NonPrimaryRoleId = NonPrimaryRole,
                          UseProxy         = UseProxy) %>%
            unique()
    }

    # Create empty Lookup dataframe
    lkup <- data.frame(OrgId       = character(),
                       OrgName     = character(),
                       OrgPostCode = character(),
                       OrgStart    = as.Date(character()),
                       OrgEnd      = as.Date(character()),
                       OrgRoleStart = as.Date(character()),
                       OrgRoleEnd  = as.Date(character()),
                       RelOrgId    = character(),
                       RelType     = character(),
                       RelOrgPrimaryRole = character(),
                       RelStart    = as.Date(character()),
                       RelEnd      = as.Date(character()),
                       stringsAsFactors=FALSE)

    if (RelTypes == "None" & RelPrimaryRoles == "None") {
        lkup <- select(lkup,-starts_with("Rel"))

    }

    # loop through each Organisation record

    for (i in (1:nrow(allorgs))) {

        addOrg <- NA
        getOrg <- getODSfull(allorgs[i,2], UseProxy = UseProxy)


        # get Organisation Start and End dates - keep single record based on Datepriority
        OrgDates   <- dplyr::bind_rows(getOrg$Organisation$Date)

        if(OpLegal == "O") {
            OrgDates <- OrgDates %>%
                arrange(desc(Type)) %>%
                slice(1)
        } else {
            OrgDates <- OrgDates %>%
                arrange(Type) %>%
                slice(1)
        }


        # add end column if missing
        if (!("End" %in% colnames(OrgDates))) {
            OrgDates$End <- NA
        }


        # find primary and non primary roles dates
        role_names <- paste(getOrg$Organisation$Roles$Role$id,
                            getOrg$Organisation$Roles$Role$Date,
                            getOrg$Organisation$Roles$Role$primaryRole,
                            sep = ",,")

        Roles <- getOrg$Organisation$Roles$Role$Date %>%
            purrr::set_names(role_names) %>%
            dplyr::bind_rows(.id = "id" ) %>%
            tidyr::separate(id, c("extension", "typeid", "id"), ",,", convert=TRUE) %>%
            group_by(extension, Type)

        # add end column if missing
        if (!("End" %in% colnames(Roles))) {
            Roles$End <- NA
        }

        Roles <- Roles %>%
            mutate(Start = min(Start),
                   End   = max(End)) %>%
            purrr::when(OpLegal == "O" ~ arrange(.,desc(Type)), OpLegal == "L" ~ arrange(.,Type)) %>%
            group_by(extension) %>%
            slice(1) %>%
            ungroup() %>%
            rename(Role = extension,
                   PrimRole = id) %>%
            select (-typeid)


        # fix Type for join as no longer required
        Roles$Type <- "dummy"

        if (!("End" %in% colnames(Roles))) {
            Roles$End <- NA
        }

        RolesPrimary    <- filter(Roles,PrimRole == TRUE & Role == PrimaryRole)
        RolesNonPrimary <- filter(Roles,is.na(PrimRole) & Role == NonPrimaryRole) %>%
            rename(RoleNP  = Role,
                   Type    = Type,
                   StartNP = Start,
                   EndNP   = End)


        if(NonPrimaryRole == "All") {
            # Find periods org was in required role
            RolePeriods <- RolesPrimary %>%
                transmute(RoleStart = pmax(Start),
                          RoleEnd = pmin(End, na.rm=TRUE)) #%>%
            #        select(RoleStart,RoleEnd) - was mutate instead of transmute

        } else {
            # Find periods org was in required role
            RolePeriods <- inner_join(RolesPrimary, RolesNonPrimary, by="Type") %>%
                transmute(RoleStart = pmax(Start,StartNP),
                          RoleEnd = pmin(End, EndNP, na.rm=TRUE)) #%>%
        }

        # keep only organisations in operation with required role after specified FromDate
        if(all(is.na(RolePeriods$RoleEnd))) {
            addOrg <- 1
        } else if (any(RolePeriods$RoleEnd >= FromDate)) {
            addOrg <- 1
            RolePeriods <- RolePeriods %>%
                filter(is.na(RolePeriods$RoleEnd) | RolePeriods$RoleEnd >= FromDate)
        } else {
            addOrg <- 0
        }



        # continue if Organisation record needs to be included in output
        if (addOrg == 1) {

            # find related organisation dates

            # if relationships not required or if no relationships exist populate rel columns with NA
            if (!(is.list(getOrg$Organisation$Rels)) | (RelTypes == "None" & RelPrimaryRoles == "None")) {

                # add row for each roleperiod
                for (k in 1:nrow(RolePeriods)) {
                    addrow <- data.frame(OrgId   = getOrg$Organisation$OrgId$extension,
                                         OrgName      = getOrg$Organisation$Name,
                                         OrgPostCode  = getOrg$Organisation$GeoLoc$Location$PostCode,
                                         OrgStart     = as.Date(OrgDates$Start, origin = "1900-01-01"),
                                         OrgEnd       = as.Date(OrgDates$End, origin = "1900-01-01"),
                                         OrgRoleStart = as.Date(RolePeriods$RoleStart[k], origin = "1900-01-01"),
                                         OrgRoleEnd   = as.Date(RolePeriods$RoleEnd[k], origin = "1900-01-01"),
                                         RelOrgId     = NA,
                                         RelType      = NA,
                                         RelOrgPrimaryRole = NA,
                                         RelStart     = NA,
                                         RelEnd       = NA, stringsAsFactors=FALSE)

                    if (RelTypes == "None" & RelPrimaryRoles == "None") {
                        addrow <- select(addrow,-starts_with("Rel"))

                    }

                    lkup <- bind_rows(lkup,addrow)
                }

                # otherwise find relationships
            } else {


                # find relationship dates and append RelTypes, Roles and Ids
                dummy_names <- paste(getOrg$Organisation$Rels$Rel$Target$OrgId$extension,
                                     getOrg$Organisation$Rels$Rel$id,
                                     getOrg$Organisation$Rels$Rel$Target$PrimaryRoleId$id,
                                     sep = ",,")

                Rels <- getOrg$Organisation$Rels$Rel$Date %>%
                    purrr::set_names(dummy_names) %>%
                    dplyr::bind_rows(.id = "id" ) %>%
                    tidyr::separate(id, c("extension", "typeid", "id"), ",,") %>%
                    group_by(extension, typeid, id) %>%
                    purrr::when(OpLegal == "O" ~ arrange(.,desc(Type),desc(Start)),
                                OpLegal == "L" ~ arrange(.,Type,desc(Start))) %>%
                    slice(1) %>%
                    ungroup() %>%
                    rename(RelOrgId = extension,
                           RelType = typeid,
                           RelPrimaryRole = id)

                # add end column if missing
                if (!("End" %in% colnames(Rels))) {
                    Rels$End <- NA
                }


                # only keep Rels that existed during Role Period
                if(all(is.na(RolePeriods$RoleEnd))) {
                    Rels <- Rels %>%
                        filter(RelPrimaryRole %in% RelPrimaryRoles &
                                   RelType        %in% RelTypes &
                                   (End >= min(RolePeriods$RoleStart) | is.na(End)))
                } else {
                    Rels <- Rels %>%
                        filter(RelPrimaryRole %in% RelPrimaryRoles &
                                   RelType        %in% RelTypes &
                                   (Start  <= max(RolePeriods$RoleEnd, na.rm=TRUE) | all(is.na(RolePeriods$RoleEnd))) &
                                   (End >= min(RolePeriods$RoleStart) | is.na(End)))
                }

                # if relationships exist but not of correct type & period populate parent and rel columns with NA
                if (nrow(Rels) == 0) {
                    # add row for each roleperiod
                    for (k in 1:nrow(RolePeriods)) {

                        addrow <- data.frame(OrgId    = getOrg$Organisation$OrgId$extension,
                                             OrgName      = getOrg$Organisation$Name,
                                             OrgPostCode  = getOrg$Organisation$GeoLoc$Location$PostCode,
                                             OrgStart     = as.Date(OrgDates$Start, origin = "1900-01-01"),
                                             OrgEnd       = as.Date(OrgDates$End, origin = "1900-01-01"),
                                             OrgRoleStart = as.Date(RolePeriods$RoleStart[k],origin = "1900-01-01"),
                                             OrgRoleEnd   = as.Date(RolePeriods$RoleEnd[k],origin = "1900-01-01"),
                                             RelOrgId     = NA,
                                             RelType      = NA,
                                             RelOrgPrimaryRole = NA,
                                             RelStart     = NA,
                                             RelEnd       = NA, stringsAsFactors=FALSE)

                        lkup <- bind_rows(lkup,addrow)
                    }

                } else {

                    # if Rels with correct type and roles exist - loop through each row to add related codes
                    for (j in 1:nrow(Rels)) {

                        # build lookups record
                        if (!("End" %in% colnames(Rels))) {
                            Rels$End[j] <- NA
                        }

                        # add row for each roleperiod
                        for (k in 1:nrow(RolePeriods)) {

                            addrow <- data.frame(OrgId    = getOrg$Organisation$OrgId$extension,
                                                 OrgName      = getOrg$Organisation$Name,
                                                 OrgPostCode  = getOrg$Organisation$GeoLoc$Location$PostCode,
                                                 OrgStart     = as.Date(OrgDates$Start, origin = "1900-01-01"),
                                                 OrgEnd       = as.Date(OrgDates$End, origin = "1900-01-01"),
                                                 OrgRoleStart = as.Date(RolePeriods$RoleStart[k],origin = "1900-01-01"),
                                                 OrgRoleEnd   = as.Date(RolePeriods$RoleEnd[k],origin = "1900-01-01"),
                                                 RelOrgId     = Rels$RelOrgId[j],
                                                 RelType      = Rels$RelType[j],
                                                 RelOrgPrimaryRole = Rels$RelPrimaryRole[j],
                                                 RelStart     = as.Date(Rels$Start[j],origin = "1900-01-01"),
                                                 RelEnd       = as.Date(Rels$End[j],origin = "1900-01-01"), stringsAsFactors=FALSE)

                            lkup <- bind_rows(lkup,addrow)
                        }
                    }
                }
            }
        }
    }
    return(lkup)
    }



