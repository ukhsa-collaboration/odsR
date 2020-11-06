
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/PublicHealthEngland/odsR.svg?branch=master)](https://travis-ci.org/PublicHealthEngland/odsR?branch=master)

[![Coverage Status](https://coveralls.io/repos/github/PublicHealthEngland/odsR/badge.svg?branch=master)](https://coveralls.io/github/PublicHealthEngland/odsR?branch=master)

<br/> <br/>

odsR Package
============

This is an R package to facilitate the extraction of NHS organisation data from the NHS Digital ODS API into the R environment.

Any feedback would be appreciated and can be provided using the Issues section of the GitHub repository, or by emailing <PHDS@phe.gov.uk>. Please note that whilst we are happy to share this package we have limited capacity to invest time in its further development.

<br/> <br/>

Installation
------------

#### From GitHub

You can install the latest version of odsR from GitHub with:

``` r
if (!require(devtools)) install.packages("devtools")

devtools::install_github("PublicHealthEngland/odsR",
                         build_vignettes = TRUE,
                         dependencies = "suggests")
```

#### From zip

Download this repository from Git and either build from source or do the following:

``` r
if (!require(devtools)) install.packages("devtools")

source <- devtools:::source_pkg("C:/path/to/odsR-master.zip")
devtools::install(source)
```

#### Using Base R

To install the package without the use of devtools, download the .tar.gz file and then run the following Where path\_to\_file would represent the full path and file name:

``` r
install.packages(path_to_file, repos = NULL, type=“source”)
```

<br/> <br/>

Package Versioning
------------------

Following installation of this package, type ‘packageVersion(“odsR”)’ in the R console to show the package version. If it is suffixed with a 9000 number then you are using an unapproved development version.

Released versions of this package will have version numbers consisting of three parts: major.minor.patch. In-development versions of this package will have a fourth component, the development version number, which will increment from 9000.

See <http://r-pkgs.had.co.nz/description.html#version> for further information on package versioning

<br/> <br/>

Package Contents
----------------

The package contents are described below - see individual item documentation for full details

**Functions:**

RELEASED:

-   getODS (get summary organisation data for all organisations that meet the specified filter criteria)
-   getODSfull (get full organisation data for a single organisation)
-   addorgname (add an Organisation Name column to a data frame that contains Organisation Codes)
-   OrgRelLkp (create lookup tables from one organisation type to another based on relationship information held within the ODS API - QAd for creation of GP Practice to CCG lookups but otherwise experimental)

**Datasets:**

-   none

**Vignettes:**

-   none
