## run from root directory against source (installed=FALSE) or installed directory
.rawDataToSavedData <- function(installed=FALSE) {
    if (installed) {
        all <- read_csv(system.file("rawdata", "ncis_bystate_bymonth_bytype.csv", package="gunsales"), na = "#N/A")
        pop.total <- read_csv(system.file("rawdata", "population.csv", package="gunsales"))
    } else {                          
        all <- read_csv("inst/rawdata/ncis_bystate_bymonth_bytype.csv", na = "#N/A")
        pop.total <- read_csv("inst/rawdata/population.csv")
    }
    save(all, pop.total, file="data/gunsales.RData", compress="xz", compression_level=9)
}

#' Source data from the FBI's National Instant Criminal Background Check System
#'
#' It was converted from the original PDF format to CSV using Tabula.
#'
#' @format A \sQuote{tbl_df} and data frame object with 11480 observation of 34 variables
#' @source \url{https://www.fbi.gov/about-us/cjis/nics/reports/nics_firearm_checks_-_month_year_by_state_type.pdf}
#' @seealso The GitHub repository created by Buzzfeed containing a similar data set
#' at \url{https://github.com/BuzzFeedNews/nics-firearm-background-checks/}
"all"

#' US Population Growth data
#'
#' Montly observations about population growth
#'
#' @docType data
#' @format A \sQuote{tbl_df} and data frame object with 217 observation of 3 variables
#' @source US Census Bureau 
"pop.total"



