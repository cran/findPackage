#' @title Find 'CRAN' Package by topic
#'
#' @description Finds CRAN packages by the topic requested. The topic can be
#'  given as a character string or as a regular expression, and will help users
#'  to locate packages matching their specified requirement.
#' @param topicString The topic as a character string or as a 
#' regular expression
#' @param fromDate  Format: YYYY-MM-DD. The search in CRAN will be restricted 
#' to packages published on or after this date, default is from the earliest 
#' available date, i.e. September 2008
#' @param toDate    Format: YYYY-MM-DD. The search in CRAN will be restricted 
#' to packages published on or before this date, default is till the 
#' current date
#' @param sortOrder The matching data is sorted by default (A) with the 
#' earlier published packages preceding the later ones. For descending sort,
#' more recent earlier, use "D"
#' @return A data frame with the package name, the date of publication,
#' title of the package and its description
#' @importFrom stats runif
#' @examples
#' ahp <- findPackage("(Analytic Hierarchy Process)|(AHP)");
#'
#' AHP_or_DEA <- 
#' findPackage(
#' "(Analytic Hierarchy Process)|(AHP)|(Data Envelopment Analysis)|(DEA)");
#'
#' fa <- findPackage("Factor Analysis")
#' @export
findPackage <- function(topicString="", 
                        fromDate="2008-09-01", 
                        toDate=Sys.Date(), sortOrder="A") { 
  if (topicString=="") stop("The topicString parameter is mandatory")
  fd <- regexpr("^(\\d{4}\\-(0?[1-9]|1[012])\\-(0?[1-9]|[12][0-9]|3[01]))$", 
                fromDate)
  if (fd!=1) stop("From date is not correctly specified")
  td <- regexpr("^(\\d{4}\\-(0?[1-9]|1[012])\\-(0?[1-9]|[12][0-9]|3[01]))$", 
                toDate)
  if (td!=1) stop("To date is not correctly specified")
  if (fromDate > toDate) stop("From date is later than To date")
  if (sortOrder != "A" & sortOrder !="D") 
    stop("Sort order can be 'A' or 'D' only")
  
  db <- tools::CRAN_package_db()
  rows <- grep(topicString, db$Description)
  db <- db[rows,
           c("Package", "Date/Publication", "Title", "Description")]
  db$`Date/Publication` <- sub("^([^ ]+).*", "\\1", db$`Date/Publication`)
  db <- db[  db$`Date/Publication` >= fromDate & 
             db$`Date/Publication` <= toDate,]
  if (sortOrder=="A") {
    db <- db[order(db$`Date/Publication`,decreasing=FALSE),]
  } else {
    db <- db[order(db$`Date/Publication`,decreasing=TRUE),]
  }
 
  rownames(db) <- NULL
  return(db)
}
