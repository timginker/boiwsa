#' US finished motor gasoline product supplied
#'
#' Weekly data beginning 2 February 1991, ending 20 January 2017. Units are "million barrels per day".
#'
#' @format ## Data.Frame
#' A data frame with 1355 rows and 2 columns:
#' \describe{
#'   \item{date}{date in a date format}
#'   \item{y}{gasoline consumption}
#' }
#' @source fpp2 package
"gasoline.data"

#' @title Israeli working dates
#' @description Israeli working dates
#' @format A data frame with 21550 rows and 4 variables:
#' \describe{
#'   \item{\code{DATE_VALUE}}{Date}
#'   \item{\code{ISR_WORKING_DAY_PART}}{1: full working day, 0.5: half working day, 0: holiday}
#'   \item{\code{JEWISH_FULL_DATE}}{Jewish date}
#'   \item{\code{DATE_WEEK_NUMBER}}{Weekday}
#'}
#' @source Personal
"dates_il"

#' @title Israeli moving holiday dates
#' @description Rosh Hashanah and Pesach dates
#' @format A data frame with 51 rows and 3 variables:
#' \describe{
#'   \item{\code{year}}{Year}
#'   \item{\code{rosh}}{Rosh Hashanah date}
#'   \item{\code{pesah}}{Pesach date}
#'}
#' @source Personal
"holiday_dates_il"

