#' phonedata
#'
#' Contains exemplary raw phone logs.
#'
#' @name phonedata
#' @keywords data
#' @docType data
"phonedata"

#' studentlife.small
#'
#' Contains exemplary gps and app data from the studentlife dataset.
#'
#' http://studentlife.cs.dartmouth.edu/dataset.html
#'
#' Wang, Rui, Fanglin Chen, Zhenyu Chen, Tianxing Li, Gabriella Harari, Stefanie Tignor, Xia Zhou, Dror Ben-Zeev, and Andrew T. Campbell. "StudentLife: Assessing Mental Health, Academic Performance and Behavioral Trends of College Students using Smartphones." In Proceedings of the ACM Conference on Ubiquitous Computing. 2014.
#' @name studentlife.small
#' @keywords data
#' @docType data
"studentlife.small"

#' lakers
#'
#' lakers dataset from the lubridate package: \link[lubridate]{lakers}. See also:
#'
#' https://www.rdocumentation.org/packages/lubridate/versions/1.7.4/topics/lakers
#'
#' A new column 'timestamp' was added for easier usage of this package. Also a new column 'userId', which indicates different games, was added.
#' Please note, that the timezone was set to 'America/Los_Angeles', ignoring different timezones.
#' @name lakers.ts
#' @keywords data
#' @docType data
"lakers.ts"
#
# lakers = lubridate::lakers
# lakers$userId = paste0(lakers$date, lakers$opponent)
# time = lakers$time
# date = lakers$date
# date_time = paste(date, time)
# ts = strptime(date_time, "%Y%m%d %H:%M", tz = "America/Los_Angeles")
# lakers$timestamp = as.numeric(ts)
# lakers.ts = lakers

