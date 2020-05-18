#' Returns the average number of cagemates each animal has for days
#' between and including start and end dates.
#'
#' @param conn database connection
#' @param ids character vector with an animal Id in each cell
#' @param start character string representation of start date in mm-dd-yyyy
#' format
#' @param end character string representation of end date in mm-dd-yyyy
#' format
#' @import magrittr
#' @importFrom RODBC sqlQuery
#' @importFrom rmsutilityr blank_fill_ids vector2string
#' @importFrom stringi stri_c
#' @export
get_average_number_of_cagemates <- function(conn, ids, start, end) {
  id <- cagemates <- NULL
  ids <- blank_fill_ids(ids)
  ids_str <- vector2string(ids)
  sql_txt <- stri_c(
    "select t.id , t.midnight_location,
    t.target_date, count(c.id) as cagemates
    from daily_demo t
    inner join daily_demo c on t.target_date = c.target_date
    and t.midnight_location = c.midnight_location
    and t.id <> c.id
    where t.id in ('", ids_str, "')
    and t.target_date between '", start, "' and '", end, "'
    group by t.id, t.midnight_location, t.target_date")

  ncm <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
  avg_cagemates <- ncm %>% dplyr::group_by(id) %>%
    dplyr::summarise(avg_cagemates = mean(cagemates))
  avg_cagemates
}
