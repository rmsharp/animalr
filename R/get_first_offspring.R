#' Get the first offspring for animals of a specified sex and arc_species_code
#' within a specified date range of provided.
#'
#' @param conn database connection object
#' @param arc_species_code character vector of length 1 having two character
#' species code
#' @param sex character vector of length 1 having "F", "M", or "both"
#' @param start_date optional character vector of length 1 having a valid date
#' represented by a unambiguous string value that specified the begining
#' date of the date interval of interest.
#' @param end_date optional character vector of length 1 having a valid date
#' represented by a unambiguous string value that specified the end
#' date of the date interval of interest.
#' @import anytime
#' @import RODBC
#' @import stringi
#' @export
get_first_offspring <- function(conn, arc_species_code,
                                sex = "F", start_date = NULL,
                                end_date = NULL) {
  if (!missing(start_date)) {
    start_date <- strftime(anytime(start_date), format = "%m-%d-%Y")
    start_date_sql <- stri_c(
      " and o.offspring_birth_date >= '", start_date, "' ")
  } else {
    start_date_sql <- " "
  }
  if (!missing(end_date)) {
    end_date <- strftime(anytime(end_date), format = "%m-%d-%Y")
    end_date_sql <- stri_c(
      " and o.offspring_birth_date <= '", end_date, "' ")
  } else {
    end_date_sql <- " "
  }
  if (!any(sex %in% c("both", "M", "F")))
    stop("sex must be one of 'F', 'M', or 'both'")
  if (sex == "both") {
    sex_sql <- " and m.sex in ('M', 'F') "
  } else if (sex == "M") {
    sex_sql <- " and m.sex = 'M' "
  } else if (sex == "F") {
    sex_sql <- " and m.sex = 'F' "
  }

  sql_txt <- stri_c(
    "select m.id, cast(m.birth_date as date) as birth_date, o.offspring_birth_date,
    datediff(day, cast(m.birth_date as date), o.offspring_birth_date) as age_days
    from master m
    inner join offspring o on m.id = o.id
    inner join valid_species vs on m.species = vs.species_code
    and vs.arc_species_code = '", arc_species_code, "'
    where o.offspring_birth_date = (
    select min(o2.offspring_birth_date)
    from offspring o2
    where o.id = o2.id)", sex_sql, start_date_sql, end_date_sql)

  first_offspring <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
  first_offspring
}
