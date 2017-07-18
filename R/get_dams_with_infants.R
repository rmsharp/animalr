#' Dams with infants within a specified date range of provided.
#'
#' @param conn database connection object
#' @param arc_species_code character vector of length 1 having two character
#' species code
#' @param current_date optional character vector of length 1 having a valid date
#' represented by a unambiguous string value that specifies the
#' date for which the report is current. While most of the time current_date
#' will indeed be the current date. This allows the user to look at the infants
#' at a prior date.
#' @param days_as_infant number of days that an animal is considered to be
#' an infant for this purpose.
#' @import anytime
#' @import RODBC
#' @import stringi
#' @export
get_dams_with_infants <-
  function(conn, arc_species_code,
           current_date = strftime(Sys.Date(), format = "%m-%d-%Y"),
           days_as_infant = 180) {
    current_date <- strftime(anytime(current_date), format = "%m-%d-%Y")

    sql_txt <- stri_c(
      "select m.id, m2.id, cast(m.birth_date as date) as birth_date,
      m2.birth_date,
      datediff(day, m2.birth_date, '", current_date, "') as age_days
      from master m
      inner join master m2 on m.id = m2.dam_id
      inner join valid_species vs on m.species = vs.species_code
      and vs.arc_species_code = '", arc_species_code, "'
      where datediff(day, m2.birth_date, '", current_date,
      "') <= ", days_as_infant,
      " and (m2.death_date is NULL or m2.death_date > '", current_date, "')
      order by m.id, m2.id")

    animals_with_infants <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
    animals_with_infants
  }
