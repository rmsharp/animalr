#' Get non-productive animals of a specified sex, arc_species_code, and age.
#'
#' @param conn database connection object
#' @param sex character vector of length one indicating sex ("M", "F", or
#' "both").
#' @param arc_species_code character vector of length 1 having two character
#' species code
#' @param age numeric value of age indicating that the animals list are to be
#' at least \code{age} years old.
#' @import RODBC
#' @import stringi
#' @export
get_non_productive_animals <- function(conn, sex, arc_species_code, age) {

  if (!any(sex %in% c("M", "F", "both"))) {
    stop("sex must be one of 'F', 'M', or 'both'.")
  }
  if (sex == "both") {
    sex_sql <- " and m.sex in ('F', 'M') "
  } else if (sex == "F") {
    sex_sql <- " and m.sex = 'F' "
  } else {
    sex_sql <- " and m.sex = 'M' "
  }

  sql_txt <- stri_c(
    "select c.id, convert(char(10), m.birth_date, 110) as birth_date,
    c.location
    from current_data c
    inner join master m on c.id = m.id
    inner join valid_species vs on m.species = vs.species_code
    where vs.arc_species_code = '", arc_species_code, "' ",
    sex_sql, "
    and c.at_sfbr = 'Y'
    and c.offspring_id is null
    and datediff(yy,m.birth_date,getdate()) >= ", age, "
    order by m.birth_date")
  npa <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)

  npa
}
