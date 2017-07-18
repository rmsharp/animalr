#' Returns dataframe with animals having a specific character in their
#' permanent Id.
#'
#' @param conn database connection object
#' @param char character to be matched
#' @param exclude character vector having two character species codes of
#' species to exclude from result set.
#' @param include character vector having two character species codes of
#' species to include in the result set.
#'
#' @import rmsutilityr
#' @import RODBC
#' @import stringi
#' @export
get_ids_having_specific_char <- function(conn, char, exclude = character(0),
                                         include = character(0)) {
  species_sql <- " "
  if (length(exclude) == 0 & length(include) == 0) {
    species_sql <- ""
  } else if (length(exclude) > 0) {
    species_sql <- stri_c(species_sql, "and vs.arc_species_code not in ('",
                          vector2string(exclude, SS = "', '"), "') ")
  } else if (length(include) > 0) {
    species_sql <- stri_c(species_sql, "and vs.arc_species_code in ('",
                          vector2string(include, SS = "', '"), "') ")
  }
  look_for_char_in_ids_sql <- stri_c(
    "SELECT avs.common_name, m.id, m.birth_date, m.bd_status,
    m.birth_code, m.death_date, m.dd_status, m.death_code,
    m.sire_id, m.dam_id, m.sex, m.species,
    m.genotyped
    FROM master m
    INNER JOIN valid_species vs
    ON m.species = vs.species_code ", species_sql, "
    INNER JOIN arc_valid_species_codes avs
    ON vs.arc_species_code = avs.arc_species_code
    WHERE id like '%|", char, "%' ESCAPE '|'")
  ids_with_char_df <- sqlQuery(conn, look_for_char_in_ids_sql,
                               stringsAsFactors = FALSE)
  ids_with_char_df
}
