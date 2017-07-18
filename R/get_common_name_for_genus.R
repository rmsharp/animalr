#' Make a named vector to translate three letter genus and species codes to
#' common_names.
#'
#' Makes a named vector with three letter \code{species_code} as the names and
#' \code{common_name} from the \code{arc_valid_species_codes} database table
#' are the vector values.
#'
#' The data are constructed so that the common name is the one associated with
#' the two character \code{arc_species_code} in the
#' \code{arc_valid_species_codes} database table.
#'
#' @param conn database connection object to the animal database
#' @import RODBC
#' @import stringi
#' @export
get_common_name_for_genus <- function(conn) {
  sql_txt <- stri_c(
    "select  vs.species_code, avs.arc_species_code, avs.common_name
    from arc_valid_species_codes avs
    inner join valid_species vs
    on avs.arc_species_code = vs.arc_species_code")
  df <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
  genus_to_common_name <- df$common_name
  names(genus_to_common_name) <- df$species_code
  genus_to_common_name
}
