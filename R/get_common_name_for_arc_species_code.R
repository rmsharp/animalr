#' Get common name for arc_species_codes
#' @param conn database connection object to the animal database
#' @param arc_species_codes a character vector with one or more two character
#' species codes as defined by the IACUC.
#' @import RODBC
#' @import stringi
#' @export
get_common_name_for_arc_species_codes <- function(conn, arc_species_codes) {
  common_names <- character(0)
  for (arc_species_code in arc_species_codes) {
    common_name <-
      sqlQuery(conn,
               stri_c("select avs.common_name ",
                      "from arc_valid_species_codes avs ",
                      "where avs.arc_species_code = '",
                      arc_species_code, "'"))
    common_name <- unlist(stri_split_fixed(as.character(common_name[[1]]), ' '))
    substring(common_name, 1, 1) <- toupper(substring(common_name, 1, 1))
    substring(common_name, 2) <- tolower(substring(common_name, 2))
    common_name <- stri_c(common_name, collapse = ' ')
    common_names <- c(common_names, common_name)
  }
  common_names
}
