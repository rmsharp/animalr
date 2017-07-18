#' Returns a character vector of common names using a vector of
#' three character species codes (species_codes)
#'
#' @param conn database connection object to the animal database
#' @param species_codes three character species codes (species_codes)
#' @export
get_common_name_for_species_code <- function(conn, species_codes) {
  dict <- get_common_name_for_genus(conn)
  as.character(dict[species_codes])
}
