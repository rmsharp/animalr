#' Returns the character string corresponding to the animal Id columns for
#' specific tables.
#'
#' @param conn database connection object
#' @param table_names character vector with table names
#' @param owner character vector of length 1 having the database schema owner.
#' Defaults to "DBO".
#' @export
get_id_str <- function(conn, table_names, owner = "dbo") {
  sapply(table_names, FUN = function(table_name) {
    id_str <- NA
    if (any(stri_detect_regex(tolower(get_column_names(conn,table_name, owner)),
                              pattern = "^id$"))) {
      id_str = "id"
    } else if (any(stri_detect_regex(tolower(get_column_names(conn,table_name,
                                                              owner)),
                                     pattern = "^snprc_id$"))) {
      id_str = "snprc_id"
    } else if (any(stri_detect_regex(tolower(get_column_names(conn,table_name,
                                                              owner)),
                                     pattern = "^animal_id$"))) {
      id_str = "animal_id"
    }
    id_str}, USE.NAMES = FALSE)
}
