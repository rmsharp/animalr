#' Returns TRUE if view exist and FALSE if it does not
#'
#' @param conn database connection object
#' @param view_name character vector of length one having the name of the
#' view.
#' @param schema character vector of length one having the name of the
#' schema
#' @import RODBC
#' @import stringi
#' @export
is_database_view <- function(conn, view_name, schema = "DBO") {
  sql_txt <- stri_c(
    "SELECT count(1) FROM INFORMATION_SCHEMA.VIEWS
    WHERE TABLE_NAME='", view_name, "'
      AND TABLE_SCHEMA = '", schema, "' ")
  sqlQuery(conn, sql_txt) > 0
}
