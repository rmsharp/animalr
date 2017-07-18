#' Returns TRUE if table exist and FALSE if it does not
#'
#' @param conn database connection object
#' @param table_name character vector of length one having the name of the
#' table.
#' @import RODBC
#' @import stringi
#' @export
is_database_tbl <- function(conn, table_name) {
  sql_txt <- stri_c(
    "IF EXISTS (SELECT 1 FROM INFORMATION_SCHEMA.TABLES
    WHERE TABLE_TYPE='BASE TABLE'
    AND TABLE_NAME = '", table_name, "')
    SELECT 1 AS result ELSE SELECT 0 AS result ")
  if (sqlQuery(conn, sql_txt)$result == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
