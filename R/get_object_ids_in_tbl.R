#' Returns a character vector of OBJECT_ID values from a character vector
#' (o_ids) that were found to be in the database table being queried.
#'
#' @param conn database connection object
#' @param o_id character vector of object_id values to see if they exist
#' in the table table_name.
#' @param table_name character vector of length one with the table name
#' @import rmsutilityr
#' @import RODBC
#' @import stringi
#' @export
get_object_ids_in_tbl <- function(conn, o_id, table_name) {
  if (length(o_id) < 1)
    return(character(0))
  o_id_str <- vector2string(o_id, SS = "', '")
  sql_txt <- stri_c(
    "select OBJECT_ID from ", table_name, " where OBJECT_ID in ('", o_id_str, "')
    order by OBJECT_ID ")
  sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)$OBJECT_ID
}
