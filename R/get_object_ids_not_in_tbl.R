#' Returns a character vector of OBJECT_ID values from a character vector
#' (o_ids) that were not found to be in the database table being queried.
#'
#' @param conn database connection object
#' @param o_id character vector of object_id values to see if they exist
#' in the table table_name.
#' @param table_name character vector of length one with the table name
#' @export
get_object_ids_not_in_tbl <- function(conn, o_id, table_name) {
  o_id[!o_id %in% get_object_ids_in_tbl(conn, o_id, table_name)]
}
