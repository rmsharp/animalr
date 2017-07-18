#' Returns the number of rows deleted from a database table when all the rows associated
#' with a list of names are deleted.
#'
#' @param conn database connection object
#' @param table_name character vector of length 1 having the table name
#' @param ids character vector containing the animal Ids
#' @param owner character vector of length 1 having the database schema owner.
#' Defaults to "DBO".
#' @import stringi
#' @import RODBC
#' @export
delete_id_from_tbl <- function(conn, table_name, ids, owner = "dbo") {
  ids <- blank_fill_ids(ids)
  rows <- integer(length(ids))
  id_str <- get_id_str(conn, table_name)
  for (i in seq_along(ids)) {
    id <- ids[i]
    sql_txt <- stri_c("delete from ", owner, ".", table_name, " where ", id_str,
                      " = '", id, "'")
    status <- sqlQuery(conn, sql_txt)
    if (!length(status) == 0)
      stop(stri_c("delete from ", owner, ".", table_name, " failed: id: ", id,
                  ";  status: ", status))
    rows[i] <- as.integer(sqlQuery(conn, "select @@rowcount"))
  }
  sum(rows)
}
