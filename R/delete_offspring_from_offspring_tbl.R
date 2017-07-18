#' Returns the number of rows deleted from the offspring table
#'
#' @param conn database connection object
#' @param ids character vector with the animal Ids to be deleted if they are
#' the offspring_id
#' @param owner character vector of length 1 having the database schema owner.
#' Defaults to "DBO".
#' @import RODBC
#' @import stringi
#' @export
delete_offspring_from_offspring_table <- function(conn, ids, owner = "dbo") {
  ids <- blank_fill_ids(ids)
  rows <- integer(length(ids))
  for (i in seq_along(ids)) {
    id <- ids[i]
    sql_txt <- stri_c("delete from ", owner, ".offspring where offspring_id = '", id, "'")
    status <- sqlQuery(conn, sql_txt)
    if (!length(status) == 0)
      stop(stri_c("delete from ", owner, ".offspring failed: offspring_id: ", id,
                  ";  status: ", status))
    rows[i] <- as.integer(sqlQuery(conn, "select @@rowcount"))
  }
  sum(rows)
}
