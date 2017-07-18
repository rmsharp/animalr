#' Returns the number of rows updated when old Ids are replaced by replacement Ids
#' in one or more database tables.
#'
#' @param conn database connection object
#' @param table_names character vector with one or more table names
#' @param old_ids character vector containing the animal Ids to be replaced
#' @param replacement_ids character vector containing the replacement animal Ids
#' @param owner character vector of length 1 having the database schema owner.
#' Defaults to "DBO".
#' @import RODBC
#' @import rmsutilityr
#' @import stringi
#' @export
update_ids_in_tables_by_id <- function(conn, table_names, old_ids,
                                       replacement_ids, owner = "dbo") {
  old_ids <- blank_fill_ids(old_ids)
  replacement_ids <- blank_fill_ids(replacement_ids)
  if (length(old_ids) != length(replacement_ids))
    stop("old_ids and replacement_ids do not have the same length.")
  rows <- integer(length(old_ids) * length(table_names))
  row_count <- 0
  for (table_name in table_names) {
    id_str <- get_id_str(conn, table_name, owner)
    for (i in seq_along(old_ids)) {
      o_id <- old_ids[i]
      r_id <- replacement_ids[i]
      sql_txt <- stri_c(
        "update ", owner, ".", table_name, " set ", id_str, " = '", r_id,
        " where ", id_str, " = '", o_id, "'")
      status <- sqlQuery(conn, sql_txt)
      if (!length(status) == 0) {
        if (!status == "No Data")
          stop(stri_c("update of id in ", owner, ".", table_name,
                      " failed: o_id: ", o_id,
                      "; r_id: ", r_id, " status: ", status))
      }
      row_count <- row_count + 1
      rows[row_count] <- as.integer(sqlQuery(conn, "select @@rowcount"))
    }
  }
  sum(rows)
}
