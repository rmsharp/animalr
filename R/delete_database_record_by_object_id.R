#' Returns logical vector indicating TRUE if record was deleted.
#'
#' This deletes records one at a time, which is expensive in time,
#' but returns a logical variable indicating success (TRUE) or
#' failure for each object Id provided.
#'
#' @param conn database connection object
#' @param table character variable having the database table name
#' @param object_id integer vector with obr object Id(s) to delete
#' @import RODBC
#' @import stringi
#' @export
delete_database_record_by_object_id <- function(conn, table, object_id) {
  records <- logical(length(object_id))
  messages <- character(length(object_id))
  for (i in seq_along(object_id)) {
    status <- sqlQuery(conn, stri_c("delete from ", table,
                                    " where OBJECT_ID = '",
                                    object_id[i], "'"))
    if (length(status) == 0) {
      records[i] <- TRUE
    } else {
      records[i] <- FALSE
      messages[i] <- stri_c("[1] ", status[1], "\\n[2] ", status[2])
    }
  }
  data.frame(record = records, message = messages)
}
