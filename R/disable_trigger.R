#' Has the side effect of disabling a specific trigger as defined by type and
#' table name.
#'
#' @param conn database connection object
#' @param type one of c("insert", "update", "delete")
#' @param table_name character vector with the table name
#' @import RODBC
#' @import stringi
#' @export
disable_trigger <- function(conn, type, table_name) {
  trigger_name <- get_trigger_name(type, table_name)
  sql_txt <- stri_c("DISABLE TRIGGER dbo.", trigger_name, " ON ", table_name)
  status <- sqlQuery(conn, sql_txt)
  if (!length(status) == 0)
    stop(stri_c("disable trigger on ", table_name, " failed; status: ", status))
}
