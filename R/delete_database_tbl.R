#' Returns TRUE if the database table was successfully deleted.
#'
#' @param conn database connnection object
#' @param table_name character vector length one having the table name
#' @param owner character vector of length 1 having the database schema owner.
#' Defaults to "DBO".
#' @import RODBC
#' @import stringi
#' @export
delete_database_tbl <- function(conn, table_name, owner = "dbo") {
  sql_txt <- stri_c(
    "IF EXISTS(select 1
    from  sysobjects
    where  id = object_id('", owner, ".", table_name, "')
    and   type = 'U')
    drop table ", table_name)
  status <- sqlQuery(conn, sql_txt)
  if (length(status) != 0) {
    print(stri_c("Delete of database table '", owner, ".", table_name,
                 "' failed with error message of: ", status))
    return(FALSE)
  } else {return(TRUE)}
}
