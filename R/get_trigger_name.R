#' Returns a character vector with one trigger name af a specific type for each table
#' name provided.
#'
#' @param type one of c("insert", "update", "delete")
#' @param table_names character vector with the table names
#' @import rmsutilityr
#' @import stringi
#' @export
get_trigger_name <- function(type, table_names) {
  types <- list(insert = "ti_", update = "tu_", delete = "td_")
  type <- tolower(type)
  if (type %in% names(types)) {
    trigger_names <- stri_c(types[type][[1]], table_names)
  } else {
    stop(stri_c("type must be one of ", get_and_or_list(types, conjunction = "or"),
                ". type is: ", type))
  }
  trigger_names
}
