#' Returns dataframe with POSIXct date vector of death dates for each animal
#' in character vector id of the provided dataframe id_df.
#' Returns NA in vector if not available.
#'
#' Assumes access to master database table in animal database.
#' @param conn database connection
#' @param id_df dataframe with animal Ids in id column
#' @importFrom RODBC sqlQuery
#' @importFrom rmsutilityr blank_fill_ids vector2string
#' @importFrom stringi stri_c
#' @export
add_death_date <- function(conn, id_df) {
  ids <- id_df$id[!is.na(id_df$id)]
  ids_str <- vector2string(unique(blank_fill_ids(ids)), SS = "','")
  death_dates_df <- sqlQuery(conn, stri_c(
    "SELECT id, death_date from master where id in ('", ids_str, "')"))
  death_dates_df$id <- blank_fill_ids(death_dates_df$id)
  id_df$id <- blank_fill_ids(id_df$id)
  merge(id_df, death_dates_df, by = 'id', sort = TRUE, all = TRUE)
}
