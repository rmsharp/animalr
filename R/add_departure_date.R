#' Returns dataframe with POSIXct date vector of departure dates for each animal
#' in character vector id of the provided dataframe id_df.
#' Returns NA in vector if not available.
#'
#' Assumes access to master database table in animal database.
#' @param conn database connection
#' @param id_df dataframe with animal Ids in id column
#' @import RODBC
#' @import rmsutilityr
#' @import stringi
#' @export
add_departure_date <- function(conn, id_df) {
  ids <- id_df$id[!is.na(id_df$id)]
  ids_str <- vector2string(unique(blank_fill_ids(ids)), SS = "','")
  departure_dates_df <- sqlQuery(conn, stri_c(
    "SELECT id, date, description, dispositionType from labkey_etl.V_DEPARTURE
    where id in ('", ids_str, "')"))
  departure_dates_df$id <- blank_fill_ids(departure_dates_df$id)
  id_df$id <- blank_fill_ids(id_df$id)
  merge(id_df, departure_dates_df, by = 'id', sort = TRUE, all = TRUE)
}
