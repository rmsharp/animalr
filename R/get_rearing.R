#' Get rearing code and rearing description for Ids provided.
#'
#' Takes a database connection and a
#' character vector of animal Ids and retrieves the siblings for each
#' id. All returned rows represent a unique individual. The SQL gets all
#' siblings of the type specified.
#'
#'
#' @param id_df dataframe with a column (\code{id}) made up of a character
#' vector of individual ids
#' @param conn database connection
#' @import RODBC
#' @import rmsutilityr
#' @import stringi
#' @export
get_rearing <- function(id_df, conn) {
  ids <- id_df$id[!is.na(id_df$id)]
  ids_str <- vector2string(unique(blank_fill_ids(ids)), SS = "','")
  rearing <- sqlQuery(conn, stri_c(
    "SELECT r.id, r.rearing_code, vr.description
    FROM rearing as r
    INNER JOIN valid_rearing vr on r.rearing_code = vr.rearing_code
    WHERE r.id in ('", ids_str, "')
    ORDER BY r.id "), as.is = TRUE)
  rearing_df <- merge(id_df, rearing, by = "id", all.x = TRUE)
  rearing_df
}
