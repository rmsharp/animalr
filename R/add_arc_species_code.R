#' Adds arc_species_code for each animal
#' in character vector id of the provided dataframe id_df.
#'
#' Returns NA in vector if not available.
#'
#' Assumes access to master database table in animal database.
#' @param conn database connection
#' @param id_df dataframe with animal Ids in id column
#' @importFrom RODBC sqlQuery
#' @importFrom rmsutilityr blank_fill_ids vector2string
#' @importFrom stringi stri_c
#' @export
add_arc_species_code <- function(conn, id_df) {
  ids_str <- vector2string(unique(blank_fill_ids(id_df$id)), SS = "','")
  arc_species_code_df <- sqlQuery(conn, stri_c(
    "SELECT id, arc_species_code from current_data where id in ('", ids_str, "')"))
  merge(id_df, arc_species_code_df, by = 'id', sort = TRUE, all = TRUE)
}
