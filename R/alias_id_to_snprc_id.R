#' Converts an alias (alternate) animal Id to an animal Id.
#'
#' A dataframe is returned with primary Id (snprc_id), alias Id (id_value),
#' and alternate Id type (id_type).
#'
#' @param conn database connection object
#' @param id_value character vector of one or more alternate animal Ids.
#' Primary Ids are also allowed.
#' @import RODBC
#' @import stringi
#' @export
alias_id_to_snprc_id <- function(conn, id_value) {
  id_value <- id_value[!is.na(id_value)]
  id_value_str <-
    vector2string(unique(c(id_value,
                           suppressWarnings(blank_fill_ids(id_value)))),
                  SS = "', '")
  sql_txt <- stri_c(
    "select ih.sfbr_id, ih.id_value, ih.id_type, vit.id_description
    from id_history ih
    inner join valid_id_types vit on ih.id_type = vit.id_type
    where ih.id_value in ('", id_value_str, "') ")
  id_df <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
  t_id_value <- sapply(id_value, function(id) {
    b_id <- suppressWarnings(blank_fill_ids(id))
    if (b_id %in% id_df$sfbr_id) b_id
    else id})
  merge(data.frame(id_value = t_id_value, stringsAsFactors = FALSE), id_df, by = "id_value", all.x = TRUE)
}
