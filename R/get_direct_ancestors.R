#' Get direct ancestors for supplied IDs.
#'
#' @return A dataframe with all direct ancestors for Ids provided.
#'
#' Takes a database connection and a
#' character vector of animal Ids and retrieves the direct ancestors for each
#' id. All returned rows represent a unique individual. The SQL gets animals
#' until there are no new parents. That is the sires and dams are NULL for
#' the progenitors.
#' gets immediate parents for one individual :
#' --SELECT * FROM dbo.f_get_pedigree(' 30145')
#' @param conn database connection object
#' @param ids character vector of individual ids
#' @import RODBC
#' @import rmsutilityr
#' @import stringi
#' @export
get_direct_ancestors <- function(conn, ids) {
  ids_str <- vector2string(unique(blank_fill_ids(ids)), SS = "','")
  direct_ancestors <- sqlQuery(conn, stri_c(
    "WITH cte1 (child_id, id, dam_id, sire_id, sex, LEVEL) AS (",
    ## anchor member definition
    "  SELECT CAST('' AS varchar(6)), m.id, m.dam_id, m.sire_id, ",
    "    m.sex, 1 AS LEVEL ",
    "  FROM MASTER AS m ",
    "  WHERE m.id in ('", ids_str, "') ",
    "  UNION ALL ",
    ##recursive part of query
    "SELECT c.id AS child_id, m.id, m.dam_id, m.sire_id, m.sex, ",
    "  LEVEL + 1 AS LEVEL ",
    "FROM cte1 AS c ",
    "JOIN master AS m ON c.dam_id = m.id OR c.sire_id = m.id) ",
    ##
    "SELECT  distinct c.id, c.dam_id, c.sire_id, c.sex ",
    "FROM cte1 AS c ",
    "ORDER by c.id"), as.is = TRUE)
  direct_ancestors$child_id <- NULL
  direct_ancestors$LEVEL <- NULL
  direct_ancestors
}
