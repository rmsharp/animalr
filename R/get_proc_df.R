#' Get procedure data for animals within date ranges.
#'
#' @return A dataframe with admission and procedure data with user name (user_n),
#' and entry_date_tm of animals for the date range specified.
#'
#' An example of how to construct the \code{ranges} list follows:
#'
#' ranges <- list(ids = c(" 12345", "1X1234"),
#'                 dates = list(c("1/2/2014", "12/30/2014"),
#'                              c("6/07/2010", "6/08/2017")))
#'
#' @param conn database connection object
#' @param ranges has the list of animals with corresponding date ranges.
#' @import RODBC
#' @import stringi
#' @export
get_proc_df <- function(conn, ranges) {
  for (i in seq_along(ranges$ids)) {
    sql_txt <- stri_c(
      "select ca.short_description, c.id, p.proc_id,
      p.proc_date_tm,
      p.admit_id, p.proc_narrative,
      p.user_name as user_n, p.entry_date_tm
      from procedures_entered p
      inner join clinic c on c.admit_id = p.admit_id
      inner join charge_account ca on ca.charge_id = c.charge_id
      where c.id in ('", vector2string(ranges$ids[[i]], SS = "', '"), "')
      and cast(p.proc_date_tm as date) <= '", ranges$dates[[i]][2], "'
      and cast(p.proc_date_tm as date) >= '", ranges$dates[[i]][1], "'
      order by c.id, p.proc_date_tm, entry_date_tm")
    df <- sqlQuery(conn, sql_txt, as.is = TRUE)
    if (i == 1) {
      proc_df <- df
    } else {
      proc_df <- rbind(proc_df, df)
    }
  }
  proc_df$timestamp <- NULL
  proc_df$proc_date_tm <- stri_sub(proc_df$proc_date_tm, 1, 19)
  proc_df$entry_date_tm <- stri_sub(proc_df$entry_date_tm, 1, 19)
  proc_df
}
