#' Creates v_animal_age_location_sex_species as a view in animal database if
#' it does not already exists.
#' @param conn database connection object
#' @import RODBC
#' @import stringi
#' @export
create_age_location_sex_species_view <- function(conn) {
  if (!is_database_view(conn, "v_animal_age_location_sex_species", "dbo")) {
    sql_txt <- stri_c(
      "CREATE VIEW dbo.v_animal_age_location_sex_species as
      -- 08-06-2017 R. Mark Sharp
      -- Adapted from v_animal_by_day2200a by srr
      SELECT sc.target_date, cdm.id, cdm.arc_species_code, cdm.sex,
      l.location , DATEDIFF(DAY, CAST(cdm.birth_date AS DATE),
      sc.target_date) AS age_in_days
      FROM (SELECT m.id, m.sex, m.birth_date, cd.arc_species_code
      FROM dbo.current_data cd
      INNER JOIN master m ON m.id = cd.id) AS cdm
      INNER JOIN dbo.acq_disp ad ON ad.id = cdm.id
      INNER JOIN dbo.sm_cal sc ON sc.target_date_2200
      BETWEEN  ad.acq_date_tm AND ISNULL(ad.disp_date_tm,
      DATEADD(HOUR, 22, CAST(CAST(getdate() AS date) AS DATETIME)))
      INNER JOIN dbo.location l ON l.id = cdm.id AND target_date_2200
      BETWEEN l.move_date_tm AND ISNULL(l.exit_date_tm,
      DATEADD(HOUR, 22, CAST(CAST(getdate() AS date) AS DATETIME))) ")
    sqlQuery(conn, sql_txt)
  }
}


