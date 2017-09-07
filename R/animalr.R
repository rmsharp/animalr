#' Begins a transaction
#'
#' @param conn database connection object
#' @import RODBC
#' @export
begin_tran <- function(conn) {
  sqlQuery(conn, "BEGIN TRANSACTION")
}
#' Rollsback a transaction, returns FALSE if there is not a
#' transaction to rollback otherwise TRUE'
#'
#' @param conn database connection object
#' @import RODBC
#' @export
rollback_tran <- function(conn) {
  status <- TRUE
  trancount <- sqlQuery(conn, "select @@trancount")
  if (trancount <= 0)
    status <- FALSE
  sqlQuery(conn, "rollback")
  status
}
#' Commits a database transaction
#'
#' @param conn database connection object
#' @import RODBC
#' @export
commit_tran <- function(conn) {
  sqlQuery(conn, "COMMIT TRANSACTION")
}
#' Get TRANSACTION count
#'
#' @param conn database connection object
#' @import RODBC
#' @export
get_tran_count <- function(conn) {
  sqlQuery(conn, "select @@trancount")
}
#' Drops Xhalfaversary table and returns TRUE if the database table
#' \code{animal.dbl.Xhalfaversary} was successfully drop
#'
#' @param conn database connnection object
#' @export
drop_Xhalfaversary <- function(conn) {
  delete_database_tbl(conn, "animal.dbl.Xhalfaversary")
}
#' Create the database table \code{animal.dbl.Xhalfaversary}
#'
#' @param conn database connection object
#' @import RODBC
#' @import stringi
#' @export
create_Xhalfaversary <- function(conn) {
  sqlQuery(conn, stri_c(
    "CREATE TABLE animal.dbo.Xhalfaversary ( ",
    "id varchar(6), ",
    "halfaversary date ",
    ")"))
}
#' Inserts animal Ids of all primates with each half birth day date from birth
#' until death or the current date into the database table
#' \code{animal.dbl.Xhalfaversary}.
#'
#' @param conn database connnection object
#'
#' @import RODBC
#' @import stringi
#' @export
Xhalfaversary_insert <-  function(conn) {
  sqlQuery(conn, stri_c(
    "WITH cte1 AS ( ",
    #  -- anchor query
    "  SELECT m.id, m.birth_date AS halfaversary,
        isnull(death_date, getdate()) AS dod
      FROM master AS m
      INNER JOIN valid_species vs ON vs.species_code = m.species
      INNER JOIN arc_valid_species_codes avs
        ON avs.arc_species_code = vs.arc_species_code
        AND avs.primate = 'Y'
      WHERE m.birth_date IS NOT NULL ",

    #  -- recurision
    "  UNION ALL
      SELECT id, DATEADD(mm, 6, c.halfaversary) AS halfaversary, dod
      FROM cte1 AS c
      WHERE halfaversary <= c.dod)
    insert into Xhalfaversary
    SELECT c.id, CAST(c.halfaversary AS DATE) AS halfaversary
    FROM cte1 AS c
    ORDER BY id asc, halfaversary asc
    OPTION (MAXRECURSION 0) "))
}
#' Fill the database table \code{animal.dbl.Xhalfaversary}
#'
#' @param conn database connnection object
#' @export
fill_halfaversary_tbl <- function(conn) {
  drop_Xhalfaversary(conn)
  create_Xhalfaversary(conn)
  Xhalfaversary_insert(conn)
}
#' Get dataframe with maintenance IACUC protocols including
#' \code{arc_num_seq}, \code{arc_num_genus}, \code{working_iacuc},
#' and \code{iacuc_type}.
#'
#' Several IACUCs are not research protocols and are designated as maintenance
#' protocols.
#'
#' @param conn database connection object
#' @import RODBC
#' @import stringi
#' @export
get_maintenance_iacuc <- function(conn) {
  sql_txt <- stri_c(
    "select arc_num_seq, arc_num_genus, working_iacuc, iacuc_type
    from valid_colony_maintenance_iacuc")
  m_iacuc_df <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
  rbind(m_iacuc_df, data.frame(arc_num_seq = 273,
                               arc_num_genus = "PC",
                               working_iacuc = "273PC",
                               iacuc_type = "M"))
}
#' Get location from \code{animal.dbo.location} for each animal on the date
#' specified.
#'
#' @param conn database connection object
#' @param id_date_df dataframe with an \code{id} column and with a
#' \code{loc_date} column (in POSIXct format).
#'
#' @import rmsutilityr
#' @import RODBC
#' @import stringi
#' @export
get_location_on_date <- function(conn, id_date_df) {
  loc_on_date_df <- data.frame()
  ids <- id_date_df$id
  loc_date <- format(id_date_df$loc_date, "%m/%d/%Y")
  for (i in seq_along(ids)) {
    sql_txt <- stri_c(
      "select l.id, '", loc_date[i],
      "' as loc_date, ", id_date_df$location[i],
      " as old_loc, l.location as new_loc
      from location l
      where cast(l.move_date_tm as date) <= '", loc_date[i],
      "' and cast(isnull(l.exit_date_tm, getdate()) as date) >= '",
      loc_date[i], "' and l.id = '", ids[i], "'")
    result <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
    if (nrow(result) >= 1) {
      loc_on_date_df <- rbind(loc_on_date_df, result)
    }
  }
  loc_on_date_df$id <- blank_fill_ids(loc_on_date_df$id)
  loc_on_date_df
}
#' Get IACUCs active for an animal on a specified date.
#'
#' @param conn database connection object
#' @param id character vector with animal Ids
#' @param ob_date date object
#' @import RODBC
#' @import stringi
#' @export
get_id_active_IACUC <- function(conn, id, ob_date) {
  iacuc_df <- data.frame()
  if (length(id) == length(ob_date) | length(ob_date) == 1) {
    if (length(ob_date) == 1) {
      id_str <- vector2string(blank_fill_ids(id), SS = "', '")
      sql_txt <- stri_c(
        "select aaa.id, aaa.working_iacuc
        from arc_animal_assignments aaa
        where aaa.id in ('", id_str, "')
          and cast(aaa.start_date as date) <= '",
            format(ob_date, "%m/%d/%Y"), "'
          and cast(isnull(aaa.end_date, getdate()) as date) >= '",
            format(ob_date, "%m/%d/%Y"), "'")
      iacuc_df <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
    } else {
      for (i in seq_along(id)) {
        sql_txt <- stri_c(
          "select aaa.id, aaa.working_iacuc
          from arc_animal_assignments aaa
          where aaa.id = '", id[i], "'
          and cast(aaa.start_date as date) <= '",
          format(ob_date[i], "%m/%d/%Y"), "'
          and cast(isnull(aaa.end_date, getdate()) as date) >= '",
          format(ob_date[i], "%m/%d/%Y"), "'")
        results <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
        iacuc_df <- rbind(iacuc_df, results)
      }
    }
  } else {
    stop(stri_c("id and ob_date must either be of the same length or
                ob_date must be of length 1."))
  }
  iacuc_df$id <- blank_fill_ids(iacuc_df$id)
  iacuc_df
}
#' Get charge_account table contents
#'
#' @param conn database connection object
#' @import RODBC
#' @import stringi
#' @export
get_charge_account_table <- function(conn) {
  charge_id_df <- sqlQuery(conn, "select * from animal.dbo.charge_account",
                           stringsAsFactors = FALSE)
  charge_id_df$timestamp <- NULL
  charge_id_df
}
#' Get clinical admissions active for an animal on a specified date.
#'
#' @param conn database connection object
#' @param id character vector with animal Ids
#' @param ob_date date object
#' @import RODBC
#' @import stringi
#' @export
get_id_clinical_admit <- function(conn, id, ob_date) {
  admit_df <- data.frame()
  if (length(id) == length(ob_date) | length(ob_date) == 1) {
    if (length(ob_date) == 1) {
      id_str <- vector2string(blank_fill_ids(id), SS = "', '")
      sql_txt <- stri_c(
        "select c.id, c.admit_id, c.charge_id, c.admit_code,
        cast(c.admit_date_tm as date) as admit_date,
        cast(isnull(c.release_date_tm, getdate()) as date) as release_date,
        c.pdx_group, c.pdx, c.admit_complaint, c.resolution, c.vet_name,
        c.lab_tests, c.histo_tests, c.object_id, c.user_name,
        c.entry_date_tm
        from clinic c
        where c.id in ('", id_str, "')
        and cast(c.admit_date_tm as date) <= '",
        format(ob_date, "%m/%d/%Y"), "'
        and cast(isnull(c.release_date_tm, getdate()) as date) >= '",
        format(ob_date, "%m/%d/%Y"), "'")
      admit_df <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
    } else {
      for (i in seq_along(id)) {
        sql_txt <- stri_c(
          "select c.id, c.admit_id, c.charge_id, c.admit_code,
            cast(c.admit_date_tm as date) as admit_date,
            cast(isnull(c.release_date_tm, getdate()) as date) as release_date,
            c.pdx_group, c.pdx, c.admit_complaint, c.resolution, c.vet_name,
            c.lab_tests, c.histo_tests, c.object_id, c.user_name,
            c.entry_date_tm
          from clinic c
          where c.id = '", id[i], "'
            and cast(c.admit_date_tm as date) <= '",
            format(ob_date[i], "%m/%d/%Y"), "'
            and cast(isnull(c.release_date_tm, getdate()) as date) >= '",
            format(ob_date[i], "%m/%d/%Y"), "'")
        results <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
        admit_df <- rbind(admit_df, results)
      }
    }
  } else {
    stop(stri_c("id and ob_date must either be of the same length or
                ob_date must be of length 1."))
  }
  admit_df$id <- blank_fill_ids(admit_df$id)
  admit_df
  }
#' Get the maximum age within a species for a specific date range
#'
#' @param conn database connection object
#' @param arc_species_code optional character vector of length 1 having the two
#' character arc_species_code defined by the IACUC. Defaults to "PC".
#' @param start_date character vector of length 1 having the first date
#' to be included in the time frame of interest.
#' Should be formatted as "\%m-\%d-\%Y".
#' @param end_date optional character vector of length 1 having the last date to be
#' included in the time frame of interest. Defaults to current date.
#' Should be formatted as "\%m-\%d-\%Y".
#' @import anytime
#' @import RODBC
#' @import stringi
#' @export
get_max_age <- function(conn, arc_species_code,
                        start_date,
                        end_date = strftime(Sys.Date(), format = "%m-%d-%Y")) {
  if (missing(start_date)) {
    start_date <- strftime(Sys.Date(), format = "%m-%d-%Y")
  } else {
    start_date <- strftime(start_date, format = "%m-%d-%Y")
  }
  end_date <- strftime(anytime(end_date), format = "%m-%d-%Y")
  max_disp_age <- sqlQuery(conn, stri_c(
    "select max(datediff(year, m.birth_date, m.death_date))
    from master m
    inner join valid_species vs on m.species = vs.species_code
      and vs.arc_species_code = '", arc_species_code, "'
      and m.birth_date is not NULL
      and m.death_date is not NULL
    where m.death_date <= '", end_date, "'
    and m.death_date >= '", start_date, "'"),
    stringsAsFactors = FALSE)
  max_current_age <- sqlQuery(conn, stri_c(
    "select max(datediff(year, m.birth_date, '", end_date, "'))
    from master m
    inner join valid_species vs on m.species = vs.species_code
      and vs.arc_species_code = '", arc_species_code, "'
    where m.death_date is NULL"), stringsAsFactors = FALSE)
 max(c(max_current_age[1,1], max_disp_age[1, 1]))
}
#' Get the maximum age within a species for a specific date range
#'
#' @param conn database connection object
#' @param arc_species_code optional character vector of length 1 having the two
#' character arc_species_code defined by the IACUC. Defaults to "PC".
#' @param start_date POSIXct vector of length 1 having the first date
#' to be included in the time frame of interest.
#' Should be formatted as "\%m-\%d-\%Y".
#' @param i integer used as the offset for setting the base_date being used
#' in the database query.
#' @import anytime
#' @import lubridate
#' @import RODBC
#' @import stringi
#' @export
get_age_dist <- function(conn, arc_species_code, start_date, i) {
  base_date <- start_date + days(i)
  base_date_str <- stri_c(base_date)
  query_str <-
    stri_c(
      "select m.id, m.birth_date, m.sex
      from master m
      inner join acq_disp ad on m.id = ad.id
        and (ad.disp_code not in (98, 99)
          or ad.disp_code is NULL)
        and (ad.disp_date_tm >= '", base_date_str, "' or
          ad.disp_date_tm is NULL)
      left outer join valid_disp_codes vdc on ad.disp_code = vdc.disp_code
        and vdc.death_code = 'Y'
      inner join valid_species vs on m.species = vs.species_code
        and vs.arc_species_code = '", arc_species_code, "'
      where m.sex in ('M', 'F')
        and m.birth_date <= '", base_date_str, "'
        and m.birth_date is not NULL ")
  age_dist <- sqlQuery(conn, query_str, stringsAsFactors = FALSE)
  # Add an age column
  #age_dist$age <- as.numeric(as.duration(base_date - age_dist$birth_date)) /
  #  (60 * 60 * 24 * 365.25) # convert from duration of seconds to years
  age_dist$age <- lubridate::interval(start = age_dist$birth_date,
                           end = base_date) /
    lubridate::duration(num = 1, units = "years")
  age_dist
}
#' Get number of days females were exposed to fertile males.
#'
#' @return dataframe with \code{id} (female Id),
#' \code{days} (number of days exposed),
#' \code{current_location}, \code{min_date} (first date of exposure),
#' and \code{max_date} (last date of exposure).
#'
#' Males having an attribute of "vasectomized" are not included.
#'
#' @param conn database connection object
#' @param arc_species_code optional character vector of length 1 having the two
#' character arc_species_code defined by the IACUC. Defaults to "PC".
#' @param min_date optional character vector of length 1 having the first date to be
#' included in the time frame of interest. Defaults to "01-01-2014".
#' Should be formatted as "\%m-\%d-\%Y".
#' @param max_date optional character vector of length 1 having the last date to be
#' included in the time frame of interest. Defaults to current date.
#' Should be formatted as "\%m-\%d-\%Y".
#' @param male_age integer value in months that indicates the minimum age the
#' male must be on each day with a female.
#' @param female_age integer value in months that indicates the minumum age
#' the female must be on each day with a male.
#' @param gestation_length integer value indicating the gestation length in
#' days for the species. Animals exposed to the male less than the gestational
#' length are not included. This can be set to 0 to include all females exposed
#' to fertile males for any length of time. Defaults to 183.
#' @import anytime
#' @import RODBC
#' @import stringi
#' @export
get_female_exposed_to_male_days <-
  function(conn, arc_species_code = "PC", min_date = "01-01-2014",
           max_date = strftime(Sys.Date(), format = "%m-%d-%Y"),
           male_age = 48, female_age = 42, gestation_length = 183) {
    min_date <- strftime(anytime(min_date), format = "%m-%d-%Y")
    max_date <- strftime(anytime(max_date), format = "%m-%d-%Y")
    sql_txt <- stri_c(
      "select fd.current_location, fd.id,
       convert(char(12), f.birth_date, 110) as birth_date,
       COUNT(fd.target_date) as days,
        MIN(fd.target_date) AS min_date, MAX(fd.target_date) AS max_date
      from master s -- sire
      INNER JOIN v_animal_by_day md -- Male animal Day
        ON md.id = s.id AND s.sex = 'M'
        AND md.arc_species_code = 'PC'
        AND s.birth_date > DATEADD(MONTH, ", -male_age, ", md.target_date)
      INNER JOIN dbo.v_animal_by_day fd ", #-- Female animal Day
      " ON fd.target_date = md.target_date
        AND fd.location = md.location
        AND fd.target_date >= '", min_date, "'
      INNER JOIN master f ", #-- female
      "  ON f.id = fd.id
        AND f.sex = 'F'
        AND fd.arc_species_code = '", arc_species_code, "'
        AND f.birth_date < DATEADD(MONTH, ", -female_age, ", fd.target_date)
      WHERE fd.target_date >= '", min_date, "'
        and not exists ( select 1 FROM offspring o where o.id = fd.id
        and datediff(day, o.offspring_birth_date, fd.target_date) <= ",
          gestation_length, ") ", #-- has had time to deliver since conception
      " AND s.id NOT IN (
          SELECT id
          FROM dbo.attributes a
          WHERE a.attribute = 'vasectomized')
      GROUP BY fd.current_location, fd.id, convert(char(12), f.birth_date, 110)
      ORDER BY fd.current_location, fd.id,
        convert(char(12), f.birth_date, 110)")
    sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
}
#' Get vasectomized males of a specified arc_species_code that died after a
#' specified date.
#'
#' @param conn database connection object
#' @param arc_species_code character vector of length 1 having two character
#' species code
#' @param died_after_date optional character vector of length 1 representing
#' last date of interest. The male must have died after this date or it is
#' not returned.
#' @import RODBC
#' @import stringi
#' @export
get_vasectomized_males <-
  function(conn, arc_species_code,
           died_after_date = strftime(Sys.Date(), format = "%m-%d-%Y")) {
    sql_txt <- stri_c(
      "select a.id
      from attributes a
      inner join master m on a.id = m.id
      and (m.death_date is NULL or
      m.death_date > '1-1-2010')
      inner join valid_species vs on m.species = vs.species_code
      and vs.arc_species_code = '", arc_species_code, "'
      where a.attribute = 'vasectomized'")
    sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
}
#' Returns dataframe with colony for each animal
#' in character vector id for a specified date in colony_date column of the
#' provided dataframe id_df.
#' Returns NA in vector if not available.
#'
#' Assumes access to master database table in animal database.
#' @param conn database connection
#' @param id_df dataframe with animal Ids in id column
#' @import RODBC
#' @import rmsutilityr
#' @import stringi
#' @export
add_colony <- function(conn, id_df) {
  id_df$id <- blank_fill_ids(id_df$id)
  id_date_df <- id_df[!(is.na(id_df$id) | is.na(id_df$colony_date)), ]
  id_date_df <- id_date_df[!duplicated(id_date_df), ]
  colony_df <- data.frame()
  for (i in seq_along(id_date_df$id)) {
    sql_txt <- stri_c(
      "SELECT id, colony, '", id_date_df$colony_date[i], "' as colony_date
       from colony
       where id = '", id_date_df$id[i], "'
         AND cast(start_date_tm as date) <= '", id_date_df$colony_date[i], "'
         AND cast(isnull(end_date_tm, getdate()) as date) >= '",
      id_date_df$colony_date[i], "' ")
     tmp_colony_df <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
     if (class(tmp_colony_df) != "data.frame")
       stop(stri_c("Attempt to access animal colony failed; status: ",
                   tmp_colony_df))
     colony_df <- rbind(colony_df, tmp_colony_df)
  }
  colony_df$id <- blank_fill_ids(colony_df$id)
  merge(id_df, colony_df, by = c("id", "colony_date"), sort = TRUE,
                                 all.x = TRUE)
}
#' Returns dataframe with sex for each animal
#' in character vector id of the provided dataframe id_df.
#' Returns NA in vector if not available.
#'
#' Assumes access to master database table in animal database.
#' @param conn database connection
#' @param id_df dataframe with animal Ids in id column
#' @import rmsutilityr
#' @import RODBC
#' @export
add_sex <- function(conn, id_df) {
  ids_str <- vector2string(unique(blank_fill_ids(id_df$id)), SS = "','")
  sex_df <- sqlQuery(conn, stri_c(
    "SELECT id, sex from master where id in ('", ids_str, "')"))
  merge(id_df, sex_df, by = 'id', sort = TRUE, all = TRUE)
}
#' Adds arc_species_code for each animal
#' in character vector id of the provided dataframe id_df.
#'
#' Returns NA in vector if not available.
#'
#' Assumes access to master database table in animal database.
#' @param conn database connection
#' @param id_df dataframe with animal Ids in id column
#' @import RODBC
#' @import rmsutilityr
#' @import stringi
#' @export
add_arc_species_code <- function(conn, id_df) {
  ids_str <- vector2string(unique(blank_fill_ids(id_df$id)), SS = "','")
  arc_species_code_df <- sqlQuery(conn, stri_c(
    "SELECT id, arc_species_code from current_data where id in ('", ids_str, "')"))
  merge(id_df, arc_species_code_df, by = 'id', sort = TRUE, all = TRUE)
}
#' Returns dataframe with and additional column (common_name) corresponding
#' to each animal in the id column of the provided dataframe.
#'
#' Returns NA in vector if not available.
#'
#' Assumes access to master database table in animal database.
#' @param conn database connection
#' @param id_df dataframe with animal Ids in id column
#' @import RODBC
#' @import rmsutilityr
#' @import stringi
#' @export
add_common_name <- function(conn, id_df) {
  ids <- blank_fill_ids(unique(id_df$id))
  ids <- ids[!is.na(ids)]
  if (length(ids) > 0) {
    ids_str <- vector2string(ids, SS = "', '")
    common_name_df <- sqlQuery(conn, stri_c(
      "select cd.id, avsc.common_name
      from arc_valid_species_codes avsc
      inner join current_data cd on avsc.arc_species_code = cd.arc_species_code
      where cd.id in ('", ids_str, "')"), stringsAsFactors = FALSE)
    common_name_df$id <- blank_fill_ids(common_name_df$id)
    id_df <- merge(id_df, common_name_df, by = 'id', sort = TRUE, all.x = TRUE)
    id_df$id <- blank_fill_ids(id_df$id)
    id_df
  }
}
#' Returns a data frame containing four columns: id, sire_id,
#' dam_id, sex when given a character vector of one or more Ids in ids.
#'
#' Reads the data from the database.
#'
#' @param conn database connection object
#' @param ids is the vector of Ids for which their relationships are being
#' sought.
#' @import RODBC
#' @import rmsutilityr
#' @import stringi
#' @export
get_relatives <- function(conn, ids) {
  id_str <- vector2string(unique(blank_fill_ids(ids)), SS = "','")
  relatives_df <- sqlQuery(conn, stri_c(
    "select id, sire_id, dam_id, sex, birth_date ",
    "from master ",
    "where id in ('", id_str, "') or ",
    "  sire_id in ('", id_str, "') or ",
    "  dam_id in ('", id_str, "') ",
    "order by id"))

  names(relatives_df) <- c('id', 'sire_id', 'dam_id', 'sex', "birth_date")
  relatives_df$id <- blank_fill_ids(relatives_df$id)
  relatives_df$sire_id <- blank_fill_ids(relatives_df$sire_id)
  relatives_df$dam_id <- blank_fill_ids(relatives_df$dam_id)
  relatives_df
}
#' Returns dataframe with all direct ancestors for Ids provided.
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
#' Returns data frame with specified siblings for Ids provided.
#'
#' Takes a database connection and a
#' character vector of animal Ids and retrieves the siblings for each
#' id. All returned rows represent a unique individual. The SQL gets all
#' siblings of the type specified.
#'
#'
#' @param conn database connection
#' @param ids character vector of individual ids
#' @param type character element having 'all', 'full', 'half', 'maternal' or
#' 'paternal'.
#' @import RODBC
#' @import rmsutilityr
#' @import stringi
#' @export
get_siblings <- function(conn, ids, type = 'all') {
  ids_str <- vector2string(unique(blank_fill_ids(ids)), SS = "','")
  if (type == 'all') {
    join_str <- "  ON (m1.sire_id = m2.sire_id OR m1.dam_id = m2.dam_id) "
  } else if (type == 'full') {
    join_str <- "  ON ((m1.sire_id = m2.sire_id AND m1.dam_id = m2.dam_id)
                  or (m1.dam_id = m2.dam_id
                      AND CAST(m1.birth_date as date) =
                          CAST(m2.birth_date as date))) "
  } else if (type == 'half') {
    join_str <-
      stri_c("  ON ((m1.sire_id = m2.sire_id AND m1.dam_id != m2.dam_id )",
            "    OR (m1.sire_id != m2.sire_id AND m1.dam_id = m2.dam_id ))")
  } else if (type == 'maternal') {
    join_str <- "  ON (m1.sire_id != m2.sire_id AND m1.dam_id = m2.dam_id) "
  } else if (type == 'paternal') {
    join_str <- "  ON (m1.sire_id = m2.sire_id AND m1.dam_id != m2.dam_id) "
  } else {
    stop("invalid sibling type in get_siblings()")
  }
  siblings <- sqlQuery(conn, stri_c(
    "SELECT distinct m1.id, m1.dam_id, m1.sire_id, m1.sex,
    m1.birth_date
    FROM master AS m2
    INNER JOIN master as m1 ", join_str,
    " WHERE m1.id <= m2.id and m2.id in ('", ids_str, "')
    ORDER BY m1.dam_id, m1.sire_id, m1.birth_date"), as.is = TRUE)
  siblings
}
#' Returns the number of cagemates each animal has for each day between and
#' including start and end dates.
#'
#' @param conn database connection
#' @param ids character vector with an animal Id in each cell
#' @param start character string representation of start date in mm-dd-yyyy
#' format
#' @param end character string representation of end date in mm-dd-yyyy
#' format
#' @import RODBC
#' @import rmsutilityr
#' @import stringi
#' @export
get_number_of_cagemates <- function(conn, ids, start, end) {
  ids_str <- vector2string(unique(blank_fill_ids(ids)), SS = "','")
  sql_txt <- stri_c(
    "select t.id , t.midnight_location,
    t.target_date, count(c.id) as cagemates
    from daily_demo t
    inner join daily_demo c on t.target_date = c.target_date
    and t.midnight_location = c.midnight_location
    and t.id <> c.id
    where t.id in ('", ids_str, "')
    and t.target_date between '", start, "' and '", end, "'
    group by t.id, t.midnight_location, t.target_date")

  number_of_cagemates <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
  number_of_cagemates
}
#' Returns the cagemates and total number of days together for each animal
#' between and including start and end dates.
#'
#' @param conn database connection
#' @param ids character vector with an animal Id in each cell
#' @param start character string representation of start date in mm-dd-yyyy
#' format
#' @param end character string representation of end date in mm-dd-yyyy
#' format
#' @import RODBC
#' @import rmsutilityr
#' @import stringi
#' @export
get_cagemates <- function(conn, ids, start, end) {
  ids_str <- vector2string(unique(blank_fill_ids(ids)), SS = "','")
  sql_txt <- stri_c(
    "select t.id , c.id as cagemates, count(t.target_date) as count
    from daily_demo t
    inner join daily_demo c on t.target_date = c.target_date
    and t.midnight_location = c.midnight_location
    and t.id <> c.id
    where t.id in ('", ids_str, "')
    and t.target_date between '", start, "' and '", end, "'
    group by t.id, c.id
    order by t.id, c.id")

  cagemates <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
  cagemates
}

#' Returns a dataframe containing the id, sex, and birth date of animals born
#' between to dates inclusive and assigned to a specific IACUC protocol.
#'
#' @param conn database connection object
#' @param working_iacuc IACUC protocol
#' @param start_date character vector of length 1 in "mm-dd-YYYY" format
#' @param end_date character vector of length 1 in "mm-dd-YYYY" format
#'
#' @import RODBC
#' @import stringi
#' @export
get_births_in_iacuc <- function(conn, working_iacuc, start_date,
                                end_date) {
  sql_txt <- stri_c(
    "select ad.id, m.sex, m.birth_date
    from master m
    inner join acq_disp ad on m.id = ad.id
      and ad.acq_code <> 3 -- stillbirth or abortion
    inner join valid_acq_codes vac on ad.acq_code = vac.acq_code
      and vac.birth_code = 'Y'
    inner join arc_animal_assignments aaa on m.id = aaa.id
    and aaa.working_iacuc = '", working_iacuc, "'
    and aaa.start_date >= '", start_date, "'
    and aaa.start_date <= '", end_date, "'
    where cast(m.birth_date as date) >= '", start_date, "'
    and cast(m.birth_date as date) <= '", end_date, "'")
  births_in_iacuc <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
  births_in_iacuc
}
#' Returns dataframe with the animals with a status of assigned, hold, or
#'  to an IACUC on a particular
#' date.
#'
#' @param conn database connection object
#' @param working_iacuc IACUC protocol
#' @param check_date character string representation of date in mm-dd-yyyy
#'
#' @import RODBC
#' @import stringi
#' @export
get_assignments_to_iacuc <- function(conn, working_iacuc, check_date) {
  ## ad.acq_code == 3 is a stillbirth OR abortion and should not be considered
  sql_txt <- stri_c(
    "select ad.id, m.sex, m.birth_date
    from master m
    inner join acq_disp ad on m.id = ad.id
      and ad.acq_code <> 3
      and cast(ad.acq_date_tm as date) <= '", check_date, "'
      and cast(isnull(ad.disp_date_tm, getdate()) as date) >= '", check_date, "'
    inner join arc_animal_assignments aaa on m.id = aaa.id
      and aaa.working_iacuc = '", working_iacuc, "'
      and cast(aaa.start_date as date) <= '", check_date, "'
      and aaa.status in ('A', 'S', 'O')")
  sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
}
#' Returns dataframe with the disposition code, disposition description, and
#' count of animals with the disposition code within start_date and end_date
#' inclusive for a specific working_iacuc.
#'
#' @param conn database connection object
#' @param working_iacuc IACUC protocol
#' @param start_date character vector of length 1 in "mm-dd-YYYY" format
#' @param end_date character vector of length 1 in "mm-dd-YYYY" format
#'
#' @import RODBC
#' @import stringi
#' @export
get_dispositions_for_iacuc <- function(conn, working_iacuc, start_date,
                                       end_date) {
  ## ad.acq_code == 3 is a stillbirth OR abortion and should not be considered
  sql_txt <- stri_c(
    "SELECT ad.disp_code, vdc.description, count(ad.disp_code)
FROM master m
    INNER JOIN acq_disp ad ON m.id = ad.id
    AND ad.acq_code <> 3
    AND cast(ad.disp_date_tm as date) >= '", start_date, "'
    AND cast(ad.disp_date_tm as date) <= '", end_date, "'
    INNER JOIN valid_disp_codes vdc ON ad.disp_code = vdc.disp_code
    AND vdc.death_code = 'Y'
    INNER JOIN arc_animal_assignments aaa ON m.id = aaa.id
    AND aaa.working_iacuc = '", working_iacuc, "'
    AND cast(aaa.start_date as date) <= '", end_date, "'
    AND isnull(aaa.end_date, getdate()) >= '", start_date, "'
    AND aaa.status IN ('A', 'S', 'O')
    GROUP by ad.disp_code, vdc.description
    ORDER by ad.disp_code
    ")
  disp_df <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
  names(disp_df) <- c("Code", "Description", "Count")
  disp_df
}
#' Returns the average number of cagemates each animal has for days
#' between and including start and end dates.
#'
#' @param conn database connection
#' @param ids character vector with an animal Id in each cell
#' @param start character string representation of start date in mm-dd-yyyy
#' format
#' @param end character string representation of end date in mm-dd-yyyy
#' format
#' @import magrittr
#' @import RODBC
#' @import rmsutilityr
#' @import stringi
#' @export
get_average_number_of_cagemates <- function(conn, ids, start, end) {
  ids <- blank_fill_ids(ids)
  ids_str <- vector2string(ids)
  sql_txt <- stri_c(
    "select t.id , t.midnight_location,
    t.target_date, count(c.id) as cagemates
    from daily_demo t
    inner join daily_demo c on t.target_date = c.target_date
    and t.midnight_location = c.midnight_location
    and t.id <> c.id
    where t.id in ('", ids_str, "')
    and t.target_date between '", start, "' and '", end, "'
    group by t.id, t.midnight_location, t.target_date")

  ncm <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
  avg_cagemates <- ncm %>% dplyr::group_by(id) %>%
    dplyr::summarise(avg_cagemates = mean(cagemates))
  avg_cagemates
}
#' Returns a character vector of table names for the database connection conn
#' where the names do not include specified strings.
#'
#' @param conn database connection
#' @param expunge_list character vector with regexpression patterns to be
#' expunged (removed) from the list of tables. The expunge_list has no
#' built in expressions.
#' @param owner character vector of length 1 having the database schema owner.
#' Defaults to "DBO".
#' @import RODBC
#' @import rmsutilityr
#' @export
get_table_names <- function(conn, expunge_list = character(0), owner = "dbo") {
  table_names <- sqlQuery(conn, "exec sp_tables", stringsAsFactors = FALSE)
  table_names <- table_names[table_names$TABLE_OWNER == owner &
                               table_names$TABLE_TYPE == 'TABLE', "TABLE_NAME"]
  remove_strings(tolower(table_names), expunge_list)
}
#' Returns a character vector of view names for the database connection conn
#' where the names do not include specified strings.
#'
#' @param conn database connection
#' @param expunge_list character vector with regexpression patterns to be
#' expunged (removed) from the list of views. The expunge_list has no
#' built in expressions.
#' @param owner character vector of length 1 having the database schema owner.
#' Defaults to "DBO".
#' @import RODBC
#' @export
get_view_names <- function(conn, expunge_list = character(0), owner = "dbo") {
  view_names <- sqlQuery(conn, "exec sp_tables", stringsAsFactors = FALSE)
  view_names <- view_names[view_names$TABLE_OWNER == owner &
                             view_names$TABLE_TYPE == 'VIEW', "TABLE_NAME"]
  remove_strings(tolower(view_names), expunge_list)
}
#' Returns a character vector of tables names for the database connection conn
#' where the tables contain a column matching a specific regular expression.
#'
#' @param conn database connection object
#' @param pattern charcter vector of length 1 having the regular expression
#' used to match columns.
#' @param expunge_list character vector with regexpression patterns to be
#' expunged (removed) from the list of tables. The expunge_list has no
#' built in expressions.
#' @param owner character vector of length 1 having the database schema owner.
#' Defaults to "DBO".
#' @import RODBC
#' @import stringi
#' @export
get_table_w_col <- function(conn, pattern, expunge_list = character(0), owner) {
  table_names <- get_table_names(conn, expunge_list, owner)
    result <- sapply(table_names, FUN = function(table_name) {
      #cat("table_name: ", table_name, "\n")
      any(stri_detect_regex(tolower(get_column_names(conn,table_name, owner)),
                            pattern = pattern))},
      USE.NAMES = FALSE)
    table_names[result]
}
#' Returns a character vector of table names for the database connection conn
#' where the names do not include specified strings, each table has a column
#' with the name 'id', and the master table is listed first.
#'
#' @param conn database connection
#' @param expunge_list character vector with regexpression patterns to be
#' expunged (removed) from the list of tables. The expunge_list has no
#' built in expressions.
#' @param owner character vector of length 1 having the database schema owner.
#' Defaults to "DBO".
#' @export
get_table_names_w_id_col <- function(conn, expunge_list = character(0),
                                     owner = "DBO") {
  table_names <- get_table_names(conn, expunge_list, owner = owner)
  table_names <- table_names[has_id_column(conn, table_names)]
  table_names <- c("acq_disp", remove_strings(table_names, "^acq_disp$"))
  table_names <- c("master", remove_strings(table_names, "^master$"))
  table_names
}
#' Get tables and columns for a database connection.
#'
#' @param conn database connection object
#' @param expunge_list character vector with regexpression patterns to be
#' expunged (removed) from the list of tables. The expunge_list has no
#' built in expressions.
#' @param owner character vector of length 1 having the database schema owner.
#' Defaults to "DBO".
#' @export
get_tables_and_columns <- function(conn, expunge_list = character(0),
                                   owner = "DBO") {
  table_names <- get_table_names(conn, expunge_list, owner = owner)
  table_col_df <- data.frame()
  for (table in table_names) {
    col_names <- get_column_names(conn, table, owner)
    table_col_df <- rbind(table_col_df,
                          data.frame(table = rep(table, length(col_names)),
                                     column = col_names))
  }
  table_col_df
}
#' Get tables, views, and columns for a database connection.
#'
#' @param conn database connection object
#' @param expunge_list character vector with regexpression patterns to be
#' expunged (removed) from the list of tables. The expunge_list has no
#' built in expressions.
#' @param owner character vector of length 1 having the database schema owner.
#' Defaults to "DBO".
#' @export
get_tables_views_and_columns <- function(conn, expunge_list = character(0),
                                         owner = "DBO") {
  table_names <- get_table_names(conn, expunge_list, owner = owner)
  view_names <- get_view_names(conn, expunge_list, owner = owner)
  table_col_df <- data.frame()
  for (table in table_names) {
    col_names <- get_column_names(conn, table, owner)
    table_col_df <- rbind(table_col_df,
                          data.frame(table = rep(table, length(col_names)),
                                     type = rep("TABLE", length(col_names)),
                                     column = col_names))
  }
  for (table in view_names) {
    col_names <- get_column_names(conn, table, owner)
    table_col_df <- rbind(table_col_df,
                          data.frame(table = rep(table, length(col_names)),
                                     type = rep("VIEW", length(col_names)),
                                     column = col_names))
  }
  table_col_df
}

#' Returns a character vector containing all of the column names of for the
#' specified table.
#'
#' @param conn database connection
#' @param table_name character vector with the name of the table
#' @param owner character vector of length 1 having the database schema owner.
#' Defaults to "DBO".
#' @import RODBC
#' @import stringi
#' @export
get_column_names <- function(conn, table_name, owner = "DBO") {
  # cat(stri_c("sqlQuery(conn, stri_c('exec sp_columns ', table_name));
  #            table_name = ", table_name, "\n"))
  sqlQuery(conn, stri_c("SELECT name FROM sys.columns
                        WHERE [object_id] = OBJECT_ID('", owner, ".",
                        table_name, "')"),
           stringsAsFactors = FALSE)$name
}
#' Returns a logical vector indicating whether or not each table contains a
#' column named 'id'.
#' @param conn database connection
#' @param table_names character vector with the names of tables
#' @param owner character vector of length 1 having the database schema owner.
#' Defaults to "DBO".
#' @import stringi
#' @export
has_id_column <- function(conn, table_names, owner = "DBO") {
  sapply(table_names, FUN = function(table_name) {
    # cat("table_name: ", table_name, "\n")
    any(stri_detect_regex(tolower(get_column_names(conn,table_name, owner)),
                          pattern = "^id$"),
        stri_detect_regex(tolower(get_column_names(conn,table_name, owner)),
                          pattern = "^snprc_id$"),
        stri_detect_regex(tolower(get_column_names(conn,table_name, owner)),
                          pattern = "^animal_id$"))},
    USE.NAMES = FALSE)
}
#' Returns a character vector with the column_names vector having had all
#' housekeeping columns removed.
#'
#' @param table_name name of database table
#' @param column_names names of columns within the table
#' @import rmsutilityr
#' @export
remove_housekeeping_columns <- function(table_name, column_names) {
  hk_columns <- c(
    "user_name",
    "entry_date_tm",
    "timestamp",
    "tid",
    "working_iacuc", #composit
    ## removing id from the list so we do not have to keep track of its position
    "^id$")

  column_names <- remove_strings(tolower(column_names), hk_columns)
  if (table_name == 'blood') {
    column_names <- remove_strings(column_names, "fraction_id")
  } else if (table_name == 'accounts' | table_name == 'diet') {
    column_names <- remove_strings(column_names, "end_date")
  } else if (table_name == "location") {
    column_names <- remove_strings(column_names, "exit_date_tm")
  }
  column_names
}
#' Returns integer vector of length one indicating the number of records that
#' exists in the table having an id equal to the value of id.
#' @param conn database connection
#' @param table_name character vector of length one with the name of the table
#' @param ids character vector of length one or more with the animal Ids
#' of interest
#' @import RODBC
#' @import rmsutilityr
#' @import stringi
#' @export
rows_with_id_in_table <- function(conn, table_name, ids) {
  id_str <- get_id_str(conn, table_name)
  if (is.na(id_str)) {
    rows <- 0
  } else {
    ids_str <- vector2string(ids, SS = "', '")
    table_df <- sqlQuery(conn, stri_c(
      "SELECT * from ", table_name, " WHERE ", id_str, " in ('", ids_str, "')"))
    rows <- nrow(table_df)
  }
  rows
}
#' Returns logical vector of length one indication whether or not at least
#' one record exists in the table having an id equal to the value of id.
#'
#' @param conn database connection
#' @param table_name character vector of length one with the name of the table
#' @param ids character vector of length one or more with the animal Ids
#' of interest
#' @export
row_exist <- function(conn, table_name, ids) {
  rows_with_id_in_table(conn, table_name, ids) > 0
}
#' Returns a dataframe with a character vector listing the table names and an
#' integer vector indicating the number of rows for those tables having
#' at least one record with values for the animals of interest.
#'
#' @param conn database connection
#' @param potential_tables character vector with the table names of tables to
#' examine for possible data
#' @param ids character vector with animal Ids
#' @export
get_tables_with_data <- function(conn, potential_tables, ids) {
  tables_with_data <- data.frame(
    table_name = character(0),
    rows = integer(0))
  for (table_name in potential_tables) {
    tables_with_data <- rbind(tables_with_data,
                              data.frame(table_name = table_name,
                                         rows = rows_with_id_in_table(
                                           conn, table_name, ids)))
  }
  tables_with_data
}
#' Returns the proported animal Id(s) that cannot be found in the master table
#' of the animal database.
#'
#' @param conn database connection object
#' @param ids character vector with animal Ids
#' @import rmsutilityr
#' @import RODBC
#' @import stringi
#' @export
is_bad_animal_id <- function(conn, ids) {
  ids <- blank_fill_ids(unique(ids[!is.na(ids)]))
  if (length(ids) == 0)
    return(character(0))
  sql_txt <- stri_c(
    "select id from master where id in ('", vector2string(ids, SS = "', '"),
    "') ")
  real_ids <- sqlQuery(conn, sql_txt, stringsAsFactors = FALSE)
  real_ids <- blank_fill_ids(real_ids$id)
  bad_ids <- ids[!ids %in% real_ids]
  bad_ids
}
#' Returns logical vector indicating if animal ids are in master.
#'
#' @param conn database connection object
#' @param ids character vector with ids
#' @import RODBC
#' @import rmsutilityr
#' @import stringi
#' @export
is_animal_id <- function(conn, ids) {
  ids[!is.na(ids)] <- blank_fill_ids(ids[!is.na(ids)])
  if (length(ids[!is.na(ids)]) == 0)
    return(rep(FALSE, length(ids)))
  is_id <- logical(length(ids))
  for (i in seq_along(ids)) {
    if (is.na(ids[i])) {
      is_id[i] <- FALSE
    } else {
      is_it_there <- sqlQuery(conn, stri_c(
        "select id from master
        where id = '", ids[i], "'"))
      is_id[i] <- ifelse(nrow(is_it_there) > 0, TRUE, FALSE)
    }
  }
  is_id
}
#' Returns logical vector indicating if animal ids represent a primate.
#'
#' @param conn database connection object
#' @param ids character vector with ids
#' @import RODBC
#' @import rmsutilityr
#' @import stringi
#' @export
is_primate_id <- function(conn, ids) {
  ids[!is.na(ids)] <- blank_fill_ids(ids[!is.na(ids)])
  if (length(ids[!is.na(ids)]) == 0)
    return(rep(FALSE, length(ids)))
  is_id <- logical(length(ids))
  for (i in seq_along(ids)) {
    if (is.na(ids[i])) {
      is_id[i] <- FALSE
    } else {
      is_it_there <- sqlQuery(conn, stri_c(
        "select cd.id from current_data cd
        inner join arc_valid_species_codes av on cd.arc_species_code =
            av.arc_species_code
          and av.primate = 'Y'
        where cd.id = '", ids[i], "'"))
      is_id[i] <- ifelse(nrow(is_it_there) > 0, TRUE, FALSE)
    }
  }
  is_id
}
#' Returns logical vector indicating if a location exists.
#'
#' @param conn database connection object
#' @param locations character vector with ids
#' @import RODBC
#' @import stringi
#' @export
is_location <- function(conn, locations) {
  if (length(locations[!is.na(locations)]) == 0)
    return(rep(FALSE, length(locations)))
  is_loc <- logical(length(locations))
  for (i in seq_along(locations)) {
    if (is.na(locations[i])) {
      is_loc[i] <- FALSE
    } else {
      is_it_there <- sqlQuery(conn, stri_c(
        "select location from valid_locations
        where location = ", locations[i]))
      is_loc[i] <- ifelse(nrow(is_it_there) > 0, TRUE, FALSE)
    }
  }
  is_loc
}
#' Returns logical vector indicating if id is animal in master and alive on the
#' provided date.
#'
#' @param conn database connection object
#' @param id_df dataframe with columns id and sample_date where date is a
#' character string with the format mm-dd-yyyy.
#' @param format optional character vector of length one having the date
#' format string to be used in call to \code{is_valid_date()}.
#' @import RODBC
#' @import stringi
#' @export
is_animal_alive <- function(conn, id_df, format = "%Y-%m-%d") {
  id_df$id[!is.na(id_df$id)] <- blank_fill_ids(id_df$id[!is.na(id_df$id)])
  if (length(id_df$id[!is.na(id_df$id)]) == 0)
    return(rep(FALSE, nrow(id_df)))
  alive <- logical(nrow(id_df))
  if (is.null(conn)) {
    conn <- odbcConnect('frogstar-vortex-animal-sa')
    close_it <- TRUE
  }

  for (i in 1:nrow(id_df)) {
    if (is.na(id_df$id[i]) | is.na(id_df$sample_date[i]) |
        !is_valid_date_str(id_df$sample_date[i], format = format)) {
      alive[i] <- FALSE
    } else {
      is_it_there <- sqlQuery(conn, stri_c(
        "select ad.id from acq_disp ad
        where ad.id = '", id_df$id[i], "'
        and '", id_df$sample_date[i], "'
           between ad.acq_date_tm and isnull(ad.disp_date_tm, getdate())
        and ad.acq_code not in (0, 25, 97)"))
      #cat(stri_c("id =", id_df$id[i], "; sample_date = ", id_df$sample_date[i],
      #           "\n"))
      alive[i] <- ifelse(nrow(is_it_there) > 0, TRUE, FALSE)
    }
  }
  alive
}
#' Returns dataframe with audit table data for audit_procedures_entered
#' table of animals for the date range specified.
#'
#' @param conn database connection object
#' @param ranges has the list of date ranges and
#' the animal Ids.
#' @import RODBC
#' @import stringi
#' @export
get_audit_proc_df <- function(conn, ranges) {
  for (i in seq_along(ranges$ids)) {
    sql_txt <- stri_c(
      "select ca.short_description, c.id, p.proc_id,
      p.proc_date_tm,
      p.admit_id, p.proc_narrative,
      p.user_name as user_n, p.entry_date_tm, p.audit_action as action,
      p.audit_date_tm,
      p.audit_user_name as audit_user
      from animal.audit.audit_procedures_entered p
      inner join clinic c on c.admit_id = p.admit_id
      inner join charge_account ca on ca.charge_id = c.charge_id
      where c.id in ('", vector2string(ranges$ids[[i]], SS = "', '"), "')
      and cast(p.proc_date_tm as date) >= '", ranges$dates[[i]][1], "'
      and cast(p.proc_date_tm as date) <= '", ranges$dates[[i]][2], "'
      order by c.id, p.proc_date_tm, entry_date_tm, audit_date_tm ")
    df <- sqlQuery(conn, sql_txt, as.is = TRUE)
    if (i == 1) {
      audit_proc_df <- df
    } else {
      audit_proc_df <- rbind(audit_proc_df, df)
    }
  }
  audit_proc_df$audit_timestamp <- NULL
  audit_proc_df$proc_date_tm <-
    stri_sub(audit_proc_df$proc_date_tm, 1, 19)
  audit_proc_df$entry_date_tm <-
    stri_sub(audit_proc_df$entry_date_tm, 1, 19)
  audit_proc_df$audit_date_tm <-
    stri_sub(audit_proc_df$audit_date_tm, 1, 19)
  audit_proc_df
}
#' Returns dataframe with vitals, date_tm of collection, user name (user_n),
#' and entry_date_tm of animals from arrival to death.

#' @param conn database connection object
#' @param ranges has the list of date ranges prior to going into the BLS4 and
#' the animal Ids. The dates are not used for this query.
#' @import RODBC
#' @import stringi
#' @export
get_vitals_df <- function(conn, ranges) {
  for (i in seq_along(ranges$ids)) {
    sql_txt <- stri_c(
      "select v.id, v.date_tm, v.respiration_rate, v.heart_rate, v.temperature,
      v.user_name as user_n, v.entry_date_tm
      from vitals v
      where v.id in ('", vector2string(ranges$ids[[i]], SS = "', '"), "')
      order by v.id, v.date_tm, v.entry_date_tm")
    df <- sqlQuery(conn, sql_txt, as.is = TRUE)
    if (i == 1) {
      vitals_df <- df
    } else {
      vitals_df <- rbind(vitals_df, df)
    }
  }
  vitals_df$date_tm <- stri_sub(vitals_df$date_tm, 1, 19)
  vitals_df$entry_date_tm <- stri_sub(vitals_df$entry_date_tm, 1, 19)
  vitals_df
}
#' Returns dataframe with audit table data for audit_vitals table of animals
#' from arrival to death.
#'
#' @param conn database connection object
#' @param ranges has the list of date ranges prior to going into the BLS4 and
#' the animal Ids. The dates are not used for this query.
#' @import RODBC
#' @import stringi
#' @export
get_audit_vitals_df <- function(conn, ranges) {
  for (i in seq_along(ranges$ids)) {
    sql_txt <- stri_c(
      "select av.id, av.date_tm, av.respiration_rate, av.heart_rate,
      av.temperature,
      av.user_name as user_n, av.entry_date_tm,
      av.audit_action, av.audit_date_tm, av.audit_user_name as a_user
      from animal.audit.audit_vitals av
      where av.id in ('", vector2string(ranges$ids[[i]], SS = "', '"), "')
      order by av.id, av.date_tm, av.entry_date_tm, av.audit_date_tm,
      av.audit_action")
    df <- sqlQuery(conn, sql_txt, as.is = TRUE)
    if (i == 1) {
      audit_vitals_df <- df
    } else {
      audit_vitals_df <- rbind(audit_vitals_df, df)
    }
  }
  audit_vitals_df$date_tm <- stri_sub(audit_vitals_df$date_tm, 1, 19)
  audit_vitals_df$entry_date_tm <- stri_sub(audit_vitals_df$entry_date_tm, 1, 19)
  audit_vitals_df
}
#' Returns dataframe with weights, date_tm of collection, user name (user_n),
#' and entry_date_tm of animals from arrival to death.

#' @param conn database connection object
#' @param ranges has the list of date ranges prior to going into the BLS4 and
#' the animal Ids. The dates are not used for this query.
#' @import RODBC
#' @import stringi
#' @export
get_weight_df <- function(conn, ranges) {
  for (i in seq_along(ranges$ids)) {
    sql_txt <- stri_c(
      "select w.id, w.date_tm, w.weight,
      w.user_name as user_n, w.entry_date_tm
      from weight w
      where w.id in ('", vector2string(ranges$ids[[i]], SS = "', '"), "')
      order by w.id, w.date_tm, w.entry_date_tm")
    df <- sqlQuery(conn, sql_txt, as.is = TRUE)
    if (i == 1) {
      weight_df <- df
    } else {
      weight_df <- rbind(weight_df, df)
    }
  }
  weight_df$date_tm <- stri_sub(weight_df$date_tm, 1, 19)
  weight_df$entry_date_tm <- stri_sub(weight_df$entry_date_tm, 1, 19)
  weight_df
}
#' Returns dataframe with audit_weight data of animals from arrival to death.
#'
#' @param conn database connection object
#' @param ranges has the list of date ranges prior to going into the BLS4 and
#' the animal Ids. The dates are not used for this query.
#' @import RODBC
#' @import stringi
#' @export
get_audit_weight_df <- function(conn, ranges) {
  for (i in seq_along(ranges$ids)) {
    sql_txt <- stri_c(
      "select aw.id, aw.date_tm, aw.weight,
      aw.user_name as user_n, aw.entry_date_tm,
      aw.audit_action, aw.audit_date_tm, aw.audit_user_name as a_user
      from animal.audit.audit_weight aw
      where aw.id in ('", vector2string(ranges$ids[[i]], SS = "', '"), "')
      order by aw.id, aw.date_tm, aw.entry_date_tm")
    df <- sqlQuery(conn, sql_txt, as.is = TRUE)
    if (i == 1) {
      audit_weight_df <- df
    } else {
      audit_weight_df <- rbind(audit_weight_df, df)
    }
  }
  audit_weight_df$date_tm <- stri_sub(audit_weight_df$date_tm, 1, 19)
  audit_weight_df$entry_date_tm <- stri_sub(audit_weight_df$entry_date_tm, 1, 19)
  audit_weight_df
}
