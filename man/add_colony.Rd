% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/animalr.R
\name{add_colony}
\alias{add_colony}
\title{Returns dataframe with colony for each animal
in character vector id for a specified date in colony_date column of the
provided dataframe id_df.
Returns NA in vector if not available.}
\usage{
add_colony(conn, id_df)
}
\arguments{
\item{conn}{database connection}

\item{id_df}{dataframe with animal Ids in id column}
}
\description{
Assumes access to master database table in animal database.
}
