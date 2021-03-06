% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/animalr.R
\name{is_animal_alive}
\alias{is_animal_alive}
\title{Returns logical vector indicating if id is animal in master and alive on the
provided date.}
\usage{
is_animal_alive(conn, id_df, format = "\%Y-\%m-\%d")
}
\arguments{
\item{conn}{database connection object}

\item{id_df}{dataframe with columns id and sample_date where date is a
character string with the format mm-dd-yyyy.}

\item{format}{optional character vector of length one having the date
format string to be used in call to \code{is_valid_date()}.}
}
\description{
Returns logical vector indicating if id is animal in master and alive on the
provided date.
}
