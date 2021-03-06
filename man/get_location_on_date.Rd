% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/animalr.R
\name{get_location_on_date}
\alias{get_location_on_date}
\title{Get location from \code{animal.dbo.location} for each animal on the date
specified.}
\usage{
get_location_on_date(conn, id_date_df)
}
\arguments{
\item{conn}{database connection object}

\item{id_date_df}{dataframe with an \code{id} column and with a
\code{loc_date} column (in POSIXct format).}
}
\description{
Get location from \code{animal.dbo.location} for each animal on the date
specified.
}
