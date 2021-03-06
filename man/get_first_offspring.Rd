% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_first_offspring.R
\name{get_first_offspring}
\alias{get_first_offspring}
\title{Get the first offspring for animals of a specified sex and arc_species_code
within a specified date range of provided.}
\usage{
get_first_offspring(
  conn,
  arc_species_code,
  sex = "F",
  start_date = NULL,
  end_date = NULL
)
}
\arguments{
\item{conn}{database connection object}

\item{arc_species_code}{character vector of length 1 having two character
species code}

\item{sex}{character vector of length 1 having "F", "M", or "both"}

\item{start_date}{optional character vector of length 1 having a valid date
represented by a unambiguous string value that specified the begining
date of the date interval of interest.}

\item{end_date}{optional character vector of length 1 having a valid date
represented by a unambiguous string value that specified the end
date of the date interval of interest.}
}
\description{
Get the first offspring for animals of a specified sex and arc_species_code
within a specified date range of provided.
}
