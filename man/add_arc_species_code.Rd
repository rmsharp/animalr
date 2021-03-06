% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_arc_species_code.R
\name{add_arc_species_code}
\alias{add_arc_species_code}
\title{Adds arc_species_code for each animal
in character vector id of the provided dataframe id_df.}
\usage{
add_arc_species_code(conn, id_df)
}
\arguments{
\item{conn}{database connection}

\item{id_df}{dataframe with animal Ids in id column}
}
\description{
Returns NA in vector if not available.
}
\details{
Assumes access to master database table in animal database.
}
