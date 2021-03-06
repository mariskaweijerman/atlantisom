% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_survey.R
\name{create_survey}
\alias{create_survey}
\title{create_survey R function for Atlantis scenario}
\usage{
create_survey(dat, time, species, spex, boxes, effic, selex)
}
\arguments{
\item{dat}{The dataframe of numbers-at-age from reading in the Atlantis fileSnapshot}

\item{time}{The timing of the survey (can be a vector if survey spans multiple time periods)}

\item{species}{The species to sample in the survey (a vector)}

\item{spex}{The specifications of Atlantis model (box-specific area, habitat, etc.)
for now, from load_boxarea a dataframe with polygon and area column names}

\item{boxes}{A matrix with two columns:
1) polygon:  box ID's that are sampled
2) survArea: area sampled in that box}

\item{effic}{Efficiency for each species: a matrix with nrow=length(species). Columns:
 species:    the species name. Matches names in species
efficiency:}

\item{selex}{Selectivity at age. A dataframe defining selectivity at age for each species. Columns are:
species: the species name. Matches names in species
agecl:   the age class that selectivity represents
selex:   the proportion selected relative to fully selected age classes (between 0 and 1)}
}
\description{
create_survey R function for Atlantis scenario
}
\details{
Returns a matrix similar to the input matrix

columns: species, agecl, polygon, layer, time, atoutput

--will sum over layers, but enter NA as layer to indicate all layers

This function is for a vector of defined species
}
\author{
Poseidon
}

