% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dra.R
\name{r2dra}
\alias{r2dra}
\title{R to DRA}
\usage{
r2dra(precincts, plan, state, path, epsg = 3857)
}
\arguments{
\item{precincts}{Required. an sf dataframe of precinct shapes}

\item{plan}{Required. Either a vector of district assignments
or the name of a column in precincts with district assignments.}

\item{state}{Required. the state postal code of the state}

\item{path}{Optional. A path to try to save to. Warns if saving failed.}

\item{epsg}{numeric EPSG code to planarize to. Default is 3857.}
}
\value{
tibble with columns Id, as used by DRA, identical to GEOID in census terms and District.
}
\description{
Project a plan at the precinct level down to blocks into a format that can be used
with DRA. Projecting down to blocks can take a lot of time for larger states.
}
\examples{
\dontrun{
# Needs Census Bureau API
cd <- tinytiger::tt_congressional_districts() |> filter(STATEFP == '49')
cnty <- tinytiger::tt_counties(state = 49)
matchedcty <- geo_match(from = cnty, to = cd)
# use counties as precincts and let the plan be their center match:
r2dra(cnty, matchedcty, 'UT', 'r2dra_ex.csv')
}
}
\concept{dra}
