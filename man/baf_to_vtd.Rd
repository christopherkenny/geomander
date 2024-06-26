% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/baf_to_vtd.R
\name{baf_to_vtd}
\alias{baf_to_vtd}
\title{Estimate Plans from a Block Assignment File to Voting Districts}
\usage{
baf_to_vtd(baf, plan_name, GEOID = "GEOID", year = 2020)
}
\arguments{
\item{baf}{a tibble representing a block assignment file.}

\item{plan_name}{character. Name of column in \code{baf} which corresponds to the districts.}

\item{GEOID}{character. Name of column which corresponds to each block's GEOID,
sometimes called "BLOCKID". Default is \code{'GEOID'}.}

\item{year}{the decade to request, either \code{2010} or \code{2020}. Default is \code{2020}.}
}
\value{
a tibble with a vtd-level assignment file
}
\description{
District lines are often provided at the census block level, but analyses
often occur at the voting district level. This provides a simple way to
estimate the block level to the voting district level.
}
\details{
If a voting district is split between blocks, this currently uses the most
common district.
}
\examples{
# Not guaranteed to reach download from redistrict2020.org
\dontrun{
# download and read baf ----
url <- paste0('https://github.com/PlanScore/Redistrict2020/', 
              'raw/main/files/DE-2021-01/DE_SLDU_bef.zip')
tf <- tempfile('.zip')
utils::download.file(url, tf)
utils::unzip(tf, exdir = dirname(tf))
baf <- readr::read_csv(
  file = paste0(dirname(tf), '/DE_SLDU_bef.csv'),
  col_types = 'ci'
)
names(baf) <- c('GEOID', 'ssd_20')

# convert to vtd level ----
baf_to_vtd(baf = baf, plan_name = 'ssd_20', 'GEOID')
}

}
