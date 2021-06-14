## Test environments
* local R installation (Windows 10), R 4.1.0
* local R installation (macOS), R 4.1.0
* ubuntu 20.04 (on GitHub Actions), (devel and release)
* windows-latest (on GitHub Actions), (release)
* macOS-latest (on GitHub Actions), (release)
* rhub::check_on_solaris()

## R CMD check results

0 errors | 0 warnings | 0 notes


## Additional Submission Notes
* Error on solaris build was replicated on R-hub and fixed by downgrading included data to use a very old version of proj
* Additional package note on CRAN linux build (Rcpp) was removed from Imports and is only listed under LinkingTo in description.