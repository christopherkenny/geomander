## Test environments
* local R installation (Windows 11), R 4.2.3
* local R installation (macOS), R 4.2.3
* ubuntu 20.04 (on GitHub Actions), (devel and release)
* windows-latest (on GitHub Actions), (release)
* macOS-latest (on GitHub Actions), (release)
* Windows (on Winbuilder), (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Additional Notes
* Fixes the un-used import of Rcpp.
* Makes use of `redist` in vignettes conditional.
* Updates the CITATION file to use `bibentry()` and remove `personList()` call.