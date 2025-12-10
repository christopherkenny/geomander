## Test environments

* local R installation (Windows 11), R 4.5.2
* local R installation (macOS), R 4.5.2
* ubuntu-latest (on GitHub Actions), (oldrel-1, devel, and release)
* windows-latest (on GitHub Actions), (release)
* macOS-latest (on GitHub Actions), (release)
* Windows (on Winbuilder), (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Additional notes

* This update fixes a download issue with an internet resource and now checks curl headers before attempting a download so that it will fail gracefully (if necessary).

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

