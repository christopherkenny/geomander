## Test environments
* local R installation (Windows 10), R 4.1.1
* local R installation (macOS), R 4.1.1
* ubuntu 20.04 (on GitHub Actions), (devel and release)
* windows-latest (on GitHub Actions), (release)
* macOS-latest (on GitHub Actions), (release)
* Windows (on Winbuilder), (release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Additional Notes
* Remove `spdep` dependency to avoid timeouts on examples. Verified on Winbuilder that there are no new notes based on this.