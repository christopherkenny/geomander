## Test environments
* local R installation (Windows 10), R 4.0.5
* local R installation (macOS), R 4.0.5
* ubuntu 20.04 (on GitHub Actions), (devel and release)
* windows-latest (on GitHub Actions), (release)

## R CMD check results

0 errors | 0 warnings | 0 notes


## Additional Submission Notes
* This is a resubmission after 2 months from version 1.0.3 and few days after 1.05. Thank you for the original comments.
* There is no paper reference for the package currently.
* Instances of T/F checked for to replace with TRUE/FALSE. An occurance of `=T` over `=TRUE` was replaced in sort.R
* \dontrun{} is replaced with \donttest{} where possible. Features that rely on the US Census Bureau API remain in \dontrun{}
* Long examples are moved to \donttest{}. A few core functionality testthat tests are added.
* Small data files were added which allow for vignettes to run without using the US Census Bureau API
* Fixed MIT License information
* GitHub URL/URIs updated in vignette and README.
* Updates to work with sf 1.0-0 and s2 1.0.5