## Test environments
* local R installation (Windows 10), R 4.0.4
* local R installation (macOS), R 4.0.4
* ubuntu 20.04 (on GitHub Actions), (devel and release)
* windows-latest (on GitHub Actions), (release)

## R CMD check results

0 errors | 0 warnings | 0 notes


## Additional Submission Notes

* First submission failed pretest due to vignettes relying on a Census Bureau API. The vignettes have been added to .Rbuildignore and VignetteBuilder has been removed from the DESCRIPTION.