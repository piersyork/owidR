## Test environments
* local R installation, R 4.1.1
* win-builder (devel)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Resubmission Fixes
* Added the webservices used to the Description file under the tag 'URL:'
* Added \value tags to the .Rd files which were missing in the previous submission.
* Changed \dontrun{} to donttest{} to keep example executable time under 5s.
