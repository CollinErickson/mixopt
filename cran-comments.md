I received an email from Brian Ripley on 8/27 that I needed to fix this package.
One of my other packages, ContourFunctions, had been removed from CRAN,
and this package was failing noSuggests. I have resubmitted ContourFunctions.
I fixed this package so that everything in Suggests was moved to Depends
or is only used within requireNamespace.

## Test environments

- local Windows 11, R 4.4.1
- R-hub: Fedora, Ubuntu, Windows
- Ubuntu 20.04.1 via GitHub Actions
- Win Builder
- macOS builder

## R CMD check results

Local Windows 11, R 4.4.1 (9/11/24):

  0 errors ✔ | 0 warnings ✔ | 0 notes ✔

macOS builder, R 4.4.0 (9/11/24):
  OK

Win builder - devel (9/11/24):
  1 NOTE, archived package

Win builder - release (1/15/23):
  OK

R-Hub Windows Server (1/14/23):
  OK

R-Hub Ubuntu (1/14/23):
  NOTE for a slow example

R-Hub Fedora Linux (1/14/23):
  Note for two slow examples


## Downstream dependencies

Another of my packages, GauPro, uses this package. I am also in the process
of updating that package, hopefully to be submitted to CRAN in the next few
days.
