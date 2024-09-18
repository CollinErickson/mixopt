I received an email from Brian Ripley on 8/27 that I needed to fix this package.
One of my other packages, ContourFunctions, had been removed from CRAN,
also for failing noSuggests. I have resubmitted ContourFunctions.
I fixed this package so that everything in Suggests was moved to Depends
or is only used within requireNamespace.

## Test environments

- local Windows 11, R 4.4.1
- R-hub: atlas, ubuntu-next, linux (R-devel), macos (R-devel), windows (R-devel)
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

Win builder - release (9/12/24):
  1 NOTE, archived package

GitHub actions - Ubuntu 22.04.4, R 4.4.1 (9/12/24):
  OK

R-Hub linux/atlas, Ubuntu 22.04.4 (9/12/23):
  OK

R-Hub linux/ubuntu-next, Ubuntu 22.04.4 (9/12/23):
  OK

R-Hub linux (R-devel), Ubuntu 22.04.4 (9/12/23):
  OK

R-Hub macos (R-devel), MacOS 13.6.9 (9/12/23):
  OK

R-Hub windows (R-devel), Microsoft Windows Server 2022 (9/12/23):
  OK

## Downstream dependencies

Another of my packages, GauPro, uses this package. I am also in the process
of updating that package, hopefully to be submitted to CRAN in the next few
days.
