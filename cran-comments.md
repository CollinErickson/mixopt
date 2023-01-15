I have made some minor changes to the functions within the package.

## Test environments

- local Windows 11, R 4.2.2
- R-hub: Fedora, Ubuntu, Windows
- Ubuntu 20.04.1 via GitHub Actions
- Win Builder
- macOS builder

## R CMD check results

Local Windows 11, R 4.2.2 (1/13/23):

  0 errors ✔ | 0 warnings ✔ | 0 notes ✔

macOS builder, R 4.2.1 (1/13/23):
  OK

Win builder (devel 1/13/23, release 1/15/23):
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
