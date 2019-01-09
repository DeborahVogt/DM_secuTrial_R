# The secuTrial R library ![travis](https://travis-ci.com/SwissClinicalTrialOrganisation/DM_secuTrial_R.svg?branch=master)

## Installing with devtools

``` R
# if you do not have devtools installed
install.packages("devtools", dependencies=TRUE)
# install secuTrail R library
devtools::install_github("SwissClinicalTrialOrganisation/DM_secuTrial_R")
# load secuTrail R library
library(secuTrial)
```
## Testing with devtools

``` R
# assuming that your R session's working directory is DM_secuTrial_R
# alternatively you can supply the fullpath to the package directory
devtools::test(".")
```
