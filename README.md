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
## Testing and running examples with devtools

``` R
# assuming that your R session's working directory is DM_secuTrial_R
# alternatively you can supply the fullpath to the package directory
devtools::test(".")
devtools::run_examples(".")
```

## Guidelines for contributers

In order to contribute to this R library you should fork the main repository, make your changes and send a pull request (PR). 
The PR should also have a description to help the reviewer understand what has been added/changed. 
New functionalities must be thoroughly documented, have examples and should be accompanied by at least one 
test in [test-secuTrial.R](tests/testthat/test-secuTrial.R) to ensure longterm robustness.
The PR will only be reviewed if all travis checks are successful. The person sending the PR should not be 
the one merging it.

