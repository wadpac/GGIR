<!-- Describe your PR here -->

<!-- Please, make sure the following items are checked -->
### Checklist before merging:

- [ ] Existing tests still work (check by running the test suite, e.g. from RStudio).
- [ ] Added tests (if you added functionality) or fixed existing test (if you fixed a bug).
- [ ] Clean code has been attempted, e.g. intuitive object names and no code redundancy.
- [ ] Documentation updated:
  - [ ] Function documentation
  - [ ] Chapter vignettes for GitHub IO
  - [ ] Vignettes for CRAN
- [ ] Corresponding issue tagged in PR message. If no issue exist, please create an issue and tag it.
- [ ] Updated release notes in `inst/NEWS.Rd` with a user-readable summary. Please, include references to relevant issues or PR discussions.
- [ ] Added your name to the contributors lists in the `DESCRIPTION` file, if you think you made a significant contribution.
- [ ] GGIR parameters were added/removed. If yes, please also complete checklist below.

**If NEW GGIR parameter(s) were added then these NEW parameter(s) are:**
- [ ] documented in `man/GGIR.Rd`
- [ ] included with a default in `R/load_params.R`
- [ ] included with value class check in `R/check_params.R`
- [ ] included in table of `vignettes/GGIRParameters.Rmd` with references to the GGIR parts the parameter is used in.
- [ ] mentioned in NEWS.Rd as NEW parameter

**If GGIR parameter(s) were deprecated these parameter(s) are:**
- [ ] documented as deprecated in `man/GGIR.Rd`
- [ ] removed from `R/load_params.R`
- [ ] removed from `R/check_params.R`
- [ ] removed from table in `vignettes/GGIRParameters.Rmd`
- [ ] mentioned as deprecated parameter in NEWS.Rd
- [ ] added to the list in `R/extract_params.R` with deprecated parameters such that these do not produce warnings when found in old config.csv files.