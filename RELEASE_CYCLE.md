# GitHub releases

We typically release GGIR on GitHub no more than once per month, typically in the last week of the month.

When you do this please inform the others that you are going to do this.
Before releasing, please make sure to check the following:

1. Make sure the change log `inst/NEWS.Rd` is up to date and that it says "GitHub-only-release date" rather than "release date"
2. Make sure the third (last) digit in the version number is incremented by one relative to the master branch and the date is the present date. This applies to the files `DESCRIPTION`, `CITATION.cff` (not the cff-version, but the version on line 56), `GGIR-package.Rd` and `NEWS.Rd` file. Use function `prepareNewRelease.R` in the root of GGIR to double check that version number and date are consistent between these files.
3. Update package contributor list if new people have contributed.
4. Run `R CMD check --as-cran` to make sure all tests and checks pass.

Then, create a new GitHub release.
Note that GitHub releases require a release name.
We typically choose a random name of a city or town in South America.
Whatever you choose this should be an easy to read and remember.

# CRAN releases

To do a CRAN release, follow the following steps:

1. Create GitHub issue at least 4 weeks before the intended CRAN release announcing the release and indicating what will be in the release and a to do list.
2. A CRAN release should not come with major changes that have not been covered by any of the GitHub releases, except from full version updates (1.0 -> 2.0 -> 3.0 -> ...). If a full version update is the case, create a branch in the wadpac/GGIR repository dedicated to the new release.
3. When everything looks ready for the release, repeat the same process as for the GitHub release with a few differences:
    - In the change log it should now say "release data" rather than "GitHub-only-release date".
    - Second digit in the version number is incremented by 1 relative to the current CRAN version.
    - Check whether a new R version has been released or is coming up and make sure GGIR is also tested with that version.
    - Run in RStudio `devtools::check( manual = TRUE, remote = TRUE, incoming = TRUE)` which will help to check urls
    - All of us will review the new release, and when all are happy I will submit it to CRAN as this needs to come from my e-mail address.
