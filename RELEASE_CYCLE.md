# Version numbering

We use version encoding **A.B-C**:

- A increases with major changes that affect backward compatibility with previous releases like changes in function names, function arguments or file format.
- B increases with every CRAN release. We aim to avoid more than four CRAN releases per year.
- C increases with every GitHub release. We aim to avoid more than one GitHub release per month.

# GitHub releases

Before releasing, please make sure to check the following:

1. Create GitHub issue at least 1 weeks before the intended release to announce the release and indicate what will be in the release.
2. Make sure the change log `inst/NEWS.Rd` is up to date and that it says "GitHub-only-release date" rather than "release date"
3. Make sure the third (last) digit in the version number is incremented by one relative to the main branch and the date is the present date. This applies to the files `DESCRIPTION`, `GGIR-package.Rd` and `NEWS.Rd` file. Use function `prepareNewRelease.R` in the root of GGIR to double check that version number and date are consistent between these files.
4. Update package contributor list if new people have contributed.
5. Run `R CMD check --as-cran` to make sure all tests and checks pass.

Note that GitHub releases require a release name. We typically choose a random name of a city or town in South America. Whatever you choose this should be an easy to read and remember word.

# CRAN releases

To do a CRAN release, follow the following steps:

1. Create GitHub issue at least 4 weeks before the intended CRAN release announcing the release and indicating what will be in the release and a to do list.
2. A CRAN release should not come with major changes that have not been covered by any of the GitHub-only releases.
3. When everything looks ready for the release, repeat the same process as for the GitHub release with a few differences:
    - In the change log it should now say "release date" rather than "GitHub-only-release date".
    - Second digit in the version number is incremented by 1 relative to the current CRAN version.
    - Check whether a new R version has been released or is coming up and make sure GGIR is also tested with that version.
    - Run in RStudio `devtools::check( manual = TRUE, remote = TRUE, incoming = TRUE)` which will help to check urls
4. Ask Vincent (GitHub tag: vincentvanhees) to submit the release to CRAN as it needs to come from my e-mail address.
