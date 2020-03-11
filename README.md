[![Build Status](https://travis-ci.org/wadpac/GGIR.svg?branch=master)](https://travis-ci.org/wadpac/GGIR) 
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/wadpac/GGIR?branch=master&svg=true)](https://ci.appveyor.com/project/wadpac/GGIR)
[![codecov](https://codecov.io/gh/wadpac/GGIR/branch/master/graph/badge.svg)](https://codecov.io/gh/wadpac/GGIR) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1051064.svg)](https://doi.org/10.5281/zenodo.1051064)
[![](https://cranlogs.r-pkg.org/badges/GGIR)](https://cran.r-project.org/package=GGIR)
[![](https://cranlogs.r-pkg.org/badges/grand-total/GGIR)](https://cran.r-project.org/package=GGIR)

  
  
  
The code in this repository is the development version for the code in R-package GGIR on CRAN

## Getting started:
The package [vignette](https://CRAN.R-project.org/package=GGIR/vignettes/GGIR.html) and [this](https://youtu.be/S8YPTrYNWdU) short tutorial video provide an introduction to GGIR, including: How it can be installed, Key software features, and where to get help.

## Contribution guidelines:
We always welcome contributions to the package.

### When you are familiar with GitHub:
We work with [GitHub Flow](https://guides.github.com/introduction/flow/) branching model.

Key steps:
1. Create a GitHub issue in this repository with description of the work that you plan to do.
2. Assign yourself to the GitHub issue you are working on, to inform other developers that you are working on it.
3. Create your own working branch or fork.
4. Make your changes in that branch or fork.
5. Commit your changes to your working branch/fork as long as you are not finished with your development.
6. Make sure the release notes in NEWS.Rd are updated.
7. Add your name to the contributors list in the DESCRIPTION file.
8. Run the tests and checks as CRAN, make sure they pass.
9. Once your work is finished, make a pull request, such that another developer can review your changes before merging them with the master branch.

### When you are unfamiliar with GitHub:
If you would like to propose additional functionalities or report an issue. Go to [issues](https://github.com/wadpac/GGIR/issues) and create a new issue.

If you would like to propose changes to the text of the manual this is possible.
1. Please go to the [man](https://github.com/wadpac/GGIR/tree/master/man) folder which holds all the parts of the manual.
2. Go to the part of the manual you want to edit and click on edit button (little pencil symbol) and make your changes.
3. Once you are finished, scroll down and describe you update and select the radio button "Create a new branch for this commit and start a pull request". One of the developers will then be able to review your changes and merge them in the master version of the code.
4. Click the green button "Propose file changes"

### Version numbers:
For a number of years I created a new GGIR release on CRAN with every major improvement to GGIR. This resulted in 18 releases in just two years, which is not very practical.

As of 2020 I am reducing the number of CRAN releases to only two or three per year, with intermediate releases on GitHub only. New features and bug fixes will first be released on GitHub, which creates a time window for extra testing before they end up in the next CRAN release. This will make the GGIR CRAN releases more stable and a good starting point for new GGIR users, while GitHub releases will become the place for the latest bug fixes and feature additions.

If possible, please use the GitHub version whenever you can to help spot issues timely before they end up in a CRAN release.

With this new release structure I will use the following version coding. **A.B-C**:

- A increases with major changes that affect backward compatibility with previous releases like changes in function names, function arguments or file format.
- B increases with every CRAN release.
- C increases with every GitHub release.