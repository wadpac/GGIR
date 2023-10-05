# Contributing guidelines

We welcome any kind of contribution to our software, from simple comment or question to a full fledged [pull request](https://help.github.com/articles/about-pull-requests/). Please read and follow our [Code of Conduct](CODE_OF_CONDUCT.md).

A contribution can be one of the following cases:

1. [you have a question](#questions);
2. [you think you may have found a bug](#bugs) (including unexpected behavior);
3. [you want to make some kind of change to the code base](#changes-or-additions) (e.g. to fix a bug, to add a new feature, to update documentation);
4. [you want to make a new release of the code base](#new-release).

The sections below outline the steps in each case.

## Questions

1. use the search functionality [here](https://groups.google.com/g/RpackageGGIR) to see if someone already experienced the same issue;
2. if your search did not yield any relevant results, start a new conversation.

## Bugs

1. use the search functionality [here](https://github.com/wadpac/GGIR/issues) to see if someone already filed the same issue;
2. if your issue search did not yield any relevant results, make a new issue, and choose the Bug report type. This includes a checklist to make sure you provide enough information to the rest of the community to understand the cause and context of the problem.

## Changes or additions

1. (**important**) announce your plan to the rest of the community *before you start working*. This announcement should be in the form of a (new) issue. Choose the Feature request type, which includes a checklist of things to consider to get the discussion going;
2. (**important**) wait until some kind of consensus is reached about your idea being a good idea;
3. if needed, fork the repository to your own Github profile and create your own feature branch off of the latest master commit. While working on your feature branch, make sure to stay up to date with the master branch by pulling in changes, possibly from the 'upstream' repository (follow the instructions [here](https://help.github.com/articles/configuring-a-remote-for-a-fork/) and [here](https://help.github.com/articles/syncing-a-fork/));
4. make sure the existing tests still work by running the test suite from RStudio;
5. add your own tests (if necessary);
6. update or expand the documentation, see [package documentation guidelines](#package-documentation);
7. make sure the release notes in `inst/NEWS.Rd` are updated;
8. add your name to the contributors lists in the `DESCRIPTION` and `CITATION.cff` files;
9. push your feature branch to (your fork of) the GGIR repository on GitHub;
10. create the pull request, e.g. following the instructions [here](https://help.github.com/articles/creating-a-pull-request/). The pull request template includes a checklist with the above items.

In case you feel like you've made a valuable contribution, but you don't know how to write or run tests for it, or how to generate the documentation: don't let this discourage you from making the pull request; we can help you! Just go ahead and submit the pull request, but keep in mind that you might be asked to append additional commits to your pull request.

### Coding style

We loosely follow the [tidyverse style guide](https://style.tidyverse.org/), but do not enforce every rule strictly.
For instance, we prefer `=` instead of `<-` as the default assignment operator.
When in doubt about what style to use, don't hesitate to get in touch.

Some general guidelines that we try to adhere to:

- Use standard R as much as possible, keep dependencies to a minimum.
- Keep loops to a minimum.
- Don't make lines too long.

If you are a first time contributor, don't worry about coding style too much.
We will help you get things in shape.

### Package documentation

We currently have three sources for documenting the package:

- The reference manual, including package basic information and the functions documentation files.
- The package vignettes.
- The github.io website (built with the `pkgdown` package).

#### Reference manual

The reference manual gets the information from the .Rd documents within the man
folder in the package repository. Therefore, updating the information in those
files will automatically update the reference manual. Note that most of the GGIR
functions are not intended for direct interaction witht he user, as such, the
documentation of all the parameters of GGIR are centralized in the .Rd file for
the GGIR function.

#### Package vignettes

The folder vignettes in the GGIR package repository contains the traditional 
vignettes for the GGIR package and the chapter vignettes that are used only for
the github.io website (see [next section](#github.io-website)). Access those 
files to edit an existing vignette, or use the structure of any of the vignettes
to build up a new one.

#### github.io website

For updating or adding information to the github.io website, we need to interact
with [the pkgdown configuration file](_pkgdown.yml) that can be found in the GGIR 
source directory in GitHub, as well as with the chapter vignettes that can be 
found in the vignettes directory (i.e., all the Rmd files starting by "chapter").

**To edit information in an existing chapter**

1. Open the vignette corresponding to the chapter you wish to edit (see the _pkgdown.yml) file for the chapter and its vignette path (href).
2. Make your changes in the vignette.
3. Run the `pkgdown::build_site()` function.

**To add a new chapter**

1. Create a new html vignette with the new chapter information.
2. Open the [_pkgdown.yml](_pkgdown.yml) file and fill up the name and reference of the
new chapter under menu. Make sure to follow the coding and structure of the rest of chapters.
3. Run the `pkgdown::build_site()` function.

#### Committing the changes to the master branch

The last step would be committing and pushin your changes to github and making a
pull request as with any other contribution to the package. Note that, running the 
`pkgdown::build_site()` function will edit the files within the docs folder, and 
probably add some new files. This only applies when editing information from the 
github.io website. It is important that these changes to the files in the
docs folder are also part of the pull requests, as otherwise the website would not 
be updated.

## New release

GGIR follows the [release cycle process described in this document](RELEASE_CYCLE.md).
