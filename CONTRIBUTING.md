---
editor_options: 
  markdown: 
    wrap: 72
---

# Contributing guidelines

GGIR welcomes any kind of contribution to the software, from simple
comment or question to a full fledged [pull
request](https://help.github.com/articles/about-pull-requests/). Please
read and follow our [Code of Conduct](CODE_OF_CONDUCT.md).

A contribution can be one of the following cases:

1.  [you have a question](#questions);
2.  [you think you may have found a bug](#bugs) (including unexpected
    behavior);
3.  [you want to make some kind of change to the code
    base](#changes-or-additions) (e.g. to fix a bug, to add a new
    feature, to update documentation);
4.  [you want to make a new release of the code base](#new-release).
5.  [you want to help evaluate and optimize an algorithm or
    functionality of GGIR](#optimize-functionalities)

## Questions {#questions}

The [GGIR google
group](https://groups.google.com/forum/#!forum/RpackageGGIR) offers an
open platform in which GGIR users can collaborate and support each
other. If you have a question related to the use of GGIR:

1.  Use the search functionality
    [here](https://groups.google.com/g/RpackageGGIR) to see if someone
    already experienced the same issue;
2.  if your search did not yield any relevant results, start a new
    conversation.

## Bugs {#bugs}

If you think you have found a bug in the GGIR code, then the [issue
tracker](https://github.com/wadpac/GGIR/issues) of the GGIR repository
on GitHub might be of help. If you want to report a bug:

1.  Use the search functionality
    [here](https://github.com/wadpac/GGIR/issues) to see if someone
    already filed the same issue;
2.  If your issue search did not yield any relevant results, make a new
    issue, and choose the Bug report type. This includes a checklist to
    make sure you provide enough information to the rest of the
    community to understand the cause and context of the problem.

## Changes or additions {#changes-or-additions}

If you found something you would like to change in the GGIR code, please
follow the next steps:

1.  (**Important**) announce your plan to the rest of the community
    *before you start working*. This announcement should be in the form
    of a (new) issue. Choose the Feature request type, which includes a
    checklist of things to consider to get the discussion going;
2.  (**Important**) wait until some kind of consensus is reached about
    your idea being a good idea;
3.  If needed, fork the repository to your own Github profile and create
    your own feature branch off of the latest master commit. While
    working on your feature branch, make sure to stay up to date with
    the master branch by pulling in changes, possibly from the
    'upstream' repository (follow the instructions
    [here](https://help.github.com/articles/configuring-a-remote-for-a-fork/)
    and [here](https://help.github.com/articles/syncing-a-fork/));
4.  Make sure the existing tests still work by running the test suite
    from RStudio;
5.  Add your own tests (if necessary);
6.  Update or expand the documentation, see [package documentation
    guidelines](#package-documentation);
7.  Make sure the release notes in `inst/NEWS.Rd` are updated;
8.  Add your name to the contributors lists in the `DESCRIPTION` file;
9.  Push your feature branch to (your fork of) the GGIR repository on
    GitHub;
10. Create the pull request, e.g. following the instructions
    [here](https://help.github.com/articles/creating-a-pull-request/).
    The pull request template includes a checklist with the above items.

In case you feel like you've made a valuable contribution, but you don't
know how to write or run tests for it, or how to generate the
documentation: don't let this discourage you from making the pull
request; we can help you! Just go ahead and submit the pull request, but
keep in mind that you might be asked to append additional commits to
your pull request.

### Coding style

We loosely follow the [tidyverse style
guide](https://style.tidyverse.org/), but do not enforce every rule
strictly. For instance, we prefer `=` instead of `<-` as the default
assignment operator. When in doubt about what style to use, don't
hesitate to get in touch.

Some general guidelines that we try to adhere to:

-   Use standard R as much as possible, keep dependencies to a minimum.
-   Keep loops to a minimum.
-   Don't make lines too long.

If you are a first time contributor, don't worry about coding style too
much. We will help you get things in shape.

### Package documentation {#package-documentation}

We currently have three sources for documenting the package:

-   The reference manual, including package basic information and the
    functions documentation files.
-   The package vignettes.
-   The github.io website (built with the `pkgdown` package).

#### Reference manual

The reference manual gets the information from the .Rd documents within
the man folder in the package repository. Therefore, updating the
information in those files will automatically update the reference
manual. Note that most of the GGIR functions are not intended for direct
interaction with the user, as such, the documentation of most arguments
is centralized in the details section of man/GGIR.Rd. If you for example
want to add an extra parameter to `params_247` then this should be
documented there. Further, you should not forget to include the new
argument in functions `load_params` and `check_params`.

#### Package vignettes

The folder vignettes in the GGIR repository contains the .Rmd files. The
.Rmd files that do not start with the word 'chapter' are used for the
traditional package vignettes as hosted by CRAN. Use these files to edit
an existing vignette, or use the structure of any of the vignettes to
build up a new one. All .Rmd files which name starts with the word
'chapter' are ignored. These chapter-vignettes are used only for the
github.io website (see [next section](#github.io-website)).

**To create a new vignette for CRAN**

To create a new package vignette for CRAN, please use
`usethis::use_vignette()` and make sure the name of the vignette file
does not start by "chapter". For example, if you want to create a new
vignette on sleep for CRAN, you may do the following:

```         
usethis::use_vignette(name = "sleep", 
                      title = "How to analyse your sleep data in GGIR")
```

This would create a new "sleep.Rmd" file within the vignettes folder in
the GGIR repository. Then you can edit this file to build up the
vignette.

**To remove a vignette from CRAN**

There are two ways to remove a vignette from CRAN:

1.  Removing the Rmd file corresponding to the vignette in the vignettes
    folder, note that the file and the information will be lost.
2.  Adding the path to the vignette in the .Rbuildignore file available
    in the GGIR repository. For example, to remove the GGIRParameters
    vignette from CRAN, you can add:

```         
^vignettes/GGIRParameters.Rmd
```

#### github.io website {#github.io-website}

For updating or adding information to the github.io website, we need to
use [the pkgdown configuration file](_pkgdown.yml) that can be found in
the repositories root directory, as well as with the chapter vignettes
discussed above.

**To edit information in an existing chapter**

1.  Open the vignette corresponding to the chapter you wish to edit (see
    the \_pkgdown.yml) file for the chapter and its vignette path
    (href).
2.  Make your changes in the vignette.
3.  Run the `pkgdown::build_site()` function.

**To add a new chapter**

1.  Create a Rmd file for the vignette via `usethis::use_vignette()` and
    make sure the name of the vignette starts by "chapter", for example:

```         
usethis::use_vignette(name = "chapterSleep", 
                      title = "10. How to analyse your sleep data in GGIR")
```

2.  Open the [\_pkgdown.yml](_pkgdown.yml) file and fill up the name and
    reference of the new chapter under menu. Make sure to follow the
    coding and structure of the rest of chapters.
3.  Run the `pkgdown::build_site()` function.

**To remove a chapter**

1.  Remove the lines corresponding to the chapter in the
    [\_pkgdown.yml](_pkgdown.yml) file in line 42 onwards.
2.  Optionally you may remove the Rmd file corresponding to the chapter,
    but only by doing the step 1, the chapter will not appear in the
    github.io website.
3.  Run the `pkgdown::build_site()` function.

**To edit the name of a chapter**

Chapter names are defined twice, in the [\_pkgdown.yml](_pkgdown.yml)
file and in the vignette file itself. You need to make sure both titles
match as the first will be used in the drop-down list in the github.io
website and the other in the specific page for the chapter.

#### Adding the changes to the master branch

The last step would be committing and pushing your changes to github and
making a pull request as with any other contribution to the package.
Note that, running the `pkgdown::build_site()` function will edit the
files within the docs folder, and probably add some new files. This only
applies when editing information from the github.io website. It is
important that these changes to the files in the docs folder are also
part of the pull requests, as otherwise the website would not be
updated.

## Optimization of functionalities {#optimize-functionality}

You might not have the coding skills to contribute to the code base of
GGIR, and still your contribution could be very important for us. Doing
research on algorithms or functionalities of GGIR is crucial to support
its development. We welcome contributions on any aspect in this regard,
and here we enumerated some ideas or research opportunities:

1.  Help improve the imputation methods for epoch-level data, especially
    for categorical outcomes (e.g., classification of physical activity
    intensities or sleep/rest status).
2.  Evaluate and optimize any algorithm used by GGIR, for example:
    -   The HorAngle algorithm for sleep period time definition.
    -   The sleep detection in children and preschoolers.
    -   The implementation of old sleep algorithms (e.g., Sadeh or
        Cole-Kripke) on data collected with modern devices.

## New release {#new-release}

GGIR follows the [release cycle process described in this
document](RELEASE_CYCLE.md).
