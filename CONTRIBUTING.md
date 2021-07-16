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
6. update or expand the documentation;
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

## New release

GGIR follows the [release cycle process described in this document](RELEASE_CYCLE.md).