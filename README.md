shinymde: A shiny interface to mde, the missing data explorer
================
2021-07-14

[![license](https://img.shields.io/badge/license-MIT-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://GitHub.com/Nelson-Gon/shinymde/graphs/commit-activity)
[![Project
Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/)
[![GitHub last
commit](https://img.shields.io/github/last-commit/Nelson-Gon/shinymde.svg)](https://github.com/Nelson-Gon/shinymde/commits/master)
[![GitHub
issues](https://img.shields.io/github/issues/Nelson-Gon/shinymde.svg)](https://GitHub.com/Nelson-Gon/shinymde/issues/)
[![GitHub
issues-closed](https://img.shields.io/github/issues-closed/Nelson-Gon/shinymde.svg)](https://GitHub.com/Nelson-Gon/shinymde/issues?q=is%3Aissue+is%3Aclosed)
[![PRs
Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square)](https://makeapullrequest.com)

# Goals of `shinymde`

`shinymde` provides an application (user interface) to ease the process
of missing data exploration. The core functionality accessed is
programmed in the code only package
[mde](https://github.com/Nelson-Gon/mde). It is hoped that this will
provide an even easier way to explore missing data.

# Installation

To install, please `clone` the repository and run `app.R` in `RStudio`.

``` shell
git clone git@github.com:Nelson-Gon/shinymde.git
cd shinymde 
```

# Currently Implemented features

-   [x] Summary of missingness

This uses [mde](https://nelson-gon.github.io/mde)’s `na_summary`
function to generate a simple missingness report. The user can also
download this report for use in other purposes.

-   [x] Recoding as NA and recoding NA as

This allows a user to recode a given value as `NA`, R’s handler of
missing values or convert `NA`s to some other value. It uses `mde`’s
`recode_*` functions.

-   [ ] Missingness plots

# `shinymde` in action

Below is a screenshot showing current functionality.

![shinymde in
action](https://github.com/Nelson-Gon/shinymde/blob/main/images/sample_ui.png?raw=true)

# Features that need further development (help)

If you would like to work on this project, here is a features list that
could be a good place to start.

-   [ ] Add hovers to user input

-   [ ] Switchable dark mode

-   [ ] Control `pattern_type`/`subset_cols` choice in `na_summary`.

-   [ ] Use cached filename in input filename if a user quits the
    program.

------------------------------------------------------------------------

Thank you and keep building,

Nelson

2021-07-14
