---
date: "1/1/2022"
author: "Nelson Gonzabato"
title: "Changes to shinymde"
---

**shinymde 0.1.2**

* Plots now fill the entire page. 

* The app was broken into several UIs and helper functions created to make it
easier to build repetitive UIs. See [#15](https://github.com/Nelson-Gon/shinymde/issues/15).  

* UI elements are now rendered programmatically to make tests easier. 

* The UI has been redesigned to make exploration even more fun. 

* There is now an option to customize plot designs for examples themes.
See https://github.com/Nelson-Gon/shinymde/issues/14

**shinymde 0.1.1**

* UI overhaul to ensure a much better user experience. Specifically, inputs were decluttered to make it easier to do analyses with minimal interference. 

* Action buttons are now the same color as the app's skin. 

* The sidebar menu is now collapsed on app start. 

* Changes to the UI to ensure that it is less cluttered. Download buttons now appear above plots/tables. 

* `dropdown` was dropped in favour of `dropdownButton`.

* The UI is now red instead of blue.

* All tabs now have drop downs instead of a more cluttered interface. 

* The dashboard side bar is now collapsed on start.

* `renderUI` was dropped in favour of `update*` functions which lead to a significant speed improvement. 

* Tooltips were dropped. We now use a simpler interface based on `shinyWidgets`.

* There is now a reset button to allow reset of plot option in the "Visualize Missingness" tab. 

* Following changes in `mde`, users can now filter columns in `na_summary` based on regular expression matches. 

* `bsTooltip` is now imported explicitly from `shinyBS`. This is a workaround to allow `R CMD check` passes. 

* Non-needed directories for build were removed from the shipped package/app. 

---

**shinymde 0.1.0**

* shinymde can now be browsed at https://nelson-gon.shinyapps.io/shinymde.

* Tool tips are now shown above not below a given widget. 

* For the dropping tab, "Percent NA" is now hidden for `drop_all_na` as expected.

* For visual summaries, users can now choose to turn off the text labels on bar plots. 

* For remote datasets, the file extension is now accepted via a selection from a list of currently supported file types. 

* Users can now choose a sheet number for "xlsx" files. 

* There is now a slider input to choose size of arrows and points for lollipop plots.

* For plot type "lollipop", `fill_variable` is now hidden. The plot is also now not flipped. 

* Users can now choose to either use a bar plot or a lollipop plot. 

* Fixed issues with downloading reports after the move to `shinydashboard`. 

* `shinyBS` is now in the `Depends` field to ensure that tootltips work as expected. This is presumably because some other functions should be loaded first for tooltip adders to work. 

* There are now tooltips to aid users with the navigation process. This also adds `shinyBS` to the list of app dependencies. 

* For inbuilt datasets, only `data.frame` objects are supported. 

* The title at the header now includes the package version. 

* There are now runnable examples (essentially one example to launch the app).

* There is now a highlight when a user hovers over a table row. 

* Hovers are now more "obvious" with target highlighted. 

* The `Reset` button at the input tab now resets the initial text too. The input tab now also provides a user with system details prior to dataset confirmation. 

* There is now a homepage that can direct users to project homepage, project
documentation, and author homepage.

* Massive UI changes to improve user experience. In particular, the layout is now based on `shinydashboard`. 

* Users now have a reset button at the "Input Data" stage that may be useful in case of mind changes. 

* `run_app` was renamed `launch_app` to avoid name conflicts for example with `golem::run_app`. 

* A simple welcome message was added to ensure that users have something visible as they select a dataset. It also serves as a form of credit to the author(s). 

* Tabs except Input Data are now hidden on app startup. 

* Code refactoring was done to allow for easier showing/hiding of elements, conditionally. 

* Missingness summaries now default to a percent_missing sort that is provided in descending order. The user can always change this. 

* User feedback is now provided via `shinyFeedback` which is a more helpful way for users to view feedback directly within the application. 


* A reset button now exists to enable a user to restore default values for plot save preferences. 

* Grouped summaries now support exclusion following a fix in [mde](https://nelson-gon.github.io/mde). 

* There is now a download button to allow saving of plots.

* All tabs now use a sidebar style which also makes it more easier to navigate and also makes the download button appear more uniform on the app's page. 

* Users now receive a summary of the input data on uploading/selecting a data set. The Input Data tab also now includes a sidebar layout. 

* Visual summaries are now shown in a sidebar layout. 


* It is now possible to download a summary of missingness, a data set with values recoded as requested, or one where values have been dropped. 

* Initially supported operations

- Data input supports either user data, an inbuilt data set, or a remote data set provided as a web link. For user data, only ".csv", ".xlsx", and ".tsv" are supported. 

- A tabular missingness summary is provided that can be downloaded by the user. The file format to write to is guessed from the user's input. 


- Recoding values supports `recode_as_na`, `recode_na_as`, `recode_na_if`, `recode_as_na_for` and other recode functions from package `mde`.

- Dropping values using `drop_` from `mde`. 

- A visual summary of missingness is also available. This is currently "just" a visual alternative to the tabular summary of missingness.  

* Added a `NEWS.md` file to track changes to the package.
