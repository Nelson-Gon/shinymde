# shinymde 0.1.0.9000


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
