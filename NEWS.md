# shinymde 0.1.0.9000


* It is now possible to download a summary of missingness, a data set with values recoded as requested, or one where values have been dropped. 

* Initially supported operations

- Data input supports either user data, an inbuilt data set, or a remote data set provided as a web link. For user data, only ".csv", ".xlsx", and ".tsv" are supported. 

- A tabular missingness summary is provided that can be downloaded by the user. The file format to write to is guessed from the user's input. 


- Recoding values supports `recode_as_na`, `recode_na_as`, `recode_na_if`, `recode_as_na_for` and other recode functions from package `mde`.

- Dropping values using `drop_` from `mde`. 

- A visual summary of missingness is also available. This is currently "just" a visual alternative to the tabular summary of missingness.  

* Added a `NEWS.md` file to track changes to the package.
