## Workshop Project folder for Data Processing

This folder contains an RStudio project. You have access to several things:

- All data files used in the main document
- The 'purled' code from the main document
- Interactive exercises


### A note about the data

Some are `.RData` files, while others might be raw `.csv`.  The former require `load('')`, while the latter can be read with `read.csv('')` or `readr::read_csv('')`.  Some are associated with other portions of the document possibly not covered in this workshop.


### A note about the code

The code is purled from the document automatically without any addtional cleaning.  You will see both code plus commented chunk titles, as well as section headers.


### A note about the exercises

To begin with, it would be better to simply read them from the document and try for yourself in your own R script or R Markdown file.


To use the interactive version:

  - You must install the `learnr` package
  - Open the `interactive_exercises_thinking_data_processing.Rmd`
  - Click `Run Document`. By default it will appear in your viewer.
  
  