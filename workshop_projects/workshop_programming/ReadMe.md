## Workshop Project folder for the Programming Section

This folder contains an RStudio project. You have access to several things:

- All data files used in the notes
- The 'purled' code from the notes
- Interactive exercises


### A note about the data

Some are `.RData` files, while others might be raw `.csv`.  The former require `load('')`, while the latter can be read with `read.csv('')` or `readr::read_csv('')`.  Some are associated with other portions of the document possibly not covered in the workshop.

### A note about the code

The code is purled from the document automatically without any additional cleaning.  You will see both code plus commented chunk titles, as well as section headers.


### A note about the exercises

To begin with, it would be better to simply read the notes and try for yourself in your own R script or R Markdown file.  However, an 'interactive' version is also provided in the exercises folder. When you run the document, by default it will appear in your viewer. But you can click the key to pop it open in your browser by clicking the 'Show in new window' button. Exercises contain code hints that you can copy into the code area and run.


**To use the interactive version**:

  - You must install the `learnr` package.  If you haven't already, you should also install the `tidyverse`.  Just run the install_script.R script.
  - Open the `interactive_exercises_tidyverse.Rmd`
  - Click `Run Document`. 
  
  