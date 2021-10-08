Added analysisscript.rmd, updated results folder, added data to data folder, updated Manuscript.Rmd

# Template structure

* All data goes into the subfolders inside the `data` folder.
* All code goes into the `code` folder or subfolders.
* All results (figures, tables, computed values) go into `results` folder or subfolders.
* All products (manuscripts, supplement, presentation slides, web apps, etc.) go into `products` subfolders.
* See the various `readme.md` files in those folders for some more information.


* The `processing_code` folder contains a single R script which loads the raw data, performs a bit of cleaning, and saves the result in the `processed_data` folder.
* The `analysis_code` folder contains an R script which loads the processed data, fits a simple model, and produces a figure and some numeric output, which is saved in the `results` folder.
* The `products` folder contains an example `bibtex` and CSL style file for references. Those files are used by the example manuscript, poster and slides.
* The `poster` and `slides` folders contain very basic examples of posters and slides made with R Markdown. Note that especially for slides, there are many different formats. You might find a different format more suitable. Check the R Markdown documentation. 
* The  `manuscript` folder contains a template for a report written in Rmarkdown (bookdown, to be precise). If you access this repository as part of [my Modern Applied Data Science course](https://andreashandel.github.io/MADAcourse/), the sections are guides for your project. If you found your way to this repository outside the course, you might only be interested in seeing how the file pulls in results and references and generates a word document as output, without paying attention to the detailed structure.
