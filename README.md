# STAT545 Assignment B4

## Option A

_Torin Halvorson, 2023-12-05_

This assignment comprised two exercises.

1. Exercise 1 - the goal here was to make a plot of the most common words in the `prideprejudice` dataset of the `janeaustenr` package

2. Exercise 2 - the goal here was to create a function that would convert words to a modified version of piglatin, named 'igplatinish'


# Files

Exercise 1:

1. 20231204_Assignmentb4_Exercise1.Rmd
  - This file contains the code for Exercise 1, above
  - The prideprejudice dataset was selected, and stopwords were removed using the `stop_words` dataset of the `tidytext` package (https://cran.r-project.org/web/packages/tidytext/index.html) prior to plotting in a barplot
  
2. 20231204_Assignmentb4_Exercise1.md
  - The corresponding output .md file for Exercise 1
  
3. 20231204_Assignmentb4Exercise1_files
  - The files produced by running code from Exercise 1
  

Exercise 2:

1. 20231204_Assignmentb4_Exercise2.Rmd
  - This file contains the code for Exercise 2, above
  - The function defined here, `igplatinish()`, takes a single string consisting exclusively of letters and converts the word to igplatinish
  - Several examples and tests are also provided
  
2. 20231204_Assignmentb4_Exercise2.md
  - The corresponding output .md file for Exercise 2
  
Others:

1. assignment-b4-torinhalvorson.Rproj
  - The corresponding R project file for the complete assignment
