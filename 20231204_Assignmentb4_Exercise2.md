Exercise 2
================
Torin
2023-12-05

This assignment will convert text to a modified version of piglatin,
called igplatinish. The rules to convert text from English (or any other
language) to igplatinish are as follows:

1.  For words beginning with vowels, the first vowel that follows at
    least one consonant is moved to the beginning of the word, along
    with the letters that immediately follow it (up to and including the
    next consonant).

2.  For words beginning with consonants, the first vowel, and the
    letters immediately following it, are moved to the beginning of the
    word

3.  Words too short or for which these criteria cannot be applied are
    left alone, but criterion 4 below still applies

4.  The sequence ‘ish’ is added to the end of the word

*Setup:*

*Load required packages*

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.4.2     ✔ purrr   1.0.1
    ## ✔ tibble  3.2.1     ✔ dplyr   1.1.3
    ## ✔ tidyr   1.3.0     ✔ stringr 1.5.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## Warning: package 'ggplot2' was built under R version 4.2.3

    ## Warning: package 'tibble' was built under R version 4.2.3

    ## Warning: package 'tidyr' was built under R version 4.2.3

    ## Warning: package 'purrr' was built under R version 4.2.3

    ## Warning: package 'dplyr' was built under R version 4.2.3

    ## Warning: package 'stringr' was built under R version 4.2.3

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(glue)
```

The following code defines a function, atpiglinish, that accepts a
single word as a string, and converts it to atpiglinish according to the
above rules.

``` r
#' Title igplatinish
#' @description This function converts text to a modified version of piglatin, called igplatinish. The rules to convert text from English (or any other language) to igplatinish are as follows: (1) For words beginning with vowels, the first vowel that follows at least one consonant is moved to the beginning of the word, along with the letters that immediately follow it (up to and including the next consonant) (2) For words beginning with consonants, the first vowel, and the letters immediately following it, are moved to the beginning of the word. (3) Words too short or for which these criteria cannot be applied are left alone, but criterion 4 below still applies. (4) The sequence 'ish' is added to the end of the word
#'
#' @param x a single string containing only letters
#'
#' @return `result` the modified word, translated to atpiglinish
#' @export 
#'
#' @examples igplatinish("piglatin")



igplatinish <- function(x) {
  #Throw an error if the input is not a character
  if (!is.character(x) == TRUE) {
    stop("Input must be of type character.")
  }
  
  #Ensure the function input is alphabetical and contains no spaces, hyphenated words, punctuation or other symbols.
  if (str_detect(x, "[^a-zA-Z]") == TRUE) {
    stop("Input must be a single word compoased of only letters with no punctuation.")
  }
  
  #If the word is too short or the criteria cannot be applied, return the word unchanged as 'output'. The word must be at least three letters and contain at least one consonant-vowels-consonant sequence (containing one or several consecutive vowels)
  #Note that the addition criterion will still occur at the end for these words
  if (str_length(x) < 3) {
    output <- x
  }
  
  else if (is.na(str_extract(x, "[^aeiouAEIOU]([aeiouAEIOU]+)([^aeiouAEIOU])")) == TRUE) {
    output <- x
  }
  
  
  #Define whether the word begins with a vowel or a consonant
  else if (str_detect(x, "^[aeiouAEIOU]") == TRUE) {
    
    #For words beginning with vowels, the first vowel that follows at least one consonant is moved to the beginning of the word, along with the letters that immediately follow it (up to and including the next consonant)
    
    
    motif <- str_extract(x, "[^aeiouAEIOU]([aeiouAEIOU]+)([^aeiouAEIOU])")
    
    #This extracted the consonant-vowels-consonant sequence, but we only wish to replace the vowel(s) (we needed to extract the whole sequence to ensure that the vowel(s) follow a consonant). Now, we will extract the vowel and the letters that follow it using a second extraction from the first extract.
    
    motif1 <- str_extract(motif, "([aeiouAEIOU]+)([^aeiouAEIOU])")
    x1 <- str_replace(x, glue("{motif1}"), "")
    output <- paste0(motif1, x1)
  }
  
  else if (str_detect(x, "^[^aeiouAEIOU]") == TRUE) {
    
    #For words beginning with consonants, the first vowel and subsequent consonant are moved to the beginning.
    
    motif <- str_extract(x, "(([aeiouAEIOU]+)([^aeiouAEIOU]))")
    x1 <- str_replace(x, glue("{motif}"), "")
    output <- paste0(motif, x1)
    
  }
  
  
  #Add 'ish' to the end of the new word
  result <- paste0(output, "ish")
  print(result)
}
```

# Examples

``` r
#Example 1 - convert a word beginning with a consonant to igplatinish
igplatinish("piglatin")
```

    ## [1] "igplatinish"

``` r
#Example 2 - convert a word beginning with a vowel to igplatinish
igplatinish("animal")
```

    ## [1] "imanalish"

``` r
#Example 3 - convert a <3-letter word to igplatinish, which will simply add the suffix 'ish'
igplatinish("it")
```

    ## [1] "itish"

# Tests

``` r
library(testthat)
```

    ## Warning: package 'testthat' was built under R version 4.2.3

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null

    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

``` r
# Test 1 - test that the function returns an error when the input is not a string
test_that("numeric", {expect_error(igplatinish(134))})
```

    ## Test passed 🎊

``` r
# Test 2 - test that the function returns an error when the input contains non-alphabetic characters
test_that("symbols", {expect_error(igplatinish("piglatin!"))})
```

    ## Test passed 🎊

``` r
# Test 3 - test that the number of letters in the output is three more than in the input (i.e. we are moving letters to the front, not removing them, then adding three letters as 'ish')
test_that("position", {expect_equal(str_length(igplatinish("piglatin")) - 3, str_length("piglatin"))})
```

    ## [1] "igplatinish"
    ## Test passed 🌈
