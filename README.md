# tribe

[![Travis-CI Build Status](https://travis-ci.org/paulponcet/tribe.svg?branch=master)](https://travis-ci.org/paulponcet/tribe) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/tribe)](https://cran.r-project.org/package=tribe) [![](https://cranlogs.r-pkg.org/badges/tribe)](https://cran.r-project.org/package=tribe)


Play with the tribe of attributes in R. 


## Installation

You can install tribe from GitHub with:

```R
# install.packages("devtools")
devtools::install_github("paulponcet/tribe")
```

## Verbs for easy manipulation of attributes

This package provides verbs for easy manipulation of attributes. 
These verbs are: 

  - `at_mutate` to create or modify attributes; 
  - `at_select` to select attributes (and `NULL`ify the others);
  - `at_rename` to rename attributes; 
  - `at_slice` to extract attributes. 

The function `tribe` is a convenient synonym of `attributes`, 
with the slight difference that it always returns a named list. 

```R
library(magrittr)
library(tribe)

# Use 'define' to create or modify attributes
df <- data.frame(x = 1:2, y = 2:3) %>%
  at_mutate(example="yes", package="dplyr")
tribe(df)

# Use 'at_slice' to extract attribute values
at_slice(df, names)

# 'at_slice_' is the standard evaluation version of 'at_slice'
at_slice_(df, "class")
at_slice_(df, ~ package)

# Similarly 'at_mutate_' is the standard evaluation version of 'at_mutate'
df <- df %>%
  at_mutate_(package = ~ NULL,
             example = ~ "no")
tribe(df)
```

## A new pipe that preserves attributes

The tribe package provides you with a new pipe `%@>%` that 
enables propagation of attributes.  

```R
library(dplyr)
library(tribe)

df <- data.frame(x = 1:2, y = 2:3) %>%
 at_mutate(example="yes",
           package="tribe", 
           class = c("my_tbl", "data.frame"))

# Attributes just created are lost when the object 
# passes through dplyr verbs
tribe(df %>% mutate(z=3))

# With the pipe '%@>%', most attributes are kept
tribe(df %@>% mutate(z=3))

# One can create a new pipe to adjust attributes propagation settings
"%newpipe>%" <- make_pipe(propagate="none", keep_also = "example")
tribe(df %newpipe>% mutate(z=3))
```
