# tribe

[![Travis-CI Build Status](https://travis-ci.org/paulponcet/tribe.svg?branch=master)](https://travis-ci.org/paulponcet/tribe)

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tribe)](http://cran.r-project.org/package=tribe)

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

  - `define` to create or modify attributes; 
  - `keep` to select attributes (and `NULL`ify the others);
  - `rebrand` to rename attributes; 
  - `take` to extract attributes. 

The function `tribe` is a convenient synonym of `attributes`, 
with the slight difference that it always returns a named list. 

```R
library(magrittr)
library(tribe)

# Use 'define' to create or modify attributes
df <- data.frame(x = 1:2, y = 2:3) %>%
  define(example="yes", package="dplyr")
tribe(df)

# Use 'take' to extract attribute values
take(df, names)

# 'take_' is the standard evaluation version of 'take'
take_(df, "class")
take_(df, ~ package)

# Similarly 'define_' is the standard evaluation version of 'define'
df <- df %>%
  define_(package = ~ NULL,
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
 define(example="yes",
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
