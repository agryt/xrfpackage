xrfr
====

Overview
--------

*This package is currently under development.*

This package contains functions to use when importing X-ray fluorescence
(XRF) data and converting the measured values from kilo counts per
second (kcps) to micromolar (µM). It is created with the intention of
being used at the microbiology department at University of Bergen. It
was created with R version 4.0.3.

Installation
------------

The package is installed from GitHub using the remotes package of the
devtools group of packages. If you do not have remotes or devtools
installed, install this first, then use:

``` r
remotes::install_github("agryt/xrfr")

# alternatively:
devtools::install_github("agryt/xrfr")
```

Usage
-----

The xrfr package is meant to be used with the [instructions
document](https://github.com/agryt/xrfr/blob/master/INSTRUCTIONS.pdf),
which explains what your data files must look like for the package to
work. The functions within the package can be used to transform data
from the XRF machine from kcps to µM, using your raw data, information
about the samples, and some basic information about the machine and
elements.

Se vignette for more information and examples of how to use the
different functions: `vignette("xrfr")`.

``` r
library(tidyverse)
library(readxl)
library(xrfr)

rawdata.df <- read_delim(file = system.file("inst/extdata/xrf_rawdata.txt", package = "xrfr"), delim = "\t",
                         locale = locale(decimal_mark = ","))

projectinfo.df <- read_excel(path = system.file("inst/extdata/xrf_projectinfo.xlsx", package = "xrfr"))

baseinfo.df <- read_excel(path = system.file("inst/extdata/xrf_setup.xlsx", package = "xrfr"))
```

The two functions necessary to perform the calculations are `readxrf()`
and `convertxrf()`.

``` r
halfway.df <- readxrf(raw.data = rawdata.df, project.info = projectinfo.df)

calculated.df <- convertxrf(imported.data = halfway.df, base.info = baseinfo.df, year = "2019",
                            first_element = "C", last_element = "As")
```

If you wish to transform your data, you can use the “after” functions.
These are `widen()`, `widen_above()`, `widen_means()`, and
`widen_means_above()`.

``` r
# transforms your data from long to wide format:
wide.df <- widen(project.data = calculated.df)

# same as widen() + excludes values not above the detection limit:
wide.above.df <- widen_above(project.data = calculated.df)

# same as widen() + calculates means based on one or two factors:
wide.means.df <- widen_means(project.data = calculated.df, first_factor = "Day",
                             second_factor = "Treatment")

# combination of widen_means() and widen_above():
wide.means.above.df <- widen_means_above(project.data = calculated.df, first_factor = "Day",
                                         second_factor = "Treatment")
```

You can find examples of what the created data frame of each function
can look like, as well as an example of each data file needed,
[here](https://github.com/agryt/xrfr/tree/master/inst/extdata).

Getting help
------------

If you are struggling to use the package, please see the vignette
(`vignette("xrfr")`) and the [instructions
document](https://github.com/agryt/xrfr/blob/master/INSTRUCTIONS.pdf)
for help. If this does not help, feel free to send your questions to
<a href="mailto:grytaasanna@gmail.com" class="email">grytaasanna@gmail.com</a>.
Any issues with the code can be reported at
<a href="https://github.com/agryt/xrfr/issues" class="uri">https://github.com/agryt/xrfr/issues</a>.