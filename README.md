xrfr
====

*This package is currently under development.*

Overview
--------

This package contains functions to use when importing X-ray fluorescence
(XRF) data and converting the measured values from kilo counts per
second (kcps) to micromolar (µM). It is created with the intention of
being used at the microbiology department at University of Bergen. It
was created with R version 4.0.3, and is a further development of Hedda
Østgaard’s [XRF package](https://github.com/heddaost/XRF).

Installation
------------

The package is installed from GitHub using the remotes package of the
devtools group of packages. If you do not have remotes or devtools
installed, install this first, then use the function `install_github()`:

``` r
install.packages("remotes")
remotes::install_github("agryt/xrfr", build_vignettes = TRUE)

# alternatively:
install.packages("devtools")
devtools::install_github("agryt/xrfr", build_vignettes = TRUE)
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

The two functions necessary to perform the calculations are `readxrf()`
and `convertxrf()`.

``` r
halfway.df <- readxrf(raw_data = rawdata.df, project_info = projectinfo.df)

calculated.df <- convertxrf(imported_data = halfway.df, base_info = baseinfo.df, year = "2019",
                            first_element = "C", last_element = "As")
```

If you wish to transform your data, you can use the “after” functions.
These are `widen()`, `widen_above()`, `widen_means()`, and
`widen_means_above()`.

``` r
# transforms your data from long to wide format:
wide.df <- widen(project_data = calculated.df)

# same as widen() + excludes values not above the detection limit:
wide.above.df <- widen_above(project_data = calculated.df)

# same as widen() + calculates means based on one or two factors:
wide.means.df <- widen_means(project_data = calculated.df, first_factor = "Day",
                             second_factor = "Treatment")

# combination of widen_means() and widen_above():
wide.means.above.df <- widen_means_above(project_data = calculated.df, first_factor = "Day",
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
