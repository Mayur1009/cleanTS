## Resubmission

> Please do not start the description with "This package", package name,
title or similar.

Update the description of the package.

> If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

There are no references to include regarding the package.

> Please write TRUE and FALSE instead of T and F. (Please don't use 'T' or
'F' as vector names.), e.g.:
   man/cleanTS.Rd:
        cleanTS(
          data,
          date_format,
          imp_methods = c("na_interpolation", "na_locf", "na_ma", "na_kalman"),
          time = NULL,
          value = NULL,
          replace_outliers = T
        )

Replaced the 'T' with 'TRUE'.

> Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)
Missing Rd-tags:
      gen.animation.Rd: \value
      gen.report.Rd: \value
      interact_plot.Rd: \value
      print.cleanTS.Rd: \value
      
These functions do not return any value. Updated the documentation accordingly.



## Test environments
* local Ubuntu 21.04, R 4.1.1
* mac OS latest (on github-actions), R 4.1.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission
