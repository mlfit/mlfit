mlfit 0.5.0

## Cran Repository Policy

- [x] Reviewed CRP last edited 2021/05/25

## Test environments

- [x] Checked locally, R 4.1.0
- [x] Checked on CI system, R 4.1.0
- [x] Checked on win-builder, R devel

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 1 notes ✔

* This is a new release.

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Amarin Siripanich <amarin.siri@gmail.com>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  IPF (35:50)
```

## CRAN's comments from the previous submission

> Please reduce the length of the title to less than 65 characters.

- Shorten to 63 characters
```
"Iterative Proportional Fitting Algorithms for Nested Structures"
```

> If there are references describing the methods in your package, please
> add these in the description field of your DESCRIPTION file in the form
> authors (year) <doi:...>
> authors (year) <arXiv:...>
> authors (year, ISBN:...)
> or if those are not available: <https:...>
> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
> auto-linking.
> (If you want to add a title as well please put it in quotes: "Title")

- Added DOIs and URLs as per requested. All references were appropiately cited in the manual.

```
... The fitting algorithms include Iterative Proportional Updating <https://trid.trb.org/view/881554>,
    Hierarchical IPF <doi:10.3929/ethz-a-006620748>, Entropy Optimization <https://trid.trb.org/view/881144>,
    and Generalized Raking <doi:10.2307/2290793>. Additionally, a number of replication methods
    is also provided such as 'Truncate, replicate, sample' <doi:10.1016/j.compenvurbsys.2013.03.004>.
```


> Please add \value to .Rd files regarding exported > methods and explain
> the functions results in the documentation. Please > write about the
> structure of the output (class) and also what the > output means. (If a
> function does not return a value, please document > that too, e.g.
> \value{No return value, called for side effects} or > similar)
> Missing Rd-tags:
>      compute_margins.Rd: \value
>      fitting_problem.Rd:  \value

- Added return tags to `compute_margins()` and `fitting_problem()`.


> Please always add all authors, contributors and > copyright holders in the
> Authors@R field with the appropriate roles.
>  From CRAN policies you agreed to:
> "[....] Where copyrights are held by an entity other > than the package
> authors, this should preferably be indicated via > ‘cph’ roles in the
> ‘Authors@R’ field, or using a ‘Copyright’ field (if > necessary referring
> to an inst/COPYRIGHTS file)."
> e.g.: Andreas Alfons
> Please explain in the submission comments what you > did about this issue.

- Kirill Müller is the copyright holder of the package. This was also there in the previous version. See DESCRIPTION.

``` r
Authors@R: 
    c(
        person(
            given = "Kirill",
            family = "Müller",
            role = c("aut", "cph"),
            comment = "Creator of the package"
        ),
```

## Reverse dependencies

None.