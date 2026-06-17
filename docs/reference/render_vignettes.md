# Render package Quarto vignettes one by one

Renders `.qmd` files in a package vignette directory and returns a
summary table showing which files rendered successfully and which
failed. This is useful for diagnosing Quarto/pkgdown failures because
each vignette is rendered separately and errors are captured rather than
stopping the whole build.

## Usage

``` r
render_vignettes(
  path = "vignettes",
  pattern = "\\.qmd$",
  files = NULL,
  quiet = FALSE,
  output_dir = NULL,
  stop_on_error = FALSE
)
```

## Arguments

- path:

  Character. Directory containing Quarto vignette files. Default is
  `"vignettes"`.

- pattern:

  Character. Regular expression used to identify Quarto files. Default
  is `"\\.qmd$"`.

- files:

  Optional character vector of files to render. If supplied, `path` and
  `pattern` are ignored.

- quiet:

  Logical. Passed to
  [`quarto::quarto_render()`](https://quarto-dev.github.io/quarto-r/reference/quarto_render.html).
  Default is `FALSE` so that full render output is printed.

- output_dir:

  Optional output directory passed to
  [`quarto::quarto_render()`](https://quarto-dev.github.io/quarto-r/reference/quarto_render.html).
  Default is `NULL`, which lets Quarto use its standard output location.

- stop_on_error:

  Logical. If `TRUE`, stop after the first failed render. If `FALSE`,
  continue rendering all files and return a results table. Default is
  `FALSE`.

## Value

A tibble with columns:

- file:

  Path to the rendered `.qmd` file.

- status:

  Either `"ok"` or `"failed"`.

- error:

  Error message if rendering failed, otherwise `NA`.

## Examples

``` r
if (FALSE) { # \dontrun{
render_vignettes()

render_vignettes(
  path = "vignettes",
  quiet = FALSE,
  stop_on_error = FALSE
)

render_vignettes(
  files = c(
    "vignettes/scenario_lizard.qmd",
    "vignettes/01_lizard_end_to_end.qmd"
  )
)
} # }
```
