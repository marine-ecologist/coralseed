# Safely Save R Objects Without Overwriting the File

Wraps [`save`](https://rdrr.io/r/base/save.html). If the file already
exists, it will not be overwritten. A message will be printed indicating
whether the file was successfully written or if it already existed.

## Usage

``` r
save2(..., file = stop("'file' must be specified"))
```

## Arguments

- ...:

  Data frame(s), other object(s), and further arguments, passed on to
  [`save`](https://rdrr.io/r/base/save.html). Required.

- file:

  Path/file name to for output. Required.

## Details

code from https://github.com/stopsack/khsmisc/blob/HEAD/R/datahandling.R
