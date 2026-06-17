#' Render package Quarto vignettes one by one
#'
#' Renders `.qmd` files in a package vignette directory and returns a summary
#' table showing which files rendered successfully and which failed. This is
#' useful for diagnosing Quarto/pkgdown failures because each vignette is
#' rendered separately and errors are captured rather than stopping the whole
#' build.
#'
#' @param path Character. Directory containing Quarto vignette files. Default is
#'   `"vignettes"`.
#' @param pattern Character. Regular expression used to identify Quarto files.
#'   Default is `"\\.qmd$"`.
#' @param files Optional character vector of files to render. If supplied,
#'   `path` and `pattern` are ignored.
#' @param quiet Logical. Passed to [quarto::quarto_render()]. Default is
#'   `FALSE` so that full render output is printed.
#' @param output_dir Optional output directory passed to
#'   [quarto::quarto_render()]. Default is `NULL`, which lets Quarto use its
#'   standard output location.
#' @param stop_on_error Logical. If `TRUE`, stop after the first failed render.
#'   If `FALSE`, continue rendering all files and return a results table.
#'   Default is `FALSE`.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{file}{Path to the rendered `.qmd` file.}
#'   \item{status}{Either `"ok"` or `"failed"`.}
#'   \item{error}{Error message if rendering failed, otherwise `NA`.}
#' }
#'
#' @examples
#' \dontrun{
#' render_vignettes()
#'
#' render_vignettes(
#'   path = "vignettes",
#'   quiet = FALSE,
#'   stop_on_error = FALSE
#' )
#'
#' render_vignettes(
#'   files = c(
#'     "vignettes/scenario_lizard.qmd",
#'     "vignettes/01_lizard_end_to_end.qmd"
#'   )
#' )
#' }
#'
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#'
#' @export
render_vignettes <- function(
    path = "vignettes",
    pattern = "\\.qmd$",
    files = NULL,
    quiet = FALSE,
    output_dir = NULL,
    stop_on_error = FALSE
) {

  requireNamespace("quarto", quietly = TRUE)

  if (is.null(files)) {
    if (!dir.exists(path)) {
      stop("`path` does not exist: ", path, call. = FALSE)
    }

    files <- list.files(
      path = path,
      pattern = pattern,
      full.names = TRUE
    )
  }

  if (length(files) == 0) {
    return(
      tibble::tibble(
        file = character(),
        status = character(),
        error = character()
      )
    )
  }

  files <- sort(files)

  purrr::map_dfr(files, function(q) {

    message("\n\n==============================")
    message("Rendering: ", q)
    message("==============================\n")

    tryCatch(
      {
        quarto_args <- list(
          input = q,
          quiet = quiet
        )

        if (!is.null(output_dir)) {
          quarto_args$output_dir <- output_dir
        }

        do.call(quarto::quarto_render, quarto_args)

        tibble::tibble(
          file = q,
          status = "ok",
          error = NA_character_
        )
      },
      error = function(e) {

        if (isTRUE(stop_on_error)) {
          stop(e)
        }

        tibble::tibble(
          file = q,
          status = "failed",
          error = conditionMessage(e)
        )
      }
    )
  })
}
