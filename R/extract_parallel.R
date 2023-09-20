#' Settlement statistics
#'
#' Function to extract points and paths from multiple %dopar% outputs
#'
#' @param input list of %dopar% outputs
#' @param type either points or paths
#' @param ... pass arguments
#' @export
#'
#'
#'
globalVariables("id")
globalVariables("id_full")

extract_parallel <- function(input, type, ...) {
  if (type == "points") {
    tmp <- do.call(rbind, lapply(input, function(x) x[[1]])) |>
      dplyr::mutate(id_full = id) |>
      dplyr::mutate(id = as.factor(substr(id_full, 1, 3)))
    return(tmp)
  } else if (type == "paths") {
    tmp <- do.call(rbind, lapply(input, function(x) x[[2]])) |>
      dplyr::mutate(id_full = id) |>
      dplyr::mutate(id = as.factor(substr(id_full, 1, 3)))
    return(tmp)
  } else {
    print("Must be either 'points' or 'paths'")
  }
}
