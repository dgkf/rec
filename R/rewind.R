#' @export
rewind <- function(x) {
  attr(x, ".rec")$state$time <- 0L
  invisible(x)
}
