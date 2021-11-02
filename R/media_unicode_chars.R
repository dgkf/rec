uni <- list(
  record = "\u23FA",
  play = "\u25B6",
  pause = "\u23F8",
  bar = list(
    " ",       # :0/8
    "\u258F",  # :1/8
    "\u258E",  # :2/8
    "\u258D",  # :3/8
    "\u258C",  # :4/8
    "\u258B",  # :5/8
    "\u258A",  # :6/8
    "\u2589",  # :7/8
    "\u2588"   # :8/8
  )
)

coluni <- function(x) {
  out <- list(
    record = cli::col_red(uni$record),
    play = cli::col_blue(uni$play),
    pause = cli::col_blue(uni$pause)
  )

  if (!missing(x)) out[x]
  else out
}
