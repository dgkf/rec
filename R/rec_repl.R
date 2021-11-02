# rec accumulator for repl-based use
.rec <- structure(
  new.env(parent = emptyenv()),
  class = "repl_recording"
)

#' @export
rec_repl_begin <- function() {
  rec_repl_env_init()
  rec_repl_update_prompt()
  rec_repl_add_task_callback()
}

#' @export
rec_repl_end <- function() {
  .rec <- get_rec_env()
  .rec$status <- "pause"
  rec_repl_update_prompt()
  rec_repl_remove_task_callback()
}

rec_repl_toggle <- function() {
  .rec <- get_rec_env()
  is_repl_recording <- isTRUE(.rec$status == "record")
  if (!is_repl_recording) {
    rec_repl_begin()
  } else {
    rec_repl_end()
    cli::cat_line(fg_grey("[recording finished]"))
  }
  return(invisible(.rec))
}

rec_repl_remove_task_callback <- function() {
  .rec <- get_rec_env()

  if (!is.null(.rec$callback_id)) {
    try(removeTaskCallback(.rec$callback_id), silent = TRUE)
    rm("callback_id", envir = .rec)
  }

  if (!is.null(.rec$connection)) {
    while (sink.number() >= .rec$sink_number) sink(NULL)
    close(.rec$connection)
    rm("connection", envir = .rec)
    rm("sink_number", envir = .rec)
  }
}

rec_repl_add_task_callback <- function() {
  .rec <- get_rec_env()
  .rec$tapes <- list()
  start <- Sys.time()

  init_tape <- function(envir) {
    new_tape <- data.frame(time = 0L, io = "input", msg = "")[c(), ]
    envir$tape <- new_tape
  }

  append_cnd <- function(cnd, envir) {
    rcrd <- data.frame(time = NA, io = "output", msg = NA)
    rcrd$time <- Sys.time() - start
    rcrd$msg <- list(cnd)
    envir$tape <- rbind(tape, rcrd)
  }

  total_user_time <- function() {
    sum(proc.time()[c(1, 4)])
  }

  fn_env <- environment()
  fn_env$is_rec_call <- TRUE
  fn_env$time <- total_user_time()
  init_tape(fn_env)

  callback <- function(str) {
    append_cnd(str, fn_env)
  }

  .rec$connection <- .Call(R_callback_connection, callback)
  sink(.rec$connection, split = TRUE)
  .rec$sink_number <- sink.number()

  record_callback <- function(expr, value, ok, visible) {
    on.exit(fn_env$time <- total_user_time())
    user_time <- total_user_time() - fn_env$time

    is_rec_call <- fn_env$is_rec_call
    fn_env$is_rec_call <- FALSE
    if (is_rec_call) return(TRUE)

    input_rcrd <- data.frame(time = NA, io = "input", msg = NA)
    input_rcrd$msg <- list(expr)
    input_rcrd$time <- if (nrow(fn_env$tape)) {
      min(fn_env$tape$time[[nrow(fn_env$tape)]] - user_time,
          fn_env$tape$time[[1L]])  # in case user time is fast due to sleeps
    } else {
      Sys.time() - start
    }

    .rec$tapes <- append(.rec$tapes, list(
      new_replay(
        list(NULL),  # don't save outputs of repl recordings
        expr = expr,
        visible = visible,
        traceback = if (!ok) .traceback() else NULL,
        tape = rbind(input_rcrd, fn_env$tape)
      )
    ))

    init_tape(fn_env)
    TRUE
  }

  .rec$callback_id <- addTaskCallback(
    record_callback,
    name = "rec-repl-callback"
  )
}

rec_repl_env_init <- function() {
  .rec <- get_rec_env()

  # clear any existing vars
  if (length(names(.rec)) > 0L)
    rm(list = ls(.rec), envir = .rec)

  .rec$status <- "record"
  .rec$state <- new.env(parent = emptyenv())
  .rec$state$time <- 0L

  if ("opt_old_prompt" %in% names(.rec))
    .rec$opt_old_prompt <- getOption("prompt", "> ")
  if ("opt_old_continue" %in% names(.rec))
    .rec$opt_old_continue <- getOption("continue", "+ ")

  .rec
}

get_rec_env <- function() {
  pkg_ns <- getNamespace(packageName())
  get0(".rec", pkg_ns, inherit = FALSE)
}

rec_repl_update_prompt <- function() {
  .rec <- get_rec_env()
  oldp <- get0("opt_old_prompt", .rec, inherits = FALSE, ifnotfound = "> ")
  oldc <- get0("opt_old_continue", .rec, inherits = FALSE, ifnotfound = "+ ")

  if (isTRUE(.rec$status == "record")) {
    oldp <- paste0(coluni("record"), substring(oldp, 2L))
    oldc <- paste0(cli::col_red("\u205E"), substring(oldc, 2L))
  }

  options(prompt = oldp, continue = oldc)
}
