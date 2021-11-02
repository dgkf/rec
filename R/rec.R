new_replay <- function(value, expr, visible, traceback, tape) {
  state <- new.env(parent = emptyenv())
  state$time <- 0L

  structure(
    list(value),
    .rec = list(
      visible = visible,
      traceback = traceback,
      tape = as_recording_tape_df(tape),
      state = state
    ),
    class = "recording"
  )
}

as_recording_tape_df <- function(df) {
  new_class <- "recording_tape_df"
  if (!new_class %in% class(df))
    class(df) <- c(new_class, class(df))
  df
}

#' @useDynLib rec R_callback_connection
#' @export
rec <- function(expr, env = parent.frame(), quiet = FALSE, quoted = FALSE) {
  # if no expression is passed, toggle repl-based recording
  if (missing(expr)) {
    return(invisible(rec_repl_toggle()))
  }

  expr_quote <- substitute(expr)

  cnds_err_traceback <- NULL

  start <- Sys.time()
  tape <- data.frame(time = 0L, io = "input", msg = NA)
  tape$msg <- list(expr_quote)

  append_cnd <- function(cnd, envir) {
    rcrd <- data.frame(time = NA, io = "output", msg = NA)
    rcrd$time <- list(Sys.time() - start)
    rcrd$msg <- list(cnd)
    assign("tape", rbind(tape, rcrd), envir = envir)
  }

  n_calls_top <- 8 + length(sys.calls())
  n_calls_bot <- 5L

  fn_env <- environment()
  callback <- function(str) append_cnd(str, fn_env)
  con <- .Call(R_callback_connection, callback)
  sink(con, split = !quiet)
  on.exit(try(close(con), silent = TRUE))

  cond_fn <- function(cnd) {
    if (inherits(cnd, "message") || inherits(cnd, "warning")) {
      calls <- sys.calls()
      calls <- utils::head(utils::tail(calls, -n_calls_top), -n_calls_bot)
      cnd$call <- if (length(calls) > 1) calls[[length(calls) - 1]] else NULL
      append_cnd(cnd, fn_env)
      if (quiet) invokeRestart(computeRestarts()[[1]])
    } else if (inherits(cnd, "error")) {
      # trim call stack back to just the scope of the evaluated expression
      calls <- sys.calls()
      calls <- utils::head(utils::tail(sys.calls(), -n_calls_top), -2L)
      cnd$call <- if (length(calls) > 1) calls[[length(calls) - 1]] else NULL
      append_cnd(cnd, fn_env)
      assign("cnds_err_traceback", rev(calls), envir = fn_env)
    } else {
      append_cnd(cnd, fn_env)
    }
    cnd
  }

  expr_with_handlers <- bquote(withCallingHandlers(
    .(expr_quote),
    condition = .(cond_fn)
  ))

  res <- withVisible(tryCatch(
    eval(expr_with_handlers, envir = env),
    error = function(e) e
  ))

  sink(NULL)
  close(con)

  new_replay(
    res$value,
    expr = if (!quoted) expr_quote else expr,
    visible = res$visible,
    traceback = cnds_err_traceback,
    tape = tape
  )
}
