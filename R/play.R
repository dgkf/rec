#' @export
play <- function(x, ...) {
  if (missing(x)) return(play(rec:::get_rec_env(), ...))
  UseMethod("play")
}

#' @export
play.repl_recording <- function(x, ...) {
  tape <- do.call(rbind, lapply(x$tapes, function(i) attr(i, ".rec")$tape))
  play(as_recording_tape_df(tape), ..., state = x$state)
}

#' @export
play.recording <- function(x, ...) {
  tape <- attr(x, ".rec")$tape
  state <- attr(x, ".rec")$state
  play(tape, ..., state = state)
}

#' @export
play.recording_tape_df <- function(x, step = 0.05, speed = 1,
  now = state$time %||% 0, rewind = TRUE, state = NULL) {

  x <- spoof_typing_io(x)
  i <- head(c(which(x$time >= now), 1L), 1L)

  line <- character(20L)
  line_i <- 0L
  duration <- tail(x$time, 1L)
  status_id <- cli::cli_status(format_replay_timeline("play", 0, duration))

  on.exit({
    # flush final line
    cat(line[0L:line_i], sep = "")
    cli::cli_status_clear(status_id)

    # store tape state after play
    if (is.environment(state)) {
      state$time <- if (i <= nrow(x)) x[[i, "time"]]
        else if (isTRUE(rewind)) 0
        else as.numeric(tail(x$time, 1L))
    }
  })

  start <- Sys.time()
  tryCatch(
    while (i <= nrow(x)) {
      step_start <- Sys.time()
      now <- (step_start - start + step) * speed
      while (i <= nrow(x) && x[[i, "time"]] < now) {
        msg_i <- x[[i, "msg"]]
        switch(x[[i, "io"]],
          typing = {
            cli::cli_status_update(status_id, "")
            start_time <- x[[i, "time"]]
            entry_time <- x[[i+1, "time"]]
            now <- start_time + animate_typed_code_block(
              paste0(collapse = "\n", deparse(msg_i)),
              step = step,
              speed = speed,
              duration = entry_time - start_time,
              cli_status_clear_callback = function(t) {
                cli::cli_status_update(status_id, "")
              },
              cli_status_update_callback = function(t) {
                status <- format_replay_timeline("play", now + t, duration)
                cli::cli_status_update(status_id, status)
              })
          },
          input = {
            # "input" events just denote the entry of code, actual display is
            # handled by a "typing" event, which is inferred from code entry
          },
          output = {
            if (inherits(msg_i, "condition")) {
              if (inherits(msg_i, "message")) message(msg_i)
              else if (inherits(msg_i, "warning")) warning(msg_i)
            } else {
              if (grepl("\n", msg_i)) {
                # unset status bar before flushing line
                cli::cli_status_update(status_id, "")
                cat(line[0L:line_i], sep = "")
                cat(gsub("\n.*$", "\n", msg_i))
                line[] <- ""
                line_i <- 0L
              }
              line_i <- line_i + 1L
              line[line_i] <- gsub(".*\n", "", msg_i)
            }
          })

        i <- i + 1L
      }

      # return to status line while we buffer the next line
      status <- format_replay_timeline("play", now, duration)
      cli::cli_status_update(status_id, status)
      Sys.sleep(max(step - as.numeric(Sys.time() - step_start), 0))
    },
    interrupt = function(cond) {
      status <- paste0(
        fg_grey("["), coluni("pause"),
        fg_grey("]",
          sprintf(" @ %s (%.f%%)",
            format_difftime(now),
            x[[min(i, nrow(x)), "time"]] / as.numeric(duration) * 100
          )))
      cli::cli_status_update(status_id, "")
      cli::cat_line(paste0("\r", status))
    })
}


animate_typed_code_block <- function(code,
  step = 0.05, speed = 1, duration = 1,
  use_prompt = TRUE, use_continue = TRUE,
  cli_status_clear_callback = identity,
  cli_status_update_callback = identity) {

  ansi_prev_line <- "\033[F"

  code <- paste0(collapse = "\n", code)
  prompt  <- if (use_prompt) getOption("prompt", "> ") else ""
  promptc <- if (use_continue) getOption("continue", "+ ") else ""

  start <- Sys.time()
  now <- 0L

  chars_per_sec <- nchar(code) / duration
  i <- 1L
  i_next <- as.numeric(now) * chars_per_sec
  line <- character(1L)
  while (round(i_next) < nchar(code)) {
    step_start <- Sys.time()
    now <- (step_start - start + step) * speed
    i_next <- as.numeric(now) * chars_per_sec

    # clear status line below and return to text line for input
    cat("\n")
    cli_status_clear_callback(now)
    cat(ansi_prev_line)

    # print completed lines and start aggregating next line
    code_i <- substring(code, round(i), round(i_next))
    if (i == 1L) code_i <- paste0(prompt, code_i)
    if (grepl("\n", code_i)) {
      code_i <- gsub("\n", paste0("\n", promptc), code_i)
      cat(line, " ", gsub("(.*\n).*", "\\1", code_i), sep = "")
      line <- gsub(".*\n", "", code_i)
    } else {
      line <- paste0(line, code_i)
    }
    cat(line, uni$bar[[8L]], sep = "")

    # update status line below and return to text line for input
    cat("\n")
    cli_status_update_callback(now)
    cat(ansi_prev_line)

    i <- i_next + 1L
    Sys.sleep(max(step - as.numeric(Sys.time() - step_start), 0))
  }
  cat("\r", line, " \n", sep = "")
  return(now)
}


format_code_block_pretty <- function(expr, lines = deparse(expr),
  use_prompt = TRUE, use_continue = FALSE) {

  prompt  <- if (use_prompt) getOption("prompt", "> ") else ""
  promptc <- if (use_continue) getOption("continue", "+ ") else ""

  # trim ws, and crop prompt chars
  promptn <- max(nchar(trimws(c(prompt, promptc), "right")))
  prompt  <- substring(prompt, 1L, promptn)
  prompt  <- paste0(prompt, strrep(" ", max(promptn - nchar(prompt), 0)))
  promptc <- substring(promptc, 1L, promptn)
  promptc <- paste0(promptc, strrep(" ", max(promptn - nchar(promptc), 0)))

  w <- nchar(length(lines))

  lines[[1L]] <- paste0(
    bg_grey(prompt, strrep(" ", w + 2L), sep = ""),
    " ",
    lines[[1L]]
  )

  lines[-1L]  <- paste0(
    bg_grey(promptc,
      strrep(" ", w - nchar(i <- seq_along(lines[-1L]) + 1L) + 1L),
      i,
      " ",
      sep = ""),
    " ",
    lines[-1L]
  )

  lines
}


format_code_block <- function(expr, lines = deparse(expr)) {
  prompt  <- getOption("prompt", "> ")
  promptc <- getOption("continue", "+ ")
  lines[[1L]] <- paste0(prompt, lines[[1L]])
  lines[-1L] <- paste0(promptc, lines[-1L])
  lines
}


format_difftime <- function(s) {
  if (s < 60)
    sprintf("%.0fs", s)
  else if (s < 60^2)
    sprintf("%.0fm:%02.0fs", s %/% 60, s %% 60)
  else if (s < 60^3)
    sprintf("%.0fh:%02.0fm:%02.0fs", s %/% 60^2, (hr <- s %% 60^2) %/% 60, hr %% 60)
}


format_replay_timeline <- function(status, set = 0L, total) {
  w <- getOption("width", 80L)
  set <- as.numeric(set)
  total <- as.numeric(total)

  if (status %in% names(coluni()))
    status <- coluni(status)

  fset <- format_difftime(set)
  ftot <- format_difftime(total)
  lcont <- paste0(fg_grey("["), status, fg_grey("]"))
  rcont <- sprintf(" %s%s / %s ", strrep(" ", nchar(ftot) - nchar(fset)), fset, ftot)

  w <- w - cli::ansi_nchar(lcont) - cli::ansi_nchar(rcont)
  nprog <- floor(perc <- w * min(set / total, 1, na.rm = TRUE))
  nprogr <- perc - nprog
  nbar <- length(uni$bar)

  ccont <- paste0(collapse = "", c(
    bg_grey(strrep(" ", max(nprog - 1L, 0L))),
    if (nprog >= 1L)
      fg_grey(bg_white(uni$bar[[max(ceiling(nprogr * nbar), 1L)]])),
    fg_white(uni$bar[[max(ceiling(nprogr * nbar), 1L)]]),
    strrep(" ", max(w - nprog - 1L, 0L))
  ))

  paste0(lcont, ccont, rcont)
}



#' Spoof typing events in a recording
#'
#' Create a spoofed "start of typing" event before each input event in the tape
#'
#' @param start_typing_delay Delay after preceeding io event before
#'   start-of-typing event. A fractional value, indicating a proportion of the
#'   time between previous event and execution of the input.
#'
spoof_typing_io <- function(tape, start_typing_delay = 0.2) {
  # duplicate "input" entry records (stamped at time of entry)
  i_input <- which(tape$io == "input")
  tape <- tape[sort(c(seq_len(nrow(tape)), i_input)), ]
  rownames(tape) <- NULL
  i_typing <- i_input + seq_along(i_input) - 1L

  # assign earlier duplicated record as a "typing" record and lag previous time
  tape$io[i_typing] <- "typing"
  tape$time[i_typing[i_typing == 1]] <- 0
  tape$time[i_typing[i_typing >  1]] <- tape$time[(i_typing - 1)[i_typing >  1]]

  # interpolate start of typing event by delay time
  typing_time <- tape$time[i_typing + 1] - tape$time[i_typing]
  tape$time[i_typing] <- tape$time[i_typing] + typing_time * start_typing_delay
  tape
}
