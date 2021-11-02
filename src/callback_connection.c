#include <stdlib.h>
#include <Rinternals.h>
#include <R_ext/Connections.h>

#if ! defined(R_CONNECTIONS_VERSION) || R_CONNECTIONS_VERSION != 1
#error "Unsupported connections API version"
#endif

typedef struct callback {
  SEXP fn;
} callback;

static Rboolean callback_con_open(Rconnection con) {
  con->isopen = TRUE;
  return TRUE;
}

static size_t callback_con_write(const void *ptr, size_t size, size_t nitems,
  Rconnection con) {

  callback *cb = (callback*) con->private;

  SEXP R_fn_call;
  SEXP r_text = PROTECT(allocVector(STRSXP, 1));
  SET_STRING_ELT(r_text, 0, mkChar(ptr));

  R_fn_call = PROTECT(lang2(cb->fn, r_text));
  eval(R_fn_call, R_GlobalEnv);

  UNPROTECT(2);
  return 1;
}

static void callback_con_close(Rconnection con) {
  con->isopen = FALSE;
}

void callback_con_destroy(Rconnection con) {
  callback *cb = (callback*) con->private;
  free(cb);
}


SEXP R_callback_connection(SEXP callback_fn) {
  if(!isFunction(callback_fn))
    error("Parameter 'callback' must be a function");

  Rconnection con;
  SEXP rc = PROTECT(
    R_new_custom_connection(
      "Callback connection",
      "r",
      "Rcallback",
      &con
    )
  );

  callback *cb = (callback*) malloc(sizeof(callback));
  cb->fn = callback_fn;

  // set connection properties
  con->incomplete = FALSE;
  con->private = cb;
  con->canseek = FALSE;
  con->canwrite = TRUE;
  con->canread = FALSE;
  con->isopen = TRUE;
  con->blocking = FALSE;
  con->text = TRUE;
  con->UTF8out = TRUE;
  con->open = callback_con_open;
  con->close = callback_con_close;
  con->destroy = callback_con_destroy;
  con->write = callback_con_write;

  UNPROTECT(1);
  return rc;
}
