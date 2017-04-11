#' Various signals for your scripts.
#'
#' Several different signals built on \code{message}, \code{warning} and
#' \code{stop} to facilitate various signals you could throw in your scripts
#' or pipelines.
#'
#' Most signals are build from signal text, which is first colored and then
#' passed to \code{message}, \code{warning} or \code{stop}.
#' By default, errors and warnings are
#' light red while messages are light blue. Finally, errors and warnings
#' enable logging their respective text, but you can also use \code{logme}
#' for logging other useful texts or variable states.
#'
#' \code{info} is just colored \code{message} without any other secondary effect.
#'
#' \code{warn} is colored \code{warning}, without call and with immediate
#' effect. It has secondary effect by setting global variable
#' \code{.warning_occurred}, which can be later used to throw \code{stop}
#' signal.
#'
#' \code{warning_occurred} returns TRUE if variable \code{.warning_occurred}
#' exist and its true.
#'
#' \code{stop_if_warn} throw \code{stop} signal if warning occurred. This signal
#' is treated similarly to \code{quit} signal with respect to R quiting with
#' error (e.g., GNU make will recognize it as error and stops).
#'
#' \code{pass} just prints word "pass" in light green. So you or users of your
#' scripts can be calm and happy.
#'
#' \code{logme} will log text to previously initialized log file.
#'
#' \code{log_init} will initialize log file and save this name into global
#' variable \code{.log_file}.
#'
#' \code{error} will print colored message and throw \code{stop} signal.
#'
#' @param text respective message for given signal, must be specified
#' @param color respective color for for given signal, if your terminal supports
#' it.
#' @param log if given message should be also logged to file.
#' @param append if \code{TRUE}, then \code{log_init} won't overwrite existing
#' log file.
#' @return only \code{warning_occurred} returns value, see description.
#'
#' @seealso \code{\link{message}}, \code{\link{warning}}, \code{\link{stop}} for
#' base signals
#'
#' @examples
#' # You could use signals as this:
#' do_something = function(){
#'     # inform what function is doing
#'     info("I am doing something")
#'     # some critical behaviour which you could test:
#'     if(1 == 2){
#'         # inform about error
#'         warn("We might have an error here")
#'         # do something else, for example log current values of parameters
#'         }
#'     # throw error if warning occurred
#'     stop_if_warn()
#'     }
#'
#' do_something()
#'
#' @name signals


#' @rdname signals
#' @export
info = function(text, color="light blue"){
    message(colorize(text, color))
    }

#' @rdname signals
#' @export
warn = function(text, color="light red", log=FALSE){
    .warning_occurred <<- TRUE
    if(log){
        logme(text)
        }
    warning(colorize(text, color), immediate.=TRUE, call.=FALSE)
    }


#' @rdname signals
#' @export
warning_occurred = function(){
    return(exists(".warning_occurred") && .warning_occurred)
    }


#' @rdname signals
#' @export
stop_if_warn = function(color="light red"){
    if(exists(".warning_occurred") && .warning_occurred){
        stop(
            colorize("Stopped execution because warning occurred.", color),
            call.=FALSE
            )
        }
    }


#' @rdname signals
#' @export
pass = function(color="light green"){
    message(colorize("pass\n", color))
    }


#' @rdname signals
#' @export
logme = function(text, color="light red"){
    if(exists(".log_file")){
        cat(text, file=.log_file, append=TRUE, sep="\n")
        } else {
        stop(colorize("Log file was not initialized!", color))
        }
    }


#' @rdname signals
#' @export
log_init = function(file, append=FALSE){
    .log_file <<- file
    cat("", file=.log_file, append=append)
    }


#' @rdname signals
#' @export
error = function(text, color="light red", log=FALSE){
    if(log){
        logme(text)
        }
    stop(colorize(text, color), call.=FALSE)
    }
