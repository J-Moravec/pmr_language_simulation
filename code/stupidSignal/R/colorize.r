#' Colorize text
#'
#' If your terminal supports colors, this will colorize your message!
#'
#' This simple function is mostly stolen from \code{sand::colourise}.
#' See: \url{https://github.com/kolaczyk/sand}.
#' Some code is missing, for example, checks if terminal support coloring is not added.
#'
#'
#' @param text text that you want colorise
#' @param color one of colors from \code{.colors}, see stupidCheck::.colors
#'
#' @return text with tags which will colorize text if printed onto terminal
#'
#' @examples
#' simple_text = colorize("I am a simple text.", color="light blue")
#'
#' # Prints text with coloring tags:
#' print(simple_text)
#'
#' # If your terminal supports colors, prints colored text:
#' cat(simple_text, "\n")
#'
#' @export
colorize = function(text, color="red"){
    if(!color %in% names(.colors)){
        stop(
            "Wrong color specification. Use these colors:\n",
            paste0(.colors, collapse="\n"), "\n",
            )
        }

    col_escape = function(col){
        return(paste0("\033[", col, "m"))
        }

    col = .colors[tolower(color)]
    init = col_escape(col)
    reset = col_escape("0")
    return(paste0(init, text, reset))
    }


#' @export
.colors = c(
    "black" = "0;30",
    "blue" = "0;34",
    "green" = "0;32",
    "cyan" = "0;36",
    "red" = "0;31",
    "purple" = "0;35",
    "brown" = "0;33",
    "light gray" = "0;37",
    "dark gray" = "1;30",
    "light blue" = "1;34",
    "light green" = "1;32",
    "light cyan" = "1;36",
    "light red" = "1;31",
    "light purple" = "1;35",
    "yellow" = "1;33",
    "white" = "1;37"
    )
