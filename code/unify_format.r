#' This script unifies format of files.
#' A lot of used files have different formating:
#' present or absent headers
#' different capitalization
#' different separator (tab, space...)
#'
#' This script will try to unify them, so other scripts can assume same format.
library("argparser", quiet=TRUE)

tolower.df = function(dataframe){
    tolower.char_only = function(vec){
        if (is.character(vec)){
            return(tolower(vec))
            } else {
            return(vec)
            }
        }
    return(data.frame(lapply(dataframe, tolower.char_only)))
    }

main = function(args){
    table = read.table(args$filein, header=args$header, sep=args$sep,
                       stringsAsFactors=FALSE, quote=""
                       )
    if(!anyNA(args$columns)){
        if(args$reverse){
            table = table[-args$columns]
            } else {
            table = table[args$columns]
            }
        }
    if(args$header){
        column_names = colnames(table)
        }
    if(!anyNA(args$colnames)){
        column_names = args$colnames
        if(ncol(table) != length(args$colname)){
            stop("Different amount of columns and colnames!")
            }
        }
    # convert to lowercase
    table = tolower.df(table)
    write.table(table, file=args$fileout, quote=FALSE, col.names=column_names,
                row.names=FALSE, sep=" "
                )
    }


args_parser = function(){
    parser = arg_parser("Change format of table based.")
    parser = add_argument(parser, "filein", type="character",
        help="Table that will be reformated."
        )
    parser = add_argument(parser, "fileout", type="character",
        help="Reformated table."
        )
    parser = add_argument(parser, "--header", flag=TRUE,
        help="Table contains header."
        )
    parser = add_argument(parser, "--sep", type="character", default=" ",
        help="separator"
        )
    parser = add_argument(parser, "--colnames", type="character", nargs=Inf,
        help=paste0("Column names of table. Either header or colnames",
            " must be specified."),
        default=NA
        )
    parser = add_argument(parser, "--columns", type="numeric", nargs=Inf,
        help=paste0("Pick only specified columns")
        )
    parser = add_argument(parser, "--reverse", flag=TRUE,
        help="Reverse column argument (specified columns are not picked)"
        )
    args = parse_args(parser)
    return(args)
    }


if(!interactive()){
    args = args_parser()
    if(!args$header && anyNA(args$colnames)){
        stop("Either header or colnames must be specified.")
        }
    if(args$sep == "\\t"){
        args$sep = "\t"
        }
    main(args)
    }
