#' encode states based on coding scheme
#'
#' This function will take two files:
#'  matrix of names and post-marital residence
#'  matrix of coding scheme
#' and substitute post-marital residence by this coding scheme

library("argparser", quiet=TRUE)
library("simpleSignal", quiet=TRUE)

read_file = function(filepath){
    data = read.table(filepath, sep=" ", header=TRUE, stringsAsFactors=FALSE)
    return(data)
    }


get_recoding_vector = function(coding_scheme){
    recoding_table = read_file(coding_scheme)
    recoding_vector = recoding_table[,2]
    names(recoding_vector) = recoding_table[,1]
    return(recoding_vector)
    }


recode = function(table, recoding_vector){
    table[,2] = recoding_vector[table[,2]]
    return(table)
    }


main = function(args){
    residence_table = read_file(args$residence)
    recoding_vector = get_recoding_vector(args$coding_scheme)
    recoded = recode(residence_table, recoding_vector)
    if(anyNA(recoded)){
        text = paste0(
            "Error in recoding, produced NA values.",
            " Check if recoding tables contain headers or if all", 
            " symbols are being recoded and try again."
            )
        simpleSignal::error(text)
        }

    write.table(recoded, file=args$recoded, col.names=args$header,
                row.names=FALSE, sep=" ", quote=FALSE
                )
    }


args_parser = function(){
    parser = arg_parser(
        paste0("Recode post-marital residence according to coding scheme")
        )
    parser = add_argument(parser, "residence", type="character",
        help="Original coding or residence table"
        )
    parser = add_argument(parser, "coding_scheme", type="character",
        help="Table with coding scheme"
        )
    parser = add_argument(parser, "recoded", type="character",
        help="File name for renamed table"
        )
    parser = add_argument(parser, "--header", flag=TRUE,
        help="Header in recoded file?"
        )
    args = parse_args(parser)
    return(args)
    }




if(!interactive()){
    args = args_parser()
    main(args)
    }
