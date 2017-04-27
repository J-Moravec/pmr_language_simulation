#' Read two column matrix and change names according to another two
#' another two column matrix.
library("argparser", quiet=TRUE)
library("simpleSignal")

get_renaming_vector = function(renaming_file){
    table = read.table(renaming_file, sep=" ", stringsAsFactors=FALSE,
                       header=TRUE
                       )
    renaming_vector = table[, 2]
    names(renaming_vector) = table[, 1]
    return(renaming_vector)
    }




rename = function(original_table, renaming_vector){
    which_rename = original_table[, 1] %in% names(renaming_vector)
    # match them
    original_table[which_rename, 1] = renaming_vector[original_table[which_rename, 1]]
    return(original_table)
    }


main = function(args){
    # get original table
    original_table = read.table(args$original_table, header=FALSE, sep=" ",
        stringsAsFactors=FALSE)
    # get renaming vector
    renaming_vector = get_renaming_vector(args$renaming_table)
    # rename and save in original format
    renamed_table = rename(original_table, renaming_vector)
    if(any(is.na(renamed_table))){
        text = "Bad conversion, some values are NULL"
        simpleSignal::error(text)
        }
    write.table(renamed_table, file=args$renamed_table, quote=FALSE, sep=" ",
                col.names=FALSE, row.names=FALSE)        
    }


args_parser = function(){
    parser = arg_parser(
        paste0("Rename some languages from original coding file")
        )
    parser = add_argument(parser, "original_table", type="character",
        help="Original coding table to be renamed"
        )
    parser = add_argument(parser, "renaming_table", type="character",
        help="Table that code how languages will be renamed"
        )
    parser = add_argument(parser, "renamed_table", type="character",
        help="file name for renamed table"
        )
    args = parse_args(parser)
    return(args)
    }


if(!interactive()){
    args = args_parser()
    main(args)
    }
