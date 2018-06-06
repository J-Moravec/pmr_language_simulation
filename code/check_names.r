#' check_names.r
#'
#' This script will check names in two tables and throw error if names don't
#' match. It will also try to generate matching matrix with fuzzy matching.
#'
#' As checking coding is easy, if names check, this will also look at coding,
#' but only throw warning (and log this warning) if coding don't match.
#'
#' Maybe this could also rename file, when we are in it. Bit of code will be
#' the same.
library("argparser", quiet=TRUE)
library("ape")
library("simpleSignal", quiet=TRUE)

check_names = function(names1, names2){
    if(all(names1 %in% names2) && all(names2 %in% names1)){
        return(TRUE)
        } else {
        return(FALSE)
        }
    }


#' fuzzy_matching
#'
#' Given two list of names, fuzzy_match names from first list to names in
#' second list
fuzzy_matching = function(names1, names2, rename){
    text = paste0(
        "Attempting at guessing correct mapping by fuzzy string matching.\n",
        "Look at generated table:\n", rename,
        "\nCorrect it and run me again.\n"
        )
    simpleSignal::info(text)
    unmatched1 = names1[!(names1 %in% names2)]
    unmatched2 = names2[!(names2 %in% names1)]
    
    matched = vector(mode="character", length=length(unmatched1))
    for(i in seq_along(unmatched1)){
        matched[i] = fuzzy_match(unmatched1[i], unmatched2)
        }

    write.table(
        cbind(unmatched1, matched), file=rename, quote=FALSE, sep=" ",
        row.names=FALSE, col.names=c("name", "rename")
        )
    }

#' fuzzy_match
#'
#' for word and vector of words, find best match for word from vector of words
fuzzy_match = function(word, wordlist){
    m1 = as.vector(adist(word, wordlist, partial=TRUE))
    m2 = as.vector(adist(wordlist, word, partial=TRUE))
    vecmin = pmin(m1, m2)
    return(wordlist[which.min(vecmin)])
    }


report_error = function(name1, names1, name2, names2){
    names1_not_in_names2 = names1[!(names1 %in% names2)]
    names2_not_in_names1 = names2[!(names2 %in% names1)]

    error_text = paste0(
        "\nNames in ", name1, " that are missing from ", name2, ":\n",
        paste0("\t", names1_not_in_names2, collapse="\n"), "\n",
        "Names in ", name2, " that are missing from ", name1, ":\n",
        paste0("\t", names2_not_in_names1, collapse="\n"), "\n\n"
        )
    simpleSignal::warn(error_text, log=TRUE) 
    }


check_coding = function(name1, table1, name2, table2){
    text = paste0(
        "Test if coding in ", name1, " corresponds to coding in ",
        name2, ":\n"
        )
    simpleSignal::info(text)
    # names are already matching
    table1 = table1[order(table1[,1]), ]
    table2 = table2[order(table2[,2]), ]
    if(all(table1[,2] == table1[,2])){
        simpleSignal::pass()
        } else {
        not_equal = table[,2] != table[,2]
        text = paste0(
            "\nFollowing codding differ between files:\n",
            "\n", name1, ":\n",
            paste0(
                "\t", table1[not_equal, 1], "\t",
                table1[not_equal, 2], collapse="\n"
                ), "\n",
            "\n", name2, ":\n",
            paste0(
                "\t", table2[not_equal, 1], "\t",
                table2[not_equal, 2], collapse="\n"
                ), "\n\n"
            )
        simpleSignal::warn(text, log=TRUE)
        }
    }


rename = function(names, rename_file){
    get_renaming_vector = function(renaming_file){
        table = read.table(
            renaming_file, sep=" ", stringsAsFactors=FALSE, header=TRUE
            )
        renaming_vector = table[, 2]
        names(renaming_vector) = table[, 1]
        return(renaming_vector)
        }
    
    renaming_vector = get_renaming_vector(rename_file)
    which_rename = names %in% names(renaming_vector)
    names[which_rename] = renaming_vector[names[which_rename]]
    return(names)
    }


read_table = function(input_file){
    data = read.table(input_file, sep=" ", stringsAsFactors=FALSE, header=TRUE)
    return(data)
    }


save_table = function(table, filename){
    write.table(
        table, file=filename, sep=" ", quote=FALSE, row.names=FALSE,
        col.names=TRUE
        )   
    }

read_tree = function(input_file){
    tree = read.nexus(input_file)
    return(tree)
    }


save_tree = function(tree, filename){
    write.nexus(tree, file=filename)
    }


get_names = function(object){
    if(class(object) == "data.frame"){
        names = object[, 1]
        } else if(class(object) == "phylo"){
        names = object$tip.label
        } else if(class(object) == "multiPhylo"){
        names = attr(object, "TipLabel")
        } else {
        simpleSignal::error("In get_names(object): Unrecognized object.")
        }
    return(names)
    }


set_names = function(object, names){
    if(class(object) == "data.frame"){
        object[, 1] = names
        } else if(class(object) == "phylo"){
        object$tip.label = names
        } else if(class(object) == "multiPhylo"){
        attr(object, "TipLabel") = names
        } else {
        simpleSignal::error("In set_names(object, names): Unrecognized object.")
        }
    return(object)
    }


save_object = function(object, path){
    if(class(object) == "data.frame"){
        save_table(object, path)
        } else if(class(object) == "phylo" || class(object) == "multiPhylo"){
        save_tree(object, path)
        } else {
        simpleSignal::error("In save_object(object, path): Unrecognized object.")
        }
    }


read_prune_list = function(filepath){
    name_list = readLines(filepath)
    return(name_list)
    }


prune_tree = function(tree, name_list){
    # check pruned tips are actually in tree:
    not_in_tree = !(name_list %in% get_names(tree))
    if(any(not_in_tree)){
        simpleSignal::error(
            paste0(
                "In prune_tree(tree, name_list): Some names in name_list are not",
                " in tree:\n",
                paste0(name_list[not_in_tree], collapse="\n")
                )
            )
        }


    if(class(tree) == "phylo"){
        tree = drop.tip(tree, name_list)
        } else if(class(tree) == "multiPhylo"){
        attr = attributes(tree)
        attr$TipLabel = attr$TipLabel[!attr$TipLabel %in% name_list]
        temp = lapply(tree, drop.tip, tip=name_list)
        tree = do.call(c.multiPhylo, temp)
        attributes(tree) = attr
        } else {
        simpleSignal::error(
            "In prune_tree(tree, name_list): Unrecognized object."
            )
        }
    return(tree)
    }


drop.na = function(x){
    return(x[!is.na(x)])
    }


#' Remove names in list from table.
#' Report any names that were in list but not in table.
reduce = function(table, name_list){
    names_in_table = match(name_list, table[,1])
    if(anyNA(names_in_table)){
        # names in list that are not in table were found
        names_not_in_table = names_in_table[is.na(names_in_table)]
        names_in_table = drop.na(names_in_table)

        # file warning:
        text = paste0(
            "These names were supposed to be removed, but are not contained",
            " in input table:\n",
            paste0("    ", names_not_in_table, collapse="\n"),
            "\n\n"
            )
        simpleSignal::warn(text, log=TRUE)
        }
    table = table[-names_in_table,]
    return(table)
    }


main = function(args){
    simpleSignal::log_init(args$log)

    if(args$tree){
        object = read_tree(args$input_one)
        object = set_names(object, tolower(get_names(object)))
        # prune tree
        if(!is.na(args$prune)){
            prune_list = read_prune_list(args$prune)
            object = prune_tree(object, prune_list)
            }
        } else {
        object = read_table(args$input_one)
        }
    table = read_table(args$input_two)

    if(!is.na(args$reduce)){
        reduce_list = readLines(args$reduce)
        table = reduce(table, reduce_list)

        if(!is.na(args$reduce_save)){
            save_table(table, args$reduce_save)
            }
        }

    name1 = basename(args$input_one)
    name2 = basename(args$input_two)

    if(!is.na(args$rename) && file.exists(args$rename)){
        text = paste0(
            "Renaming file was found, renaming ", name1, ".\n"
            )
        simpleSignal::info(text)
        renamed_names = rename(get_names(object), args$rename)
        object = set_names(object, renamed_names)
        }

    names1 = get_names(object)
    names2 = table[, 1]

    # test names
    text = paste0(
        "Check if names in ", name1, " correspond to names in ", name2, ":\n"
        )
    simpleSignal::info(text)
    if(check_names(names1, names2)){
        simpleSignal::pass()

        # if names are correct, test coding if specified
        if(args$coding){
            check_coding(name1, object, name2, table)
            }

        if(!is.na(args$save)){
            save_object(object, args$save)
            }

        } else {
        # if names are not correct, create report
        report_error(name1, names1, name2, names2)

        # if names are not correct, create suggeste mapping if specified
        if(!is.na(args$rename)){
            fuzzy_matching(names1, names2, args$rename)
            }
        # throw error
        simpleSignal::stop_if_warn()
        }
    }


args_parser = function(){
    parser = arg_parser(
        paste0(
            "This script will primary check names in table. However,",
            " if required, it can also attempt guess right mapping, if",
            " name lists do not match. It can also check coding, this",
            " will however do not throw any error, just log warning."
            )
        )
    parser = add_argument(
        parser, "input_one", type="character",
        help=paste0(
            "First table, tree or list of names to check. This list of names will",
            " will be renamed if rename option is specified and rename",
            " file exists."
            )
        )
    parser = add_argument(
        parser, "input_two", type="character",
        help=paste0(
            "Second table or list of names to check. If specified, names in",
            " this list would be checked against names in first list to find",
            " best match. Matched names from this file are then kept."
            )
        )
    parser = add_argument(
        parser, "--rename", type="character",
        help=paste0(
            "If specified, this file is used to translate names from first",
            " input file, which is then checked as normal. However, if",
            " rename file does not exists, and name lists differ, then",
            " correct matching will be guessed with fuzzy-matching."
            )
        )
    parser = add_argument(
        parser, "--coding", flag=TRUE,
        help=paste0(
            "If specified, script will also check if codding is the same.",
            " But unlike name lists, unsuccessful test will not throw error."
            )
        )
    parser = add_argument(
        parser, "--log", type="character", default="file.log",
        help="Log file with errors and warnings."
        )
    parser = add_argument(
        parser, "--save", type="character",
        help=paste0(
            "If specified together with rename, renaming file is found",
            " and renamed input is successfully passed, script will save",
            " renamed table into path specified by this argument."
            )
        )
    parser = add_argument(
        parser, "--tree", flag=TRUE,
        help=paste0(
            "If specified, first output is treated as tree or trees.",
            " In that case, coding can't be checked."
            )
        )
    parser = add_argument(
        parser, "--prune", type="character",
        help=paste0(
            "File with names for tips that will be pruned from tree.",
            " File must contain single name per line.",
            " If --tree flag is not specified, this option is ignored."
            )
        )

    parser = add_argument(
        parser, "--reduce", type="character",
        help=paste0(
            "Similar to prune, but for input_two file. File with list of names",
            " that will be removed from input_two."
            )
        )
    parser = add_argument(
        parser, "--reduce_save", type="character",
        help=paste0(
            "Name of file for saving reduced input_two."
            )
        )
    args = parse_args(parser)
    return(args)
    }


if(!interactive()){
    args = args_parser()
    if(args$coding && args$tree){
        simpleSignal::error("Can't check coding for tree input.")
        }
    main(args)
    }
