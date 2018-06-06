#' summarize_trees.r
#'
#' This script will summarize nexus trees and output simple formated stats
library("argparser", quiet=TRUE)
library("ape")
library("simpleSignal", quiet=TRUE)


read_tree = function(filepath){
    tree = read.nexus(filepath)
    return(tree)
    }


# paste0 that formats before pasting
pastef = function(..., collapse=NULL, digits=4){
    vec = list(...)
    vec = lapply(vec, format, digits=digits)
    vec = .Internal(paste0(vec, collapse=collapse))
    return(vec)
    }


get_heights = function(tree){
    edgelengths = node.depth.edgelength(tree)
    Ntips = length(tree$tip.label)
    heights = edgelengths[1:Ntips]
    return(heights)
    }

get_tree_stats = function(tree, digits){
    get_tree_stats_multiphylo = function(tree){
        tree_stats = list()

        tree_stats[["n_trees"]] = length(tree)
        tree_stats[["n_taxa"]] = length(attr(tree, "TipLabel"))


        tree_heights = unlist(lapply(lapply(tree, get_heights), max))
        tree_stats[["tree_height_mean"]] = mean(tree_heights)
        tree_stats[["tree_height_min"]] = min(tree_heights)
        tree_stats[["tree_height_max"]] = max(tree_heights)
        tree_stats[["tree_height_sd"]] = sd(tree_heights)


        tree_heights = unlist(lapply(lapply(tree, get_heights), min))
        tree_stats[["tree_height_min_mean"]] = mean(tree_heights)
        tree_stats[["tree_height_min_min"]] = min(tree_heights)
        tree_stats[["tree_height_min_max"]] = max(tree_heights)
        tree_stats[["tree_height_min_sd"]] = sd(tree_heights)


        tree_heights = unlist(lapply(lapply(tree, get_heights), mean))
        tree_stats[["tree_height_mean_mean"]] = mean(tree_heights)
        tree_stats[["tree_height_mean_min"]] = min(tree_heights)
        tree_stats[["tree_height_mean_max"]] = max(tree_heights)
        tree_stats[["tree_height_mean_sd"]] = sd(tree_heights)

        branches = unlist(lapply(tree, getElement, "edge.length"))
        tree_stats[["branch_mean"]] = mean(branches)
        tree_stats[["branch_min"]] = min(branches)
        tree_stats[["branch_max"]] = max(branches)
        tree_stats[["branch_sd"]] = sd(branches)

        total_branch = unlist(
            lapply(
                lapply(tree, getElement, "edge.length"), sum
                )
            )
        tree_stats[["total_branch_mean"]] = mean(total_branch)
        tree_stats[["total_branch_min"]] = min(total_branch)
        tree_stats[["total_branch_max"]] = max(total_branch)
        tree_stats[["total_branch_sd"]] = sd(total_branch)
        return(tree_stats)
        }

    get_tree_stats_phylo = function(tree){
        tree_stats = list()
        
        tree_stats[["n_trees"]] = 1
        tree_stats[["n_taxa"]] = length(tree$tip.label)

        # does not make sense
        tree_height = max(node.depth.edgelength(tree))
        tree_stats[["tree_height"]] = tree_height

        branches = tree$edge.length
        tree_stats[["branch_mean"]] = mean(branches)
        tree_stats[["branch_min"]] = min(branches)
        tree_stats[["branch_max"]] = max(branches)
        tree_stats[["branch_sd"]] = sd(branches)

        total_branch = sum(tree$edge.length)
        tree_stats[["total_branch"]] = total_branch

        return(tree_stats)
        }

    tree_stats_format_multiphylo = function(tree_stats){
        text = pastef(
            "Tree stats:\n",
            "        n_trees: ", tree_stats$n_trees, "\n",
            "        n_taxa: ", tree_stats$n_taxa, "\n",
            "    Tree height: max\n",
            "        mean: ", tree_stats$tree_height_mean, "\n",
            "        min: ", tree_stats$tree_height_min, "\n",
            "        max: ", tree_stats$tree_height_max, "\n",
            "        sd: ", tree_stats$tree_height_sd, "\n",
            "    Tree height: min\n",
            "        mean: ", tree_stats$tree_height_min_mean, "\n",
            "        min: ", tree_stats$tree_height_min_min, "\n",
            "        max: ", tree_stats$tree_height_min_max, "\n",
            "        sd: ", tree_stats$tree_height_min_sd, "\n",
            "    Tree height: mean\n",
            "        mean: ", tree_stats$tree_height_mean_mean, "\n",
            "        min: ", tree_stats$tree_height_mean_min, "\n",
            "        max: ", tree_stats$tree_height_mean_max, "\n",
            "        sd: ", tree_stats$tree_height_mean_sd, "\n",
            "    Branch length:\n",
            "        mean: ", tree_stats$branch_mean, "\n",
            "        min: ", tree_stats$branch_min, "\n",
            "        max: ", tree_stats$branch_max, "\n",
            "        sd: ", tree_stats$branch_sd, "\n",
            "   Total branch length:\n",
            "       mean: ", tree_stats$total_branch_mean, "\n",
            "       min: ", tree_stats$total_branch_min, "\n",
            "       max: ", tree_stats$total_branch_max, "\n",
            "       sd: ", tree_stats$total_branch_sd, "\n",
            "-----------------------------------------------------------------\n\n",
            digits=digits)
        return(text)
        }

    tree_stats_format_phylo = function(tree_stats){
        text = pastef(
            "Tree stats:\n",
            "        n_trees: ", tree_stats$n_trees, "\n",
            "        n_taxa: ", tree_stats$n_taxa, "\n",
            "    Tree height:\n",
            "        value: ", tree_stats$tree_height, "\n",
            "    Branch length:\n",
            "        mean: ", tree_stats$branch_mean, "\n",
            "        min: ", tree_stats$branch_min, "\n",
            "        max: ", tree_stats$branch_max, "\n",
            "        sd: ", tree_stats$branch_sd, "\n",
            "   Total branch length:\n",
            "       value: ", tree_stats$total_branch, "\n",
            "-----------------------------------------------------------------\n\n",
            digits=digits)
        return(text)
        }

    if(class(tree) == "phylo"){
        tree_stats = get_tree_stats_phylo(tree)
        tree_stats_text = tree_stats_format_phylo(tree_stats)
        } else if(class(tree) == "multiPhylo"){
        tree_stats = get_tree_stats_multiphylo(tree)
        tree_stats_text = tree_stats_format_multiphylo(tree_stats)
        } else {
        simpleSignal::error("ERROR: in get_tree_stats: unrecognized tree format")
        }
    return(tree_stats_text)
    }


main = function(args){
    tree = read_tree(args$input)
    tree_stats = get_tree_stats(tree, args$decimal)
    cat(tree_stats, file=args$output)
    }


args_parser = function(){
    parser = arg_parser(
        paste0(
            "Summarize trees with some statistics, like branch length or height."
            )
        )
    parser = add_argument(
        parser, "input", type="character",
        help="Tree or trees in nexus format.",
        )
    parser = add_argument(
        parser, "output", type="character",
        help="Output file with calculated statistics."
        )
    args = parse_args(parser)
    return(args)
    }


if(!interactive()){
    args = args_parser()
    main(args)
    }
