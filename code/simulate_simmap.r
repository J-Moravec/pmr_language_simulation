library("ape")
library("phytools", quiet=TRUE)
library("argparser", quiet=TRUE)


read_rate_matrix = function(filepath){
    matrix = read.table(filepath, header=TRUE, sep="\t")
    matrix = as.matrix(matrix)
    return(matrix)
    }


read_tree = function(filepath){
    tree = read.nexus(filepath)
    }


read_residence = function(filepath){
    residence = read.table(filepath, stringsAsFactors=FALSE)
    return(residence)
    }


simulate_simmap = function(tree, residence, nsim, ratemat){
    tips = residence_vector(residence)
    tips = tips[tree$tip.label]
    if(anyNA(tips)){
        stop("ERROR: some names in tree and in toble does not match!")
        }
    if(any(tips == "-")){
        tips = reformat_residence(tips)
        }

    simmap = make.simmap(tree=tree, x=tips, nsim=nsim, model="ARD", Q=ratemat, message=FALSE)
    return(simmap)
    }


residence_vector = function(residence){
    res_vec = residence[, 2]
    names(res_vec) = residence[, 1]
    return(res_vec)
    }


reformat_residence = function(residence){
    states = sort(unique(residence))
    states = states[!states == "-"]
    res_mat = matrix(0, nrow=length(residence), ncol=length(states))
    colnames(res_mat) = states
    rownames(res_mat) = names(residence)
    for(state in states){
        res_mat[residence == state, state] = 1
        }
    res_mat[residence == "-", ] = 1/length(states)
    return(res_mat)
    }


get_simmap_times = function(simmap){
    times_in_states = lapply(simmap, function(x) colSums(x$mapped.edge))
    total_times = lapply(simmap, function(x) sum(x$edge.length))
    proportion_times_in_states = mapply(
        function(x,y) x/y, x=times_in_states, y=total_times, SIMPLIFY=FALSE
        )
    result = do.call(rbind, proportion_times_in_states)
    return(result)
    }


get_HPD_interval = function(vector, prob=0.95, round=2){
    vals = sort(vector)
    n_samp = length(vals)
    gap = max(1, min(n_samp - 1, round(n_samp * prob)))
    init = 1:(n_samp - gap)
    inds = which.min(vals[init + gap] - vals[init])
    HPD = c(vals[inds], vals[inds + gap])
    HPD = round(HPD, round)
    names(HPD) = c("lower", "upper")
    attr(HPD, "Probability") = gap/n_samp
    return(HPD)
    }


make_time_table = function(simmap){
    times = get_simmap_times(simmap)
    HPD = apply(times, 2, get_HPD_interval)
    means = round(colMeans(times), 2)
    time_table = cbind("mean"=means, t(HPD))
    return(time_table)
    }


get_states = function(names){
    names_no_N = names[names != "N"]
    states = unique(unlist(strsplit(names_no_N, split=",", fixed=TRUE)))
    states = sort(states)
    return(states)
    }

make_rate_names = function(states){
    named_vector = c()
    for(state1 in states){
        for(state2 in states){
            named_vector[paste0(state1, ",", state2)] = paste0("q", state1, state2)
            }
        }
    return(named_vector)
    }



make_change_table = function(simmap){
    count = countSimmap(simmap)$Tr
    # rename
    states = get_states(colnames(count))
    rate_names = make_rate_names(states)
    rate_names = c(rate_names, "N"="total")
    colnames(count) = rate_names[colnames(count)]
    not_transitions = paste0("q", states, states)
    transitions = colnames(count)[!colnames(count) %in% not_transitions]
    count = count[, transitions]

    means = colMeans(count)
    HPD = apply(count, 2, get_HPD_interval)
    count_table = cbind("mean"=means, t(HPD))
    return(count_table)
    }


write_table = function(table, filename){
    write.table(
        table,
        file=filename,
        quote=FALSE,
        sep="\t",
        row.names=TRUE,
        col.names=TRUE
        )
    }


main = function(args){
    residence = read_residence(args$residence)
    tree = read_tree(args$tree)
    ratemat = read_rate_matrix(args$matrix)
    simmap = simulate_simmap(tree, residence, args$nsim, ratemat)
    change_table = make_change_table(simmap)
    time_table = make_time_table(simmap)
    write_table(change_table, args$changes)
    write_table(time_table, args$times)
    }


args_parser = function(){
    parser = arg_parser(
        paste0(
            "simulate_simmap.r\n",
            "Simulate number of transitions and time spend in states using",
            " SIMMAP method."
            )
        )
    parser = add_argument(
        parser, "--tree", type="character",
        help=paste0(
            "Tree topology on which simulation are performed.",
            " Tree tip names must be identical to names in residence file."
            )
        )
    parser = add_argument(
        parser, "--residence", type="character",
        help=paste0(
            "Table with names and their residence. Table must have same format",
            " as table for BayesTraits. Uknown states are supported, but",
            " state combination is not."
            )
        )
    parser = add_argument(
        parser, "--matrix", type="character",
        help="Rate matrix from BayesTraits."
        )
    parser = add_argument(
        parser, "--nsim", type="numeric",
        help="Number of SIMMAP simulations."
        )
    parser = add_argument(
        parser, "--changes", type="character",
        help="File name for table with number of changes from SIMMAP simulation."
        )
    parser = add_argument(
        parser, "--times", type="character",
        help=paste0(
            "File name for table with time spend in states in SIMMAP simulation."
            )
        )
    args = parse_args(parser)
    args$opts = NULL

    if(anyNA(args)){
        print(parser)
        stop("All parameters are required.")
        }
    return(args)
    }


if(!interactive()){
    args = args_parser()
    main(args)
    }
