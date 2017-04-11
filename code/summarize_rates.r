#' summarize_rates.r
#'
#' This script will summarize rates from BayesTraits output after header is
#' removed.
#' It will filter out burnin and states where rate is zero and output
#' median and mean rate matrix.
#' It will also calculate transition matrices for several branch-times
#' as calculated from tree.
library("argparser", quiet=TRUE)
library("ape")
library("stupidSignal")


read_traces = function(filenames){
    traces = list()
    for(filename in filenames){
        trace = read_table(filename)
        traces[[filename]] = trace
        }
    return(traces)
    }



read_table = function(filepath){
    table = read.table(filepath, header=TRUE, sep="\t", stringsAsFactors=FALSE)
    }


get_columns = function(table, pattern){
    pos = grep(pattern, names(table))
    return(table[, pos])
    }


remove_burnin = function(traces, burnin, percentage){
    afterburn = list()
    for(name in names(traces)){
        trace = traces[[name]]
        n = nrow(trace)
        if(percentage){
            n_burn = trunc(burnin * n)
            } else {
            n_burn = burnin
            }
        if(burnin != 0){
            trace = trace[-seq_len(n_burn),]
            }
        text = paste0("Removing ", n_burn, " states out of ", n, " total.")
        stupidSignal::info(text)
        afterburn[[name]] = trace
        }
    #names(afterburn) = names(traces)
    return(afterburn)
    }


# merge traces, usually after burnin
# rbind needs same columns for data.frame, thus this is also a good check
merge_traces = function(traces){
    trace = do.call(rbind, traces)
    return(trace)
    }


remove_zeros = function(vector){
    bad = (vector == 0)
    return(vector[!bad])
    }


get_ratenames = function(rate_names){
    rate_names = sub("^q", "", rate_names)
    states = unique(unlist(strsplit(rate_names, split="", fixed=TRUE)))
    return(states)
    }


get_rate_matrix = function(table, method, remove_zeros=FALSE){
    if(method == "mean"){
        fmethod = mean
        } else if(method == "median"){
        fmethod = median
        } else if(method == "mode"){
        fmethod = mode
        } else {
        text = "Unrecognized method. Method must be either mean or median!"
        stupidSignal::error(text)
        }
    states = get_ratenames(colnames(table))
    # change table of rates into list, remove zero
    rate_list = list()
    for(rate in colnames(table)){
        filtered = table[[rate]]
        if(remove_zeros){
            filtered = remove_zeros(filtered)
            }
        rate_list[[rate]] = fmethod(filtered)
        }

    # build rate matrix:
    rate_matrix = matrix(0, nrow=length(states), ncol=length(states))
    colnames(rate_matrix) = states
    rownames(rate_matrix) = states
    for(i in seq_along(states)){
        for(j in seq_along(states)){
            if(i == j){
                rate_matrix[i, j] = 0
                } else {
                rate = rate_list[[paste0("q", states[i], states[j])]]
                rate_matrix[i, j] = rate
                }
            }
        }
    for(i in seq_along(states)){
        rate_matrix[i, i] = -sum(rate_matrix[i, -i])
        }
    return(rate_matrix)
    }


get_transition_matrix = function(rate_matrix, time){
    # Q -- rate matrix
    # Q = CDC^-1 -- eigen decomposition
    # P(t) -- transition matrix dependent on time
    # P(t) = exp(Q*t) = C exp(D*t) * C^-1 -- calculation
    decomposition = eigen(rate_matrix)
    lambdas = decomposition$values
    C = decomposition$vectors
    # bevause D is diagonal matrix, exp(D) is diag(exp(lambdas))
    # where lambdas are the diagonal elements
    expD = diag(exp(lambdas*time))
    trans_mat = C %*% expD %*% solve(C)
    colnames(trans_mat) = colnames(rate_matrix)
    rownames(trans_mat) = rownames(rate_matrix)
    return(trans_mat)
    }


read_tree = function(filepath){
    tree = read.nexus(filepath)
    return(tree)
    }


get_height = function(tree){
    if(class(tree) == "phylo"){
        height = max(node.depth.edgelength(tree))
        } else if(class(tree) == "multiPhylo"){
        height = lapply(tree, node.depth.edgelength)
        height = lapply(height, max)
        height = mean(unlist(height))
        } else {
        stupidSignal::error("ERROR: in get_height: unrecognized tree format")
        }
    return(height)
    }


get_branch_length = function(tree){
    if(class(tree) == "phylo"){
        branch_length = mean(tree$edge.length)
        } else if (class(tree) == "multiPhylo"){
        branch_length = mean(unlist(lapply(tree, getElement, "edge.length")))
        } else {
        stupidSignal::error("ERROR: in get_branch_length: unrecognized tree format")
        }
    return(branch_length)
    }


# paste0 that formats before pasting
pastef = function(..., collapse=NULL, digits=4){
    vec = list(...)
    vec = lapply(vec, format, digits=digits)
    vec = .Internal(paste0(vec, collapse=collapse))
    return(vec)
    }


format_matrix = function(matrix, digits=2){
    # make text vector out of row names, col names and matrix itself
    # one for minus, another one for dot
    max_width = ceiling(log10(max(abs(matrix)))) + digits + 2
    text_matrix = formatC(matrix, digits = digits, format="f", width=max_width)
    # make text matrix into rows separated by space
    text_matrix = apply(text_matrix, 1, paste, collapse=" ")
    col_names = colnames(matrix)
    col_names = formatC(col_names, width=max_width)
    col_names = paste(col_names, collapse=" ")

    row_names = c("", rownames(matrix))
    row_names = formatC(row_names, width=max(nchar(row_names))+1)

    # format in following way:
    # (without | and - )
    #
    #  | colnames
    # -----------
    # r|   matrix
    # o|   matrix
    # w|   matrix

    # connect matrix with colnames:
    text_matrix = c(col_names, text_matrix)
    # connect right stuff with row_names
    text_matrix = paste(row_names, text_matrix, sep=" ")
    text_matrix = paste(text_matrix, collapse="\n")
    text_matrix = paste0(text_matrix, "\n\n")
    return(text_matrix)
    }


get_text_rate_matrix = function(rate_matrix, method, digits=2){
    text_rate_matrix = format_matrix(rate_matrix, digits=digits)
    text = pastef(
        "Rate matrix according to: ", method, "\n\n",
        "Rate matrix:\n", text_rate_matrix,
        digits=digits
        )
    return(text)
    }


get_text_transition_matrix = function(rate_matrix, tree, method, text, digits=2){
    time = method(tree)
    transition_matrix = get_transition_matrix(rate_matrix, time)
    text_transition_matrix = format_matrix(transition_matrix, digits=digits)
    text_transition_matrix = pastef(
        text, time, "\n", text_transition_matrix, digits=digits
        )
    return(text_transition_matrix)
    }


get_branch_transition_matrix = function(rate_matrix, tree, digits=2){
    return(
        get_text_transition_matrix(
            rate_matrix,
            tree,
            get_branch_length,
            "Transition matrix with time equal to mean branch length: ",
            digits=digits
            )
        )
    }


get_height_transition_matrix = function(rate_matrix, tree, digits=2){
    return(
        get_text_transition_matrix(
            rate_matrix,
            tree,
            function(x){get_height(x)/10},
            "Transition matrix with time equal to 1/10 of tree height: ",
            digits=digits
            )
        )
    }


mode = function(vec, n=1000, from=0){
    # approximate with density:
    dens = density(vec, n=1000, from=0)

    # find maximal value
    mode = dens$x[which.max(dens$y)]
    return(mode)
    }


get_stat_vector = function(name, vec){
    decimal = 2
    stats = c(mean(vec), median(vec), sd(vec), mode(vec))
    max_width = max(ceiling(log10(max(abs(stats)))) + decimal + 1, nchar(name))
    stats = formatC(stats, format="f", digits=decimal, width=max_width)
    # padd name with " "
    name = formatC(name, width=max_width, flag="-")
    stats = c(name, stats)
    return(stats)
    }


get_header = function(){
    stats = c("", "mean:", "median:", "sd:", "mode:")
    max_width = max(nchar(stats))
    stats = formatC(stats, width=max_width, flag="-")
    return(stats)
    }


get_formatted_stats = function(dataframe, title, fun=NULL, stats_per_row=8){
    stats = list()
    header = get_header()
    for(name in names(dataframe)){
        vec = dataframe[[name]]
        if(!is.null(fun)){
            vec = fun(vec)
            }
        stat_vector = get_stat_vector(name, vec)
        stats = c(stats, list(stat_vector))
        }
    starts = seq(from=1, to=length(stats), by=stats_per_row)
    stops = starts + stats_per_row - 1
    stops[length(stops)] = length(stats)

    # assembly stats
    text = list()
    for(i in seq_along(starts)){
        sublist = stats[starts[i]:stops[i]]
        sublist[["sep"]] = " "
        sublist = c(list(header), sublist)
        subtext = paste0(do.call(paste, sublist), collapse="\n")
        text = c(text, list(subtext))
        }

    text[["sep"]] = "\n\n"
    text = do.call(paste, text)
    text = paste0(title, "\n", text, "\n", strrep("-", 60), "\n\n")
    return(text)
    }

first = function(x){
    x[1]
    }

last = function(x){
    x[length(x)]
    }

plot_density = function(vec, title){
    # 400 is belivable end value
    dens = density(vec, n=1000, from=0)
    dens$x = c(first(dens$x), dens$x, last(dens$x))
    dens$y = c(0, dens$y, 0)
    plot(dens, type="n", axes=FALSE, main=title, xlab="", ylab="")
    polygon(dens, col="lightgrey", border="darkgrey")
    axis(1, col=NA, col.ticks=1, lwd=1)
    axis(2, col=NA, col.ticks=1, lwd=1, las=1)
    }


plot_densities = function(dataframe, file, pattern="{@}", mfrow=c(3,2)){
    n_var = length(dataframe)
    n_graphs_per_plot = prod(mfrow)
    starts = seq(from=1, to=n_var, by=n_graphs_per_plot)
    stops = starts + n_graphs_per_plot - 1
    stops[length(stops)] = n_var
    for(i in seq_along(starts)){
        filename = gsub(pattern, i, file, fixed=TRUE)
        png(filename, res=144, width=1024, height=768)
        par(
            "mar" = c(3, 4, 2, 1) + 0.1,
            "xaxs"="i",
            "yaxs"="i",
            "mfrow"=c(3,2)
            )


        subdataframe = dataframe[starts[i]:stops[i]]
        for(name in names(subdataframe)){
            plot_density(subdataframe[[name]], name)
            }
        invisible(dev.off())
        }
    }


main = function(args){
    traces = read_traces(args$input)
    traces = remove_burnin(traces, args$burnin, args$percentage) 
    trace = merge_traces(traces)

    # various stats:
    text = list()

    # get rates with and without zeros
    rates = get_columns(trace, "^q")
    # with zeros
    rate_header = "Rates with zeros"
    text = c(text, list(get_formatted_stats(rates, rate_header)))

    # without zeros
    rate_header = "Rates without zeros"
    text = c(text, list(get_formatted_stats(rates, rate_header, fun=remove_zeros)))

    # Roots:
    roots = get_columns(trace, "^Root")
    root_names = sub(".", " ", names(roots), fixed=TRUE)
    root_names = sub(".", "(", root_names, fixed=TRUE)
    root_names = sub(".", ")", root_names, fixed=TRUE)
    names(roots) = root_names
    root_header = "Probability of tree root being in state:"
    text = c(text, list(get_formatted_stats(roots, root_header)))

    # RJ prior:
    others = list()
    rj_prior_name = "RJ Prior Mean"
    others[[rj_prior_name]] = trace$RJ.Prior.Mean
    if(args$kappa){
        if(is.null(trace$Kappa)){
            stop("ERROR: Kappa is not present.")
            }
        others[["Kappa"]] = trace$Kappa
        }
    others = as.data.frame(others)
    text = c(text, list(get_formatted_stats(others, "Other variables:")))



    # read tree and get matrix stats
    if(!is.na(args$tree)){
        tree = read_tree(args$tree)
        }

    mean_rate_matrix = get_rate_matrix(rates, "mean")
    mean_matrix_stats = get_text_rate_matrix(
        mean_rate_matrix, "mean", args$decimal
        )
    text = c(text, list(mean_matrix_stats))

    if(!is.na(args$tree)){
        mat = get_branch_transition_matrix(mean_rate_matrix, tree, args$decimal)
        mat2 = get_height_transition_matrix(mean_rate_matrix, tree, args$decimal)
        text = c(text, list(mat), list(mat2))
        }

    text = c(text, list(paste0(strrep("-", 60), "\n")))

    median_rate_matrix = get_rate_matrix(rates, "median")
    median_matrix_stats = get_text_rate_matrix(
        median_rate_matrix, "median", args$decimal
        )
    text = c(text, list(median_matrix_stats))

    if(!is.na(args$tree)){
        mat = get_branch_transition_matrix(median_rate_matrix, tree, args$decimal)
        mat2 = get_height_transition_matrix(median_rate_matrix, tree, args$decimal)
        text = c(text, list(mat), list(mat2))
        }
    text = c(text, list(paste0(strrep("-", 60), "\n")))

    mode_rate_matrix = get_rate_matrix(rates, "mode")
    mode_matrix_stats = get_text_rate_matrix(mode_rate_matrix, "mode", args$decimal)
    text = c(text, list(mode_matrix_stats))


    text = do.call(paste0, text)
    cat(text, file=args$output, append=FALSE)

    if(!is.na(args$matrix)){
        rate_matrix = get_rate_matrix(rates, args$method, args$filter_zeros)
        write.table(
            rate_matrix, file=args$matrix, quote=FALSE, sep="\t",
            row.names=TRUE, col.names=TRUE
            )
        }
    if(!is.na(args$imagedir)){
        if(!dir.exists(args$imagedir)){
            dir.create(args$imagedir)
            }

        plot_densities(rates, file.path(args$imagedir, "rates_dens_{@}.png"))
        plot_densities(roots, file.path(args$imagedir, "roots_dens_{@}.png"))
        plot_densities(others, file.path(args$imagedir, "others_dens_{@}.png"))
        }
    }



args_parser = function(){
    parser = arg_parser(
        paste0(
            "summarize_rates.r\n",
            "This script will summarize rates from modified BayesTrait output",
            " (after removing header) and output rate and transition matrices",
            " for several branch-time values. It also output some tree stats."
            )
        )
    parser = add_argument(
        parser, "--input", type="character", nargs="Inf",
        help="Single or multiple outputs from BayesTrait with removed header."
        )
    parser = add_argument(
        parser, "output", type="character",
        help="Output path where summary information will be written."
        )
    parser = add_argument(
        parser, "--tree", type="character",
        help=paste0(
            "Tree used in BayesTraits analysis. If not included,",
            " probability transition matrix is not calculated."
            )
        )
    parser = add_argument(
        parser, "--burnin", type="numeric", default=0,
        help=paste0(
            "Burnin that will be removed. Can be either in number of states",
            " or in percentages, if percentage flag is specified."
            )
        )
    parser = add_argument(
        parser, "--percentage", flag=TRUE, help="If burnin is in percentage."
        )
    parser = add_argument(
        parser, "--kappa", flag=TRUE,
        help=paste0(
            "If specified, kappa will be analyzed as well."
            )
        )
    parser = add_argument(
        parser, "--matrix", type="character",
        help="If specified, mean rate matrix will be saved in specified path."
        )
    parser = add_argument(
        parser, "--method", type="character", default="mean",
        help=paste0(
            "Method for outputted matrix. Default is mean, possible values:",
            " mean, median, mode"
            )
        )
    parser = add_argument(
        parser, "--filter_zeros", flag=TRUE,
        help=paste0(
            "If specified, zeros will be filtered from race for outputted",
            " rate matrix calculation."
            )
        )
    parser = add_argument(
        parser, "--imagedir", type="character",
        help=paste0(
            "Folder for png images of estimated densities.",
            " If not specified, nothign is plotted."
            )
        )
    parser = add_argument(
        parser, "--decimal", type="numeric", default=2,
        help="Number of decimal places."
        )
    args = parse_args(parser)
    return(args)
    }


if(!interactive()){
    args = args_parser()

    # in older versions of argparser, multiple arguments for single parameter
    # are not translated into vector, but passed as single string with
    # individual arguments separated by comma
    if(length(args$input) == 1 && grep(",", args$input, fixed=TRUE)){
        args$input = unlist(strsplit(args$input, split=",", fixed=TRUE))
        }
    if(!args$method %in% c("mean", "median", "mode")){
        stop("Invalid method. Must be either mean, median or mode.")
        }
    main(args)
    }
