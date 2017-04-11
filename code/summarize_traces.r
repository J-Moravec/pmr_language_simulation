# summarize trace from BayesTraits output
# outputs 6 main statistics from coda package:

library("coda")
library("argparser", quiet=TRUE)


# read trace
read_trace = function(inputfile){
    trace = read.table(inputfile, sep="\t", header=TRUE)
    # remove unvanted columns
    trace["Iteration"] = NULL
    trace["X"] = NULL
    trace["Harmonic.Mean"] = NULL
    return(mcmc(trace))
    }


read_traces = function(filename, from, to, pattern){
    list_of_mcmc = list()
    for(i in from:to){
        inputfile = sub(pattern=pattern, replacement=i, filename, fixed=TRUE)
        single_mcmc = read_trace(inputfile)
        list_of_mcmc[[i]] = single_mcmc
        }
    mcmc_list = mcmc.list(list_of_mcmc)
    return(mcmc_list)
    }


read_traces2 = function(filenames){
    list_of_mcmc = list()
    for(filename in filenames){
        single_mcmc = read_trace(filename)
        list_of_mcmc[[filename]] = single_mcmc
        }
    mcmc_list = mcmc.list(list_of_mcmc)
    return(mcmc_list)
    }


# because default plotting function is crap, wrapper function that do some testing and then call specific plotting function
plot_mcmc = function(
    object, filename,
    chainpattern="{@}", varpattern="{@@}", # used for substitution in filename
    ppar=list(), pplot=list(), ppng=list() # parametrization of par, plot and png
    ){
    if(class(object) == "mcmc.list"){
            .plot_mcmc_list(
                object, filename,
                chainpattern, varpattern,
                ppar, pplot, ppng
                )
        } else if(class(object) == "mcmc"){
            .plot_mcmc(object, filename, varpattern, ppar, pplot, ppng)
        } else {
        stop("ERROR: object is not mcmc or mcmc.list!")
        }
    }

# function for plotting mcmc
.plot_mcmc = function(object, filename, varpattern, ppar, pplot, ppng){
    if(!.single_match(filename, varpattern)){
        stop(
            "Pattern substitution is not unique! This can be potentially",
            " harmful. Please, set filename and pattern so that both",
            " patterns are unique!"
            )
        }
    default_par = list(
        #ppar[["mfrow"]] = c(3,2)
        "mar" = c(3, 2, 2, 1) + 0.1
        )
    default_plot = list(
        ask=FALSE,
        auto.layout=FALSE
        )
    default_png = list(
        res = 144,
        width=1024,
        height=768
        )

    ppar = merge_lists(ppar, default_par)
    pplot = merge_lists(pplot, default_plot)
    ppng = merge_lists(ppng, default_png)

    if(!is.null(ppar[["mfrow"]]) && !is.null(ppar[["mfcol"]])){
        stop("Specify either mfrow or mfcol, not both.")
        } else if (!is.null(ppar[["mfrow"]])){
        n_graphs_per_plot = prod(ppar[["mfrow"]])
        } else if(!is.null(ppar[["mfcol"]])){
        n_graphs_per_plot = prod(ppar[["mfcol"]])
        } else {
        ppar[["mfrow"]] = c(3,2)
        n_graphs_per_plot = prod(ppar[["mfrow"]])
        }

    if((n_graphs_per_plot %% 2) != 0){
        stop("Please, set even number of fields for plotting")
        }
    npar = ncol(object)

    n_graphs_total = 2*npar

    starts = seq(from=1, to=npar, by=n_graphs_per_plot/2)
    stops = starts + n_graphs_per_plot/2 - 1
    stops[length(stops)] = npar

    # get subset from object
    for(i in seq_along(starts)){
        subfilename = gsub(varpattern, i, filename, fixed=TRUE)
        ppng[["filename"]] = subfilename
        do.call(png, ppng)

        do.call(par, ppar)
        #cat("start: ", starts[i], " stop: ", stops[i], "\n")
        subobject = object[,starts[i]:stops[i], drop=FALSE]
        pplot[["x"]] = subobject
        do.call(coda:::plot.mcmc, pplot)
        invisible(dev.off())
        }
    }


# function for plotting mcmc.list, calls .plot_mcmc
# function get number of chains and substitute them into filename
# according to pattern, then call .plot_mcmc for each chain.
.plot_mcmc_list = function(
    object, filename,
    chainpattern, varpattern,
    ppar, pplot, ppng
    ){
    if(!.single_match(filename, chainpattern)){
        stop(
            "Pattern substitution is not unique! This can be potentially",
            " harmful. Please, set filename and pattern so that both",
            " patterns are unique!"
            )
        }
    n_chain = length(object)
    vec_chain = seq_len(n_chain)
    for(i in vec_chain){
        
        chainfilename = gsub(chainpattern, i, filename, fixed=TRUE)
        .plot_mcmc(object[[i]], chainfilename, varpattern, ppar, pplot, ppng)
        }
    }


.single_match = function(filename, pattern){
    n_matches = lengths(regmatches(
        filename, gregexpr(pattern, filename, fixed=TRUE)
        ))

    return(
        length(filename) == 1 &&
        n_matches == 1
        )
    }


# adds elements of list2 to list1
merge_lists = function(list1, list2){
    for(element in names(list2)){
        list1[[element]] = list2[[element]]
        }
    return(list1)
    }


calculate_stats = function(traces, basename){
    try_stat = function(traces, basename, file, fun){
        #on.exit(sink())
        sinkfile = file.path(basename, file)
        stats = try(fun(traces))
        if(class(stats) != "try-error"){
            capture.output(print(stats), file=sinkfile)
            }
        }

    stats_and_files = list(
        "ESS" = list(
            "fun" = coda::effectiveSize,
            "file" = "1-effective_sample_size.txt"
            ),
        "geweke" = list(
            "fun" = coda::geweke.diag,
            "file" = "2-geweke.txt"
            ),
        "gelman" = list(
            "fun" = coda::gelman.diag,
            "file" = "3-gelman_and_rubin.txt"
            ),
        "raftery" = list(
            "fun" = coda::raftery.diag,
            "file" = "4-raftery_and_lewis.txt"
            ),
        "heidelberger" = list(
            "fun" = coda::heidel.diag,
            "file" = "5-heidelberger_and_welch.txt"
            ),
        "autocorrelation" = list(
            "fun" = coda::autocorr,
            "file" = "6-autocorrelation.txt"
            ),
        "crosscorrelation" = list(
            "fun" = coda::crosscorr,
            "file" = "7-crosscorrelation.txt"
            )
        )

    for(name in names(stats_and_files)){
        message("Processing statistic: ", name)
        sublist = stats_and_files[[name]]
        try_stat(traces, basename, sublist$file, sublist$fun)
        }


    }

main = function(args){
    if(!is.null(args$i)){
        traces = read_traces2(args$i)
        if( !dir.exists(args$dirname) ){
            dir.create(args$dirname)
            }
        calculate_stats(traces, args$dirname)
        picfiles = file.path(args$dirname, "chain{@}graphs{@@}.png")
        plot_mcmc(traces, picfiles)
        }
    cat(date(), file=args$timestamp)
    }



args_parser = function(){
    parser = arg_parser(
        paste0(
            "Check convergence for single or multiple outputs of Bayesian MCMC",
            " analysis using coda package."
            )
        )
    parser = add_argument(
        parser, "-i", type="character", nargs="Inf",
        help=paste0(
            "single or multiple outputs (individual runs/chains) from",
            " Bayesian MCMC analysis"
            )
        )
    parser = add_argument(
        parser, "--dirname", type="character", default=".",
        help="Output folder for various statistics."
        )
    parser = add_argument(
        parser, "--timestamp", type="character", default=".",
        help="Timestamps file for GNUmake purpose."
        )
    args = parse_args(parser)
    return(args)
    }


if(!interactive()){
    args = args_parser()
    main(args)
    }
