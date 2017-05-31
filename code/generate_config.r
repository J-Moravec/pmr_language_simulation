library("simpleSignal", quiet=TRUE)

args = commandArgs(TRUE)

config = paste0(
    "1
    $TYPE
    # this many iteration was on Austronesian tree
    iterations $ITERATIONS
    # ultimately, burnin can be removed, but with burnin, I can see
    # better how the chain behaves
    burnin 0
    # with 10^9 iterations, this will be around 50 000 samples.
    sample $SAMPLE
    # fiona used 0 100, other even smaller like 0 10
    # with large amount of iterations, it shouldnt be a problem
    #rjhp exp 0 100
    logfile $LOGFILE
    $OTHER
    run\n"
    )


params = list(
    "ITERATIONS" = "1000000000",
    "SAMPLE" = "20000",
    "LOGFILE" = "bayestraits.log",
    "TYPE" = "2", # 1 for ML, 2 for BI
    "OTHER" = ""
    )

last = function(x){
    return(x[length(x)])
    }


parse_args = function(args){
    # find params in args
    params_pos = grep("--", args)
    # check if all params have at least single argument to consume
    difference = diff(params_pos)
    which_diff = which(difference == 1)
    which_missing = params_pos[which_diff]
    if(last(params_pos) == length(args)){
        which_missing = c(which_missing, last(params_pos))
        }
    if(length(which_missing) > 0){
        text = paste0(
            "Error: In parse_args: must specify at least single argument",
            " for these parameters: ",
            paste0(args[which_missing], collapse=", ")
            )
        simpleSignal::error(text)
        }
    # parameter vector for substitution
    params_names = args[params_pos]
    params_names = sub("--", "", params_names, fixed=TRUE)
    params_args = args[params_pos + 1]
    params_args = sub("\\n", "\n", params_args, fixed=TRUE)
    names(params_args) = params_names
    return(params_args)
    }


substitute_params = function(params, args){
    are_params = names(args) %in% names(params)
    are_not_params = !are_params
        if(any(are_not_params)){
        text = paste0(
            "These input parameters are not config parameters: ",
            names(args)[are_not_params]
            )
        simpleSignal::warn(text)
        }
    # substitute
    params[names(args)[are_params]] = args[are_params]
    return(params)
    }


substitute_config = function(params){
    params_names = paste0("$", names(params))
    for(i in seq_along(params_names)){
        config = sub(params_names[i], params[i], config, fixed=TRUE)
        }
    config = gsub("\n[[:space:]]+", "\n", config)
    return(config)
    }

if(!interactive()){
    if(!length(args) == 0){
        args = parse_args(args)
        }
    params = substitute_params(params, args)
    config = substitute_config(params)
    cat(config)
    }
