#' model_string.r
#'
#' This scripts take file with model string from RJ MCMC analysis and
#' returns output table with the model strings and their count.


library("argparser", quiet=TRUE)
library("stupidSignal")
.columnames = NA

read_model_string = function(filepath){
    model_string = readLines(filepath)
    model_string = sub(" $", "", model_string) # remove whitespace
    if(anyNA(.columnames)){
        .columnames <<- unlist(strsplit(model_string[1], split=" "))
        }
    model_string = model_string[-1]
    return(model_string)
    }


read_traces = function(filenames){
    traces = list()
    for(filename in filenames){
        trace = read_model_string(filename)
        traces[[filename]] = trace
        }
    return(traces)
    }


merge_traces = function(traces){
    trace = do.call(rbind, traces)
    return(trace)
    }


remove_burnin = function(traces, burnin, percentage){
    afterburn = list()
    for(name in names(traces)){
        trace = traces[[name]]
        n = length(trace)
        if(percentage){
            n_burn = trunc(burnin * n)
            } else {
            n_burn = burnin
            }
        if(burnin != 0){
            trace = trace[-seq_len(n_burn)]
            }
        text = paste0("Removing ", n_burn, " states out of ", n, " total.")
        stupidSignal::info(text)
        afterburn[[name]] = trace
        }
    return(afterburn)
    }


count_model_string = function(model_string, round){
    # get coutn of each mnodel string
    model_string_table = table(model_string)
    model_string_table = sort(model_string_table, decreasing=TRUE)

    # get percentage
    model_string_percentage = model_string_table/sum(model_string_table)

    # pretty print percentage
    class(model_string_percentage)=NULL # for formatC
    model_string_percentage = formatC(
        model_string_percentage, round, width=round+3, format="f"
        )

    # pretty print count
    class(model_string_table)=NULL # for formatC
    max_width = max(model_string_table)
    max_width = ceiling(log10(max_width))
    model_string_table = formatC(
        model_string_table, digits=max_width, width=max_width+1
        )

    # make a table with three columns: model string, count, percentage
    model_string_table = cbind(
        rownames(model_string_table),
        model_string_table,
        model_string_percentage
        )
    colnames(model_string_table) = c("model string", "count", "percentage")
    return(model_string_table)
    }


count_zero_frequency = function(model_string, columnames, round){
    model_matrix = strsplit(model_string, split=" ")
    model_matrix = matrix(
        unlist(model_matrix), nrow=length(model_matrix), byrow=TRUE
        )
    model_matrix = ifelse(model_matrix == "Z", 1, 0)
    count = colSums(model_matrix)
    percentage = colMeans(model_matrix)

    # pretty print count
    max_width = max(count)
    max_width = ceiling(log10(max_width))
    count = formatC(count, width=max_width+1, digits=max_width)

    # pretty print percentage
    percentage = formatC(percentage, round, width=round+3, format="f")
    
    # pretty print rates:
    rates = columnames
    if(anyNA(rates)){
        rates = seq_along(count)
        max_width = max(rates)
        max_width = ceiling(log10(max_width))
        rates = formatC(rates, width=max_width+1, digits=max_width)
        }

    # make table out of it:
    zero_frequency = cbind(rates, count, percentage)
    colnames(zero_frequency) = c("rate", "count", "percentage")
    return(zero_frequency)
    }


write_table = function(table, filepath, append){
    write.table(
        table, file=filepath, append=append,
        row.names=FALSE, col.names=TRUE, quote=FALSE, sep=" "
        )
    }


write_summary = function(zeros, model_string, filepath){
    # create connection to file
    output_con = file(filepath, open="w")
    # on exit of this function, close connection
    # This also prevents having opened connection after failing on some error
    on.exit(close(output_con))

    write_table(zeros, output_con, append=FALSE)
    cat(strrep("-", 60), "\n", file=output_con, append=FALSE)
    write_table(head(model_string), file=output_con, append=FALSE)
    }


main = function(args){
    model_strings = read_traces(args$input)
    model_strings = remove_burnin(model_strings, args$burnin, args$percentage)
    model_string = merge_traces(model_strings)
    # full output later, 
    model_string_table = count_model_string(model_string, args$decimal)
    zero_frequency = count_zero_frequency(model_string, .columnames, args$decimal)
    write_summary(zero_frequency, model_string_table, args$output)
    }


args_parser = function(){
    parser = arg_parser(
        paste0(
            "Count model strings from BayesTraits RJ MCMC analysis.\n",
            "This script will count number of occurence of all model strings",
            " from RJ MCMC analysis and output them sorted according to their",
            " count in table, together with percentage out of total states.\n",
            "The same is done for for frequency of zeros for individual rates.\n",
            "Script can additionally remove burnin, which can be stated as",
            " either count or percentage of total."
            )
        )
    parser = add_argument(
        parser, "--input", type="character", nargs="Inf",
        help="Input model strings from BayesTraits RJ MCMC analysis."
        )
    parser = add_argument(
        parser, "output", type="character",
        help="Output file with summarized model string."
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
        parser, "--header", flag=TRUE, help="File contains header."
        )
    parser = add_argument(
        parser, "--colnames", flag=TRUE,
        help="File contains column names (after header)."
        )
    parser = add_argument(
        parser, "--decimal", type="numeric", default=4,
        help="Round to specified number of digits."
        )
    args = parse_args(parser)
    return(args)
    }


if(!interactive()){
    args = args_parser()
    main(args)
    }
