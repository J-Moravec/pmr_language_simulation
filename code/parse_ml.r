library("argparser", quiet=TRUE)
library("simpleSignal", quiet=TRUE)



get_results = function(text){
    # find "Tree No", as this signify start of result header:
    start = grep(pattern="Tree No", text)
    text = text[start:length(text)]
    # now split both lines (header and results) with '\t'
    ml = strsplit(text, split="\t", fixed=TRUE)
    results = as.numeric(ml[[2]])
    names(results) = ml[[1]]
    return(results)
    }


get_rates = function(results){
    # anything that starts with q:
    rates = results[grep("^q", names(results))]
    }


pretty_print = function(vec){
    decimal = 2
    max_width_names = max(nchar(names(vec))) + 1
    max_width_vec = ceiling(log10(max(abs(vec)))) + decimal + 2
    max_width = max(max_width_names, max_width_vec)
    names(vec) = sub(" ", "_", names(vec), fixed=TRUE)
    textnames = formatC(names(vec), width=max_width)
    textnames = paste0(textnames, collapse="")

    textvec = formatC(vec, digits=2, format="f", width=max_width)
    textvec = paste0(textvec, collapse="")
    text = paste0(textnames, "\n", textvec, "\n\n")
    return(text)
    }

main = function(args){
    text = readLines(args$input)
    ml_result = get_results(text)

    # print either whole results or only rates
    ml_rates = get_rates(ml_result)

    if(args$rates){
        text = pretty_print(ml_rates)
        } else {
        text = pretty_print(ml_result)
        }
    cat(text, file=args$output)
    }


args_parser = function(){
    parser = arg_parser(
        paste0(
            "Parse BayesTraits maximum likelihood output and prints",
            " it with equally wide columns"
            )
        )
    parser = add_argument(
        parser, "input", type="character",
        help="output from BayesTraits ML analysis"
        )
    parser = add_argument(
        parser, "output", type="character", help="Parsed ML output"
        )
    parser = add_argument(
        parser, "--rates", flag=TRUE, help="Output only rates"
        )
    args = parse_args(parser)
    return(args)
    }


if(!interactive()){
    args = args_parser()
    main(args)
    }
