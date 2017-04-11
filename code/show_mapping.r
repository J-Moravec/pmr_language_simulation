library("ape")
library("argparser", quiet=TRUE)
library("phangorn")

main = function(args){
    .colors = c(
        "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33",
        "#A65628", "#F781BF", "#999999"
        )

    if(is.na(args$colors)){
        args$colors = .colors
        }

    read_table = function(tablepath){
        temp = read.table(tablepath, header=FALSE, stringsAsFactors=FALSE)
        return(temp)
        }

    get_names_to_states = function(table){
        names_to_states = table[, 2]
        names(names_to_states) = table[, 1]
        return(names_to_states)
        }

    read_tree = function(treepath){
        temp = read.nexus(treepath)
        return(temp)
        }



    get_states_to_colors = function(states, colors){
        states_to_colors = colors[seq_along(states)]
        names(states_to_colors) = states
        return(states_to_colors)
        }

    plot_tree = function(tree, imagepath, color_vec, colors, states, position,
                         size){
        pdf(imagepath, width=size[1], height=size[2])
        plot(
            tree, edge.width=3, tip.color=color_vec, font=2, node.depth=2,
            align.tip.label=TRUE, underscore=TRUE
            )
        par(mar=c(0,0,0,0))
        plot.window(c(0,100), c(0,100))
        legend(position, fill=colors, legend=states, bty="n", xpd=TRUE,
            border=FALSE)
        invisible(dev.off())
        }


    tree = read.nexus(args$tree)
    if(class(tree) == "multiPhylo"){
        # construct maximum clade credibility tree
        tree = phangorn::maxCladeCred(tree)
        } else if(class(tree) == "phylo"){
        # do nothing
        } else {
        stop("Tree must be class phylo or multiPhylo")
        }
    if(!is.na(args$max_cred)){
        write.nexus(tree, file=args$max_cred)
        }
    if(!is.na(args$image)){
        table = read_table(args$table)
        states = unique(table[,2])
        if(length(args$colors) < length(states)){
            stop("Number of states is greater than the number of colors.")
            }
        # create translating vector between names and states
        names_to_states = get_names_to_states(table)
        # create translating vector between states and colors
        states_to_colors = get_states_to_colors(states, args$colors)
        #print(names_to_states[tree$tip.labels])
        translated_names = states_to_colors[names_to_states[tree$tip.label]]
        plot_tree(tree, args$image, translated_names, args$colors, states,
                  args$legend, args$size)
        }
    }


args_parser = function(){
    parser = arg_parser(
        paste0("Color tree according to states in table.")
        )
    parser = add_argument(
        parser, "tree", type="character", help="Input tree in nexus format."
        )
    parser = add_argument(
        parser, "--table", type="character", help="Input table with states."
        )
    parser = add_argument(
        parser, "--legend_coords", type="numeric", nargs=2,
        help=paste0(
            "Position of legend in x and y coordinates. Irrespective of",
            " size or aspect ratio, coordinates range from 0 to 100.",
            " So 0 0 would be lower left corner while 100 100 upper right",
            " corner. These coordinates take in acount total size of image,",
            " thus they ignore margings, although there still might be",
            " a bit of space around."
            )
        )
    parser = add_argument(
        parser, "--size", type="numeric", default=c(7,7), nargs=2,
        help="Size of image in inches. Default values are 7 and 7."
        )
    parser = add_argument(
        parser, "--add", flag=TRUE,
        help="Arguments for size are added to default values instead of set to."
        )
    parser = add_argument(
        parser, "--legend_pos", default="topleft",
        help="String positioning legend. Default is topleft."
        )
    parser = add_argument(
        parser, "--image", type="character",
        help="Name of output image."
        )
    parser = add_argument(
        parser, "--colors", nargs=Inf, type="character",
        help=paste0(
            "Colors used for coloring states. If not specified,",
            "Set1 palette from RColorBrewer is used."
            )
        )
    parser = add_argument(
        parser, "--max_cred", type="character",
        help=paste0(
            "Path for maximum clade credibility tree. If input is sample of",
            " trees, maximum clade credibility tree is outputted. If input",
            " is a single tree, it is saved into specified path."
            )
        )
    args = parse_args(parser)
    return(args)
    }


if(!interactive()){
    args = args_parser()
    if(args$add){
        args$size = args$size + c(7,7)
        }
    if(any(is.na(args$legend_coords))){
        args$legend = args$legend_pos
        } else {
        args$legend = args$legend_coords
        }
    if(is.na(args$table) && !is.na(args$image)){
        stop("If image is specified, you must specify table as well.")
        }
    main(args)
    }
