#!/bin/bash
set -u # stop if variables are not set
set -e # stop if error

# help function
function HELP {
    echo -e \\n"Usage: $0 [-r] [-h] [-c <integer>] <input> <output>"
    echo -e "Removes head from BayesTraits2 to create trace file that can be"
    echo -e "processed with Tracer from Beast package."\\n
    echo -e "-r    remove single or multiple columns from trace"
    echo -e "-c    specify column to be removed, default is 7 (model string"
    echo -e "      from RJ MCMC analysis)"
    echo -e "-o    column only, gets column with model string from RJ MCMC"
    echo -e "      instead of whole output without model string;"
    echo -e "-h    prints this help and exits"\\n
    }


# check number of arguments:
NUMARGS=$#
if [ $NUMARGS -eq 0 ]; then
    HELP
    echo -e "\\nERROR: Not enough arguments."
    exit 1
fi


# initialize REMOVE
REMOVE=false
COLUMN=7
ONLY=false

### getopts ###
while getopts :roc:h FLAG; do
    case $FLAG in
        r) # set option for r
            REMOVE=true 
        ;;
        c) # which column
            COLUMN=$OPTARG
        ;;
        o) # column only -- model string column from RJ MCMC analysis
            ONLY=true
        ;;
        h) # show help
            HELP
            exit 0
        ;;
        \?) # unrecognized option -- show help
            HELP
            echo -e "\\nERROR: Option -$OPTARG not recognized."
            exit 1
        ;;
    esac
done

shift $((OPTIND-1)) # Tells getopts to move to next argument

INPUT=$1
OUTPUT=$2

if [ ! -f "$INPUT" ]
then
    echo "File $INPUT does not exist!"
    exit 1
elif [ "$OUTPUT" == "" ]
then
    echo "Output can't be empty string"
    exit 1
else
    LINE=`grep -n "Iteration[^s]" $INPUT`
    LINE=${LINE%%":"*}
    tail -n +$LINE $INPUT > $OUTPUT

    if [ $REMOVE == true ]
    then
        if [ $ONLY == true ]
        then
            # get header from file
            head -n 1 $OUTPUT > $OUTPUT.head
            cut -f $COLUMN $OUTPUT > $OUTPUT.cut
            sed -i "s/'//g" $OUTPUT.cut
            NRATES=(`head -n 2 $OUTPUT.cut | tail -n 1`)
            NRATES=${#NRATES[*]}
            COLNAMES=`cut -f $(($COLUMN+1))-$(($COLUMN+$NRATES)) $OUTPUT.head`
            COLNAMES=($COLNAMES)
            COLNAMES=${COLNAMES[*]}
            sed -i "1 s/Model string/$COLNAMES/" $OUTPUT.cut
            rm $OUTPUT.head
        else
            cut -f $COLUMN --complement $OUTPUT > $OUTPUT.cut
        fi
        mv $OUTPUT.cut $OUTPUT
    fi
fi
