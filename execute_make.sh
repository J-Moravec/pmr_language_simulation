#!bin/bash

set -u # stop if variables are not set
set -e # stop if error

################################################################################
# Setting
################################################################################
# datasets:
DIRS='Austronesian Bantu IndoEuropean UtoAztecan PamaNyungan'

# folders:
MAXLIKE=results/maximum_likelihood.info
REST=results

# non-optargs settings:
RECREATE_MAKEFILE=true
MAKE="remake"

# default settings:
DOMAXLIKE=false
DOANAL=false
NUMPROC=5
COMMAND=""
COPYRES=false
################################################################################



# help function
function HELP {
    echo -e \\n"Usage: $0 [-m] [-a] [-p <integer>] [-c <clean | reprocess | ...>] [-r]"
    echo -e "Helper script that compiles and runs makefiles for simulation of"
    echo -e "post-marital residence evolution."
    echo -e "By default, script recreates makefiles and exits."\\n
    echo -e "-m    performs short Maximum Likelihood analysis estimation"
    echo -e "-a    performs long Bayesian estimation and subsequent analyses"
    echo -e "-p    number of processors used during analysis, default is $NUMPROC"
    echo -e "-c    specify target for makefiles if different than whole"
    echo -e "      analysis is to be performed (e.g., clean to remove all"
    echo -e "      but source files or reprocess to summarize results)"
    echo -e "-r    copy results from local dataset folders to global folder"
    echo -e "-d    datasets (directories) for which analysis will be performed"
    echo -e "-h    prints this help and exits"\\n
    }

# check number of arguments:

### getopts ###
while getopts :map:c:rd:h FLAG; do
    case $FLAG in
        m)
            DOMAXLIKE=true
        ;;
        a)
            DOANAL=true
        ;;
        p)
            NUMPROC=$OPTARG
        ;;
        c)  
            COMMAND=$OPTARG
        ;;
        r)
            COPYRES=true
        ;;
        d)  
            DIRS=$OPTARG
            echo "Setting datasets to: $DIRS"
        ;;
        h)
            HELP
            exit 0
        ;;
        \?)
            HELP
            echo -e "\\nERROR: Option -$OPTARG not recognized."
            exit 1
        ;;
    esac
done

shift $((OPTIND-1))

for NAME in $DIRS; do
    DIR=datasets/$NAME
    if [ ! -f $DIR/makefile -o RECREATE_MAKEFILE ]; then
        echo "Recreating makefile for $NAME"
        cat makefiles/setting.global makefiles/setting.${NAME} \
            makefiles/targets.${NAME} makefiles/targets.global > $DIR/makefile
        fi
    done


if [ "$DOMAXLIKE" = "true" ]; then
    mkdir -p $REST
    echo > $MAXLIKE
    for NAME in $DIRS; do
        echo "Processing Maximum Likelihood estimate for dataset: $NAME"
        DIR=datasets/$NAME
        (cd $DIR && $MAKE -j 5 maxlike)
        echo $NAME >> $MAXLIKE
        cat $DIR/$MAXLIKE >> $MAXLIKE
        done
    fi


if [ "$DOANAL" = "true" ]; then
    for NAME in $DIRS; do
        DIR=datasets/$NAME
        if [ -z "$COMMAND" ]; then
            echo "Processing dataset: $NAME"
            (cd "$DIR" && "$MAKE" -j "$NUMPROC")
            else
            echo "Processing command $COMMAND on dataset: $NAME"
            (cd "$DIR" && "$MAKE" -j "$NUMPROC" "$COMMAND")
            fi
        done
    fi


if [ "$COPYRES" = "true" ]; then
    mkdir -p $REST
    for NAME in $DIRS; do
        DIR=datasets/$NAME    
        name=$(echo "$NAME" | tr "[:upper:]" "[:lower:]")    
        cp $DIR/$REST/*.txt $REST/
        cp $DIR/$REST/*.pdf $REST*
        cp -r $DIR/$REST/dens $REST/${name}_dens
        cp -r $DIR/$REST/dens_kappa $REST/${name}_dens_kappa
        done
    fi

echo -e "#######################################################################"
echo -e "Finished. I hope I helped."
