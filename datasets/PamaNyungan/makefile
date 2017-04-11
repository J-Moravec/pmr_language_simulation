# setting RECIPEPREFIX to plus sign:
.RECIPEPREFIX = +


# these folders are already existing
SOURCE = sources
SCRIPTS = ../../code

# coding scheme
CODING_SCHEME = ../../coding_schemes/m_p_a_n.txt

# number of runs
NRUNS := 5
# generating integer list
RUNS := $(shell seq 1 $(NRUNS))


# these folders must be created, if they doesn't exist
PREP = preprocessing
LOGS = log
DATA = data
ANAL = analysis
POST = postprocessing
REST = results
DIRS = $(PREP) $(LOGS) $(DATA) $(ANAL) $(POST) $(REST)

# commands shortcuts:
UNIFY_FORMAT = Rscript $(SCRIPTS)/unify_format.r
CHECK_NAMES = Rscript $(SCRIPTS)/check_names.r
RECODE = Rscript $(SCRIPTS)/recode.r
COLOR = Rscript $(SCRIPTS)/show_mapping.r
MODEL_STRING = Rscript $(SCRIPTS)/model_string.r
SUMMARIZE = Rscript $(SCRIPTS)/summarize_rates.r
CONFIG = Rscript $(SCRIPTS)/generate_config.r
BAYES_TRAITS = $(SCRIPTS)/BayesTraitsV2
REFORMAT_TREE = python $(SCRIPTS)/reformat_tree.py
REMOVE_HEAD = $(SCRIPTS)/remove_head.sh
PARSE_ML = Rscript $(SCRIPTS)/parse_ml.r
CONVERGENCE = Rscript $(SCRIPTS)/summarize_traces.r
TREESTATS = Rscript $(SCRIPTS)/summarize_trees.r
SIMMAP = Rscript $(SCRIPTS)/simulate_simmap.r

# define BayesTraits analysis:
ITERATIONS = 100000000
SAMPLE = 10000
RJHP = "rjhp exp 0 200"
KAPPA = "rjhp exp 0 200\nKappa"

# burnin for summarization of BayesTraits results
BURNIN = 0.1

# number of SIMMAP simulations
SIMMAP_NSIM = 2000

# define all parts of analysis
# unfortunatelly, it must be here as a first target
#   create dirs: basic directory structure
#   preprocess: preprocess files, specific for each dataset
#   recode: recode PMR into chosed coding scheme and show its mapping on tree
#   analyze: run BayesTraits analysis
#   postprocess: summarizing BayesTraits analysis
all: create_dirs preprocess recode analyze postprocess


.PHONY: create_dirs preprocess recode analyze postprocess
# name of analysis
NAME = pamanyungan

# which tree will be used
TREE = $(NAME).trees

# setting RJHP for PamaNyungan
RJHP = "rjhp exp 0 400"
# ------------------------------------------------------------------------------
# This is specific part of makefile for Pama Nyungan dataset

preprocess: unify_formatting check_tree




unify_formatting: $(PREP)/residence.txt

$(PREP)/prep_residence.txt: $(SOURCE)/residence.txt | $(DIRS)
+ $(UNIFY_FORMAT) $< $@ --sep " " --colnames "name" "residence" --header




check_tree: $(PREP)/$(TREE) $(DATA)/$(TREE)

$(PREP)/$(TREE): $(SOURCE)/$(TREE)
+ $(REFORMAT_TREE) $< $@

$(DATA)/$(TREE): $(PREP)/$(TREE) $(PREP)/prep_residence.txt $(SOURCE)/prune.txt $(SOURCE)/reduce.txt
+ $(CHECK_NAMES) $< $(word 2,$^) --tree --save $@ --rename $(PREP)/renaming.txt \
 --log $(LOGS)/check_tree.log --prune $(SOURCE)/prune.txt \
 --reduce $(SOURCE)/reduce.txt --reduce_save $(PREP)/residence.txt

$(PREP)/residence.txt: $(DATA)/$(TREE)

# end of specific part
# ------------------------------------------------------------------------------
create_dirs: $(DIRS)

$(DIRS):
+ mkdir -p $@




recode: $(DATA)/residence.txt $(DATA)/mapped_states.pdf

$(DATA)/residence.txt: $(PREP)/residence.txt $(CODING_SCHEME)
+ $(RECODE) $< $(word 2,$^) $@

$(DATA)/mapped_states.pdf: $(DATA)/$(TREE) $(DATA)/residence.txt
+ $(COLOR) $< --table $(word 2,$^) --image $@ --add --size 4 14 --legend_coords 102 0

$(DATA)/max_clade_cred.tree: $(DATA)/$(TREE)
+ $(COLOR) $< --max_cred $@

## for single file:
#analyze: $(ANAL)/$(NAME).log
#$(DATA)/$(NAME).cmd:
#+ $(CONFIG) --LOGFILE $(ANAL)/$(NAME).log --ITERATIONS $(ITERATIONS) > $@
#
#$(ANAL)/$(NAME).log: $(DATA)/$(NAME).cmd $(DATA)/$(TREE) $(DATA)/residence.txt
#+ $(BAYES_TRAITS) $(word 2,$^) $(word 3,$^) < $<


# maximum likelihood estimate for setting priors
maxlike: $(REST)/maximum_likelihood.info $(REST)/maximum_likelihood.out

$(REST)/maximum_likelihood.info: $(ANAL)/maximum_likelihood.log
+ $(PARSE_ML) $< $@ --rates

$(REST)/maximum_likelihood.out: $(ANAL)/maximum_likelihood.log
+ $(PARSE_ML) $< $@

$(ANAL)/maximum_likelihood.log: $(DATA)/maximum_likelihood.cmd $(DATA)/max_clade_cred.tree $(DATA)/residence.txt
+ $(BAYES_TRAITS) $(word 2, $^) $(word 3, $^) < $< > /dev/null

$(DATA)/max_clade_cred.tree: $(DATA)/mapped_states.pdf

$(DATA)/maximum_likelihood.cmd: | $(DIRS)
+ $(CONFIG) --LOGFILE $(ANAL)/maximum_likelihood.log --TYPE 1 > $@





analyses := $(foreach RUN, $(RUNS), $(ANAL)/$(NAME).log.$(RUN) )
cmdinputs := $(foreach RUN, $(RUNS), $(DATA)/$(NAME).cmd.$(RUN) )

analyses_kappa := $(foreach RUN, $(RUNS), $(ANAL)/$(NAME)_kappa.log.$(RUN) )
cmdinputs_kappa := $(foreach RUN, $(RUNS), $(DATA)/$(NAME)_kappa.cmd.$(RUN) )

analyze: $(analyses) $(analyses_kappa)

$(analyses): $(ANAL)/$(NAME).log.% : $(DATA)/$(NAME).cmd.% $(DATA)/$(TREE) $(DATA)/residence.txt
+ $(BAYES_TRAITS) $(word 2, $^) $(word 3, $^) < $< > /dev/null
+ echo "Finished run $*"

$(cmdinputs): $(DATA)/$(NAME).cmd.%: | $(DIRS)
+ $(CONFIG) --LOGFILE $(ANAL)/$(NAME).log.$* --ITERATIONS $(ITERATIONS) \
 --SAMPLE $(SAMPLE) --OTHER $(RJHP) > $@

$(analyses_kappa): $(ANAL)/$(NAME)_kappa.log.% : $(DATA)/$(NAME)_kappa.cmd.% $(DATA)/$(TREE) $(DATA)/residence.txt
+ $(BAYES_TRAITS) $(word 2, $^) $(word 3, $^) < $< > /dev/null

$(cmdinputs_kappa): $(DATA)/$(NAME)_kappa.cmd.%: | $(DIRS)
+ $(CONFIG) --LOGFILE $(ANAL)/$(NAME)_kappa.log.$* --ITERATIONS $(ITERATIONS) \
 --SAMPLE $(SAMPLE) --OTHER $(KAPPA) > $@




traces := $(foreach RUN, $(RUNS), $(POST)/$(NAME).trace.$(RUN) )
model_strings := $(foreach RUN, $(RUNS), $(POST)/$(NAME).modstr.$(RUN) )

traces_kappa := $(foreach RUN, $(RUNS), $(POST)/$(NAME)_kappa.trace.$(RUN) )
model_strings_kappa := $(foreach RUN, $(RUNS), $(POST)/$(NAME)_kappa.modstr.$(RUN) )


postprocess: $(POST)/convergence_timestamp.txt \
 $(POST)/convergence_timestamp_kappa.txt \
 $(REST)/$(NAME)_stats.txt $(REST)/$(NAME)_stats_kappa.txt \
 $(REST)/$(NAME)_mapped_states.pdf \
 $(REST)/$(NAME).modstr.txt $(REST)/$(NAME)_kappa.modstr.txt \
 $(REST)/$(NAME)_treestats.txt $(REST)/$(NAME)_transitions.txt

$(POST)/convergence_timestamp.txt: $(traces)
+ $(CONVERGENCE) -i $^ --dirname $(REST)/conv --timestamp $@

$(POST)/convergence_timestamp_kappa.txt: $(traces_kappa)
+ $(CONVERGENCE) -i $^ --dirname $(REST)/conv_kappa --timestamp $@

$(traces): $(POST)/$(NAME).trace.% : $(ANAL)/$(NAME).log.%
+ $(REMOVE_HEAD) -r $< $@

$(model_strings): $(POST)/$(NAME).modstr.% : $(ANAL)/$(NAME).log.%
+ $(REMOVE_HEAD) -ro $< $@

$(traces_kappa): $(POST)/$(NAME)_kappa.trace.% : $(ANAL)/$(NAME)_kappa.log.%
+ $(REMOVE_HEAD) -r $< $@

$(model_strings_kappa): $(POST)/$(NAME)_kappa.modstr.% : $(ANAL)/$(NAME)_kappa.log.%
+ $(REMOVE_HEAD) -ro $< $@

$(REST)/$(NAME)_stats.txt: $(traces)
+ $(SUMMARIZE) $@ --input $(traces) --burnin $(BURNIN) --percentage --matrix $(POST)/rate_matrix.txt --imagedir $(REST)/dens

$(REST)/$(NAME)_stats_kappa.txt: $(traces_kappa)
+ $(SUMMARIZE) $@ --input $(traces_kappa) --burnin $(BURNIN) --percentage --imagedir $(REST)/dens_kappa --kappa

$(REST)/$(NAME)_mapped_states.pdf: $(DATA)/mapped_states.pdf
+ cp $< $@

$(REST)/$(NAME).modstr.txt: $(model_strings)
+ $(MODEL_STRING) $@ --input $(model_strings) --burnin $(BURNIN) --percentage --decimal 4

$(REST)/$(NAME)_kappa.modstr.txt: $(model_strings_kappa)
+ $(MODEL_STRING) $@ --input $(model_strings_kappa) --burnin $(BURNIN) --percentage --decimal 4

$(REST)/$(NAME)_treestats.txt: $(DATA)/$(TREE)
+ $(TREESTATS) $< $@

$(REST)/$(NAME)_transitions.txt: $(DATA)/residence.txt $(DATA)/max_clade_cred.tree | $(REST)/$(NAME)_stats.txt
+ echo "Simmulating SIMMAP, this may take a while."
+ $(SIMMAP) --tree $(DATA)/max_clade_cred.tree --residence $(DATA)/residence.txt \
 --matrix $(POST)/rate_matrix.txt --nsim $(SIMMAP_NSIM) --changes $@ \
 --times $(REST)/$(NAME)_timesinstates.txt


clean:
+ rm -r $(DIRS)

reprocess:
+ rm -r $(POST) $(REST)
+ $(MAKE)

.PHONY: clean reprocess
