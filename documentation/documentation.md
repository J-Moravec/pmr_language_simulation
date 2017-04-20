title: Pipeline documentation
tags: ["bah"]
----
# Pipeline documentation

This documentation should provide a short overview on structure, aims and function of pipeline as a whole and its individual parts.

## Design aims and problems

In this work, one of our target was to produce open and repeatable analysis and partially because of that, some code became a bit more complicated, as it had to handle specificities in each dataset. This is notable especially in *preprocessing* step. Furthermore, bugs and/or *design features* in used software and packages exacerbated this even more, such as notably bad `ape`'s code for reading Nexus trees, which can't handle if the translation matrix is not ordered. This forced us to use `python` and `dendropy`, which would not be otherwise required. Overall, the pipeline is mostly written in `R`, utilizing `bash` in a few parts where problems could be easily solved using highly optimized programs `sed` and `cut` for fast processing of large outputs from `BayesTraits`. Finally, individual scripts form isolated steps that are connected with `make`, note that you need `3.82` or higher version of `GNU make` as we use `.RECIPEPREFIX` variable in our makefiles.

## Pipeline structure:
Our pipeline is divided into *preprocessing*, *analysis step* and *postprocessing*:
* **preprocessing** -- prepares source file for BayesTraits analysis
  *  reformat source files from their original format into new unified format
  * compare languages in residence file with languages in tree file and report any problems (i.e., missing names from one or the other)
  * prune/remove languages that are not in tree/table
  * recode according to specified *coding scheme*
  * create PDF of tree with mapped residence states
  * create input setting file for `BayesTraits`
* **analysis** -- main analysis using `BayesTraits`, this is the most time-consuming step
* **postprocessing** -- test for convergence, summarize results and perform SIMMAP analysis
  * testing for convergence
  * summarizing results with various statistics into formated tables
  * SIMMAP simulation

Only the *preprocessing* step is specific for each dataset, all other steps are exactly the same. This is also reflected in the form of makefiles. Due to lack of templating capabilities in standard makefiles, we have divided our makefiles into four parts, which are connected together before analysis:

* setting.global -- various settings that are common for each dataset
* setting.local -- local setting for specific dataset
* targets.local -- preprocessing step which is specific for every dataset
* targets.global -- all others general steps

## Used scripts and their short description:

In total, 15 scripts were used to perform analysis. Here is short description of them:

#### stupidSignal
Package providing colored wrappers around standard `R` signals (`stop()`, `warning()` and `message()`), so that they can be easily distinguished in output. Note that the coloring code does not currently determine if terminal is capable of handling colors, so if it is not capable, or if output is send to file instead of graphical terminal, then you will see the special coloring marks.


#### check_names.r
Compare two tables or table and tree and report if any item from tree is missing from table or if any item from table is missing from tree. Both tree-names and table-names are set to lowercase. Both tree and table can be pruned (remove unvanted languages) by specifying `--prune` or `--reduce` arguments.


#### generate_config.r
Generate config file for `BayesTraits` with default setting. Setting can be changed by specifying arguments. Thus, using makefile part `setting.global`, config can be easily set globally for every dataset just in single line.

#### model_string.r
After using `remove_head.sh`, model string, which describe parametrization of rate matrix, can be analyzed using this script.

#### parse_ml.r
Instead of full Bayesian analysis with `BayesTraits`, one can also run Maximum Likelihood. This was used in analysis to determine RJ MCMC priors. This script will summarize results from ML analysis, which can be run through `execute_make.sh`.

#### recode.r
This script is used in `preprocessing` to recode residence according to specified coding scheme. This enables anyone to change from our 4 states (patrilocal, matrilocal, ambilocal, neolocal) to 3 states (patrilocal, matrilocal, ambilocal+neolocal) and so on. Recode also output residence table in format expected by `BayesTraits`.

#### reformat_tree.py
The only `python` script. Some trees that we have obtained did have slightly misformed translation matrix (part of NEXUS specification, traditionally tip names are translated into numbers, which are then used in TREE part of NEXUS file), they were out of order as if someone manually deleted whole tip from matrix and tree, bust still perfectly valid. `ape`'s function for reading NEXUS files was unable to deal with it and authors commented that this is by design. Thus, we had to use `python` to just load and save again NEXUS file to correct this.

#### remove_head.sh
Bash script that process `BayesTraits` output by removing the header and model string. This produce DSV (delimiter-separated values), which can be easily processed by `R`. This can also parse only the model string part of output, which is then processed by `model_string.r`.

#### rename.r
Originally used for renaming residence table to remove conflicts with names in tree. This function was absorbed by `check_names.r` and this script is not used in pipeline.

#### show_mapping.r
Produce image of trees with mapped post-marital residence states after (or at the end) of postprocessing stage. To summarize posterior samples of trees, single maximum clade credibility tree is created, which is the used for SIMMAP analysis.

#### simulate_simmap.r
Code to run SIMMAP analysis using `phytools` library.

#### summarize_rates.r summarize_traces.r summarize_trees.r
These functions analyse output from `BayesTraits` after it was preprocessed by `remove_head.sh`. Script `summarize_rates.r` summarize rates (as name suggest), script `summarize_trees.r` will summarize posterior sample of trees (such as their size, total sum of all branches, number of tips...) and `summarize_traces.r` will perform analysis of convergence using `coda` package. Note that this package throws a lot of warning and errors (matrix operations on with small numbers), so please ignore them.

#### unify_format.r
This script will take table or part of table and output this talbe in a new format with unified separator, presence of header and so on across all datasets.
