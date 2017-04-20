## A supplement to *Few Global Commonalities in Transition Dynamics between Post-Marital Residence States* (Moravec et al. 2017)

This repository contains code and source files to fully replicate analysis from *Few Global Commonalities in Transition Dynamics between Post-Marital Residence States* (Moravec et al. 2017). Output files from analysis itself are not included due to size limitations. Input and output files of phylogenetic reconstruction of Pama-Nyungan language family are not included as well for similar reasons.

Analysis was performed on GNU/Linux operating system using `BayesTraits v2`, `R`, `Python`, `make`/`remake` and liberal amount of `bash` scripts. The usage of most recent version of all tools is suggested.

Used `R` packages: `ape`, `phangorn`, `phytools`, `coda`, `argparser` and self-made package `stupidSignal` (code provided)

Used `Python` packages: `dendropy` and `argparse`

Note that due to usage of GNU tools, Unix systems using POSIX-only compilant tools (e.g., MacOS) might have some problems (e.g., interpretation of special characters in `echo`). Generally, `bash` scripts do not form critical part of the pipeline and could be replaced (e.g., by `R` scripts) to make this analysis possible on Windows operating systems as well.

Folder structure is as follows:
* **code** -- scripts required for analysis
* **coding_schemes** -- scripts were written as to enable recoding of data into different format (i.e., recoding ambilocal as unknown state between matrilocality and patrilocality), this folder contain these coding formats
* **datasets** -- with folders for individual language families, currently contain only pregenerated makefile (which is automatically assembled during analysis) and folder with source files
* **documentation** -- more detailed documentation
* **makefiles** -- templates to generate makefiles

To run analysis, run the script `execute_make.sh -h` and follow its instructions.

For more information, read the documentation:
* in HTML format: [here](https://htmlpreview.github.io/?https://github.com/J-Moravec/pmr_language_simulation/blob/master/documentation/documentation.html)
* in markdown format: [here](documentation/documentation.md)
