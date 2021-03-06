## A Pragmatist's Guide to Prediction in the Social Sciences

This repository contains code and reproduction materials for the Pragmatist's Guide to Prediction [paper](https://osf.io/preprints/socarxiv/tjkcy). Most data is publicly available (see Data section) and should be placed in the `/data` folder. For the examples using data on tracking in the Netherlands, the data is sensitive and cannot be shared publicly. Full code underlying the analyses can be found in [this repository](https://github.com/MarkDVerhagen/Prediction_Teacher_Bias). For more information see the paper [here](https://osf.io/preprints/socarxiv/y6mnb/).

### Repo structure

The repository consists of three scripts generating the three substantive figures in the paper (scripts `1x_.*.R`). These scripts use a number of datasets which are generated using five data preprocessing scripts (scripts `0x_.*.R`). Minimal code to reproduce each of the figures is enclosed in the repository. To generate the datasets, some data will have to be downloaded and accessed through external parties (e.g. the Fragile Families Challenge). Instructions are included below.

### Datasets

Five sets of data are used throughout the paper. They are introduced below including downloading links, when available. Minimal reproducible versions of the dataset are enclosed in this repository to reproduce the figures and main results (without need to run the `0x_.*R` scripts), which can be downloaded as parts of this repository. Only the data underlying Figure 1D and Figure 3B cannot be publicly shared.

The first dataset used are the results of the Fragile Families Challenge. The data can be obtained from the [Princeton Office of Population Research](https://opr.princeton.edu/archive/restricted/) and require the user to register and apply for access to the dataset.

The second dataset concerns the HMDA data available from Stata, see [this paper](https://journals.sagepub.com/doi/full/10.1177/0049124115610347?casa_token=VbI0inJuP90AAAAA%3Ao3h-wllgyhMUGBKypBPtTnH-3ncx9U5HZd3uNZ93KWF3l9_ZoPqCgIsKujzR0OTlBT8Z3R0roJVMeA) for an example.

The third dataset contains sensitive student-level data and cannot be shared. The code used to analyze the data and extensive descriptives can be found elsewhere, for more information you can read the original paper [here](https://osf.io/preprints/socarxiv/y6mnb/). The two datasets with predictions underlying the figures in the paper can be made available on request from the authors of the study.

The fourth dataset can be downloaded from the reproduction materials of [this paper](https://www.aeaweb.org/articles?id=10.1257/aer.96.3.461).

The fifth dataset used is a simulation of years of education and wages and can be generated by running the `01_data_mincerian.R` script. It uses the 2018 General Social Survey as a framework to generate a synthetic dataset, which can be downloaded [here](https://gss.norc.org/).

The fifth dataset concerns wage data in the United States and is available from [openicpsr](https://www.openicpsr.org/openicpsr/project/116216/version/V1/view;jsessionid=10AD4ACEB1CC6CC11B75CFCD3DDFB7D5).

To run the code in this repository, generate an additional folder in the repository with the following structure:

```bash
????????? data
???   ????????? ffc
???   ???   ?????????  background.dta
???   ???   ?????????  train.csv
???   ???   ?????????  test.csv
???   ???   ?????????  submissions.csv
???   ????????? mincerian
???   ???   ????????? GSS2018.dta
???   ????????? mortgage
???   ???   ????????? mortgage.dta
???   ????????? lemieux
???   ???   ????????? morgm.sas7bdat
???   ????????? teacher_bias
???   ???   ????????? performance_simulation.rds
???   ???   ????????? 1_250_cross_within.rds
???   ???   ????????? teacher_bias_perf.rds
???   ????????? edit
```

### Generating the plotting data

After obtaining the data, run the three `0x_.*.R` scripts to generate the datasets used to generate the figures. Note that tha `02_data_ffc.R` is a copy of the code provided by the FFC project, written by [Ian Lundberg](https://github.com/ilundberg).
