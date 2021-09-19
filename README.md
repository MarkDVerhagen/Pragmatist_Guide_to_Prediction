## A Pragmatist's Guide to Prediction in the Social Sciences

This repository contains code and reproduction materials for the Pragmatist's Guide to Prediction [paper](https://osf.io/preprints/socarxiv/tjkcy). Most data is publicly available (see Data section) and should be placed in the `/data` folder. For the examples using data on tracking in the Netherlands, the data is sensitive and cannot be shared publicly. Full code underlying the analyses can be found in [this repository](). For more information see the paper [here](https://osf.io/preprints/socarxiv/y6mnb/).

### Repo structure

The repository consists of three scripts generating the three substantive figures in the paper (scripts `1x_.*.R`). In addition, code to generate the datasets -- in case the data is publicly available are provided as well -- (scripts `0x_.*.R`). Minimal code to reproduce each of the figures is enclosed in the repository. To generate the datasets, some data will have to be downloaded and accessed through external parties (e.g. the Fragile Families Challenge).

### Datasets

Four datasets are used throughout the paper. They are introduced below including downloading links. Minimal reproducible versions of the dataset are enclosed in this repository to reproduce the figures and main results (without need to run the `0x_.*R` scripts) in the `/data/to_plot/` folder which can be downloaded as parts of this repository.

The first dataset used are the results of the Fragile Families Challenge. The data can be obtained from the [Princeton Office of Population Research](https://opr.princeton.edu/archive/restricted/) and require the user to register and apply for access to the dataset.

The second dataset used is a simulation of years of education and wages and can be generated by running the `01_data_mincerian.R` script. No additional downloads are necessary.

The third dataset used is available from Stata .

The fourth dataset contains sensitive student-level data and cannot be shared. The code used to analyze the data and extensive descriptives can be found elsewhere, for more information you can read the paper [here](https://osf.io/preprints/socarxiv/y6mnb/).

To run the code in this repository, generate an additional folder in the repository with the following structure:

```bash
├── data
│   ├── ffc
│   │   ├──  background.dta
│   │   ├──  train.csv
│   │   ├──  test.csv
│   │   ├──  submissions.csv
│   └── mincerian
│   │   ├── GSS2018.dta
│   ├── mortgage
│   │   ├── mortgage.csv
│   ├── to_plot
```

### Generating the plotting data

After obtaining the data, run the three `0x_.*.R` scripts to generate the datasets used to generate the figures. Note that tha `02_data_ffc.R` is a copy of the code provided by the FFC project, written by [Ian Lundberg](https://github.com/ilundberg).
