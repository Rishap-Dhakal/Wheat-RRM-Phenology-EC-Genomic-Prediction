# Random regression models with genotype-specific phenology-driven environmental covariates for genomic prediction in wheat

R code accompanying the paper:

> **Improving genomic prediction in wheat with random regression models with genotype-specific phenology-driven environmental covariates**
> <!-- Add authors, journal, year and DOI here -->

## Overview

This repository contains the ASReml-R scripts used to fit and cross-validate **random regression models (RRM)** for genomic prediction of wheat yield across multi-environment trials. Instead of modelling each environment as a separate level, genotype-by-environment (G×E) interaction is modelled as a reaction norm of each cultivar along one or more **environmental covariates (ECs)**. The ECs are summarised over genotype-specific crop phenology stages (vegetative and flowering periods), so each cultivar experiences the environment relative to its own development.

Genomic relationships enter through a sparse inverse genomic relationship matrix (`GINV.sparse`), and the intercept and EC slopes are modelled with an unstructured covariance (`us()`) across cultivars.

## Environmental covariates used

| Variable | Description (as named in the code) |
|---|---|
| `meanmaxtempveg` | Mean maximum temperature, vegetative phase |
| `meanmintempveg` | Mean minimum temperature, vegetative phase |
| `meantempveg` | Mean temperature, vegetative phase |
| `mmacumuladaveg` | Accumulated rainfall (mm), vegetative phase |
| `mmacumuladaflor` | Accumulated rainfall (mm), flowering phase |
| `heladasveg` | Frost events, vegetative phase |
| `evapoveg` | Evapotranspiration/evaporation, vegetative phase |

## Model structure

Each script fits models of the following form in ASReml-R:

```r
fixed  = Yield ~ 1 + EC
random = ~ environ:cultivar + environ:trial +
           str(~ vm(cultivar, Ginv) + EC:vm(cultivar, Ginv),
               vmodel = ~ us(k):vm(cultivar, Ginv))
```

where `k = (number of ECs) + 1`. Models are fitted with **1 to 5 ECs** and different EC combinations.

## Cross-validation schemes

| Folder | Scheme | What is predicted |
|---|---|---|
| `scripts/CV0/` | Leave-one-environment-out | Performance of cultivars in a **new, untested environment** |
| `scripts/CV1/` | 5-fold, 50 repetitions, folds by cultivar | Performance of **new, untested genotypes** in tested environments |
| `scripts/CV2/` | 5-fold, 50 repetitions, folds by cultivar × environment | Performance of **tested genotypes in tested environments** (incomplete field trials) |

## Repository contents

```
scripts/
├── CV0/
│   └── 1EC_CV0_CODEall.R              # 1-EC models, leave-one-environment-out
├── CV1/
│   ├── 1EC_CV1_CODEall_10122025.R     # 1 EC
│   ├── 2EC_CV1_CODEall_10122025.R     # 2 ECs
│   ├── 3EC_CV1_CODEall_10122025.R     # 3 ECs
│   └── 4EC_CV1_CODEall_10122025.R     # 4 ECs
└── CV2/
    ├── 1EC_CV2_CODEall_20122025.R     # 1 EC
    ├── 2EC_CV2_CODEall_23122025.R     # 2 ECs
    ├── RRM_3_EC_all_CV2.R             # 3 ECs
    ├── RRM_4EC_all_CV2_new_all.R      # 4 ECs
    └── RRM_5ECall_CV2_new.R           # 5 ECs
```

## Requirements

- R
- [ASReml-R](https://vsni.co.uk/software/asreml-r) (commercial software; a valid VSNi licence is required)

## Input data

The scripts expect the following R data objects, which are **not included** in this repository:

- `data_RRM_filtered_FA2_0508.rda` – phenotypic data (`data_RRM`) with yield BLUPs/weights, cultivar, environment, trial and EC columns (`RRM_5ECall_CV2_new.R` loads `data_RRM_centered.EC.rda` instead)
- `GINV.sparse.rda` – sparse inverse genomic relationship matrix (`GINV.sparse`)

## Usage

Scripts are written to be run interactively, block by block. Before running, set the working directory to the folder containing the input `.rda` files and check that the `load()` and `save()` file paths match your setup. Each block fits one EC combination and saves the resulting models, predictions and held-out data to an `.rda` file.

## Citation

If you use this code, please cite the paper above.

## Contact

<!-- Add corresponding author / contact details -->
