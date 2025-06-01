# cellAnnotator

[![R-CMD-check](https://github.com/JunhuiLi1017/cellAnnotator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JunhuiLi1017/cellAnnotator/actions/workflows/R-CMD-check.yaml)
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Overview

cellAnnotator is an R package designed for cell type annotation in single-cell RNA sequencing (scRNA-seq) data using Support Vector Machine (SVM) classification. This package enables researchers to leverage known cell type reference datasets to accurately classify cells in query datasets.

## Features

- Comprehensive workflow for cell type annotation
- Support Vector Machine (SVM) based classification
- Automated gene filtering and feature selection
- Built-in visualization tools for result interpretation
- Model persistence for saving and loading trained classifiers
- Support for both reference and query datasets

## Installation

You can install the package from GitHub using the `devtools` package:

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("JunhuiLi1017/cellAnnotator")
```

## Quick Start

```r
library(cellAnnotator)

# Load example data
data("reference_meta")
data("reference_expr")
data("query_meta")
data("query_expr")

# Split reference data
alldataset <- splitTrainTest(reference_meta, reference_expr,
                           groupBy = "cellType",
                           sampleID = "cell",
                           splitParameter = "ratio")

# Normalize data
trainNorExp <- normalizeSCExp(alldataset$trainExplanatoryData, dThresh = 0.25)

# Filter genes
filterGene <- filterSCGene(trainNorExp, mu = 2, alpha1 = 0.1, alpha2 = 0.01, threshold = 0.25)

# Select feature genes
uniqueFeatureGene <- findFeatureGene(alldataset$trainResponseData$cellType,
                                   trainNorExp,
                                   filteredGene = filterGene,
                                   topX = 20,
                                   reverse = TRUE)

# Build and evaluate classifier
classifier <- buildSVMclassifier(t(as.matrix(trainNorExp[uniqueFeatureGene,])),
                               as.factor(alldataset$trainResponseData$cellType))
```

## Detailed Documentation

For detailed usage instructions and examples, please refer to the package vignette:

```r
vignette("cellAnnotator")
```

## Workflow

The package provides a complete workflow for cell type annotation:

1. Data preparation and loading
2. Reference dataset splitting
3. Expression data normalization
4. Gene filtering
5. Feature gene selection
6. SVM classifier building
7. Model evaluation
8. Visualization and interpretation
9. Model saving and loading

## Citation

If you use cellAnnotator in your research, please cite:

```
@software{cellAnnotator,
  author = {Junhui Li},
  title = {cellAnnotator: Cell Type Annotation Using Support Vector Machine},
  year = {2024},
  url = {https://github.com/JunhuiLi1017/cellAnnotator}
}
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the GPL-3 License - see the [LICENSE](LICENSE) file for details.

## Contact

For questions and feedback, please open an issue on GitHub or contact the maintainer. 