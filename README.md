[![R Version](https://img.shields.io/badge/R-%3E%3D4.0-blue)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Scripts for data analysis and plots in  
**Souley, M.N.S. et al.**  
*Physico‑chemical characterization of the Niger river in Niamey and its impacts on Mormyridae biodiversity*  
*Environmental Research Communications* (forthcoming)

## Repository structure

```
niger-mormyridae-analysis/
├── .gitignore
├── LICENSE
├── README.md
├── CITATION
├── pre-commit-config.yaml
├── scripts/
│   ├── correlation_matrix_corrplot.R
│   ├── env_vs_diversity_correlation_tables.R
│   └── species_env_clustering_heatmap_permanova.R
├── data/
│   └── README.md
└── outputs/
    └── .gitkeep

```

## Dependencies

Install required R packages:

```r
install.packages(c(
  "tidyverse", "Hmisc", "vegan", "cluster", 
  "pheatmap", "factoextra", "corrplot"
))
```

```bash
Rscript scripts/correlation_matrix_corrplot.R
Rscript scripts/env_vs_diversity_correlation_tables.R
Rscript scripts/species_env_clustering_heatmap_permanova.R
```

## Citations

If you find the codes useful in your research, please consider citing the following paper to support my work. Thank you for your support.
Souley, M.N.S., et al. (forthcoming). Physico‑chemical characterization of the Niger river in Niamey and its impacts on Mormyridae biodiversity. Environmental Research Communications.
Code repository: https://github.com/yourusername/niger-mormyridae-analysis

## 📄 License

The repo is free and open source, licensed under the MIT License.

## Acknowledgments

We gratefully acknowledge the support of the following:
-  Direction de la Pêche et de l'Aquaculture (DPA).
-   Fishermen and fishe momgers of the all data collection stations.
