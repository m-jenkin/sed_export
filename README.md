**Author**: Matt Jenkin, University of Lausanne, Switzerland

**Date**: Feb 2024

R code used to perform main analysis for the article: Subglacial sediment export from an Alpine glacier DOI: [DOI](DOI)

-   Using data shared on [Zenodo](Zenodo) repository.
-   `renv` used for package management - `renv.lock`, `.RProfile` and `export.Rproj` provided.
-   `run.R` loads data and calls custom functions for data analysis.

```         
.
├── data
│   ├── bed.tif
│   ├── export.csv
│   ├── hydrographs.csv
│   ├── meteo_daily.csv
│   ├── glacier_outline_2021.gpkg
│   └── surface.tif
├── export.Rproj
├── functions
│   ├── entropy_func.R
│   ├── export_by_SLA_func.R
│   ├── hysteresis_index_func.R
│   └── shreve_func.R
├── renv.lock
└── run.R
```

CC BY
