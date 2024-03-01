Author: Matt Jenkin, University of Lausanne, Switzerland ([mjenkin\@unil.ch](mailto:mjenkin@unil.ch))

Date: Feb 2024

Code and data used for the main analysis of glacier sediment export datasets in the unpublished article: [Subglacial sediment export from an Alpine glacier](---). The main export datasets are archived on [Zenodo](---). 

For a quick glance, see `run.html` for the code and rendered output. Written in R mainly using the `tidyverse` package collection and `sf`, `terra` and `whitebox` for geospatial data. A `.Rproj` and a [`renv`](https://rstudio.github.io/renv/articles/renv.html) lockfile are provided for package version management. The code mainly generates statistical summaries and exploratory graphs, calculates a metric describing daily water-sediment hysteresis, and then performs analyses relating sediment export to snow cover.

------------------------------------------------------------------------

```         
.
└── sed_export
    ├── README.md
    ├── .Rprofile
    ├── data
    │   ├── bed.tif
    │   ├── export.csv
    │   ├── glacier_outline_2021.gpkg
    │   ├── hydrographs.csv
    │   ├── meteo_daily.csv
    │   └── surface.tif
    ├── export.Rproj
    ├── functions
    │   ├── entropy_func.R
    │   ├── export_by_SLA_func.R
    │   ├── hysteresis_index_func.R
    │   └── shreve_func.R
    ├── renv
    │   ├── activate.R
    │   ├── library
    │   └── staging
    ├── renv.lock
    └── run.R
```

CC BY
