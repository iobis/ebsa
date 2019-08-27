# ebsa

This repository hosts R code to generate shapefiles of biodiversity indices and other metrics based on OBIS data in support of the CBD EBSA process. The current version of the code uses a CSV export of the relevant OBIS fields, and exports shapefiles as well as map images. Core functionality can be found in `lib.R`, overall metrics are calculated in `script.R`, and metrics for data subsets are calculated in `subsets.R`.

![maps](metrics.png)