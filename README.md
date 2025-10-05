<!-- TODO: adjust this file along with adding new code files -->
<!-- TODO 2: once you set up renv, change the structure of setup section  -->

# Overview

This repository contains the code in R used for the master’s thesis introducing the random ilr data augmentation techniques. This project organizes R scripts, data sets, and outputs for a thesis project. Scripts in `R/` are typically numbered to indicate execution order (e.g., `R/00_data_prep` should be executed as first). Data is expected under `data/` and results are saved under `results/`.

## Project Structure

```
.
├── R/                     # R scripts (e.g., 00_data_prep, …)
├── data/
│   ├── raw/               # Unmodified source data (not committed if large/sensitive)
│   └── preproc/           # Intermediate artifacts from preprocessing
├── outputs/
│   ├── figures/           # Plots and figures
│   └── tables/            # Result tables / model summaries
├── docs/                  # Notes, manuscript, or exported reports
├── .gitignore
└── README.md
```

Adjust folders as needed for your workflow. Ensure `.gitignore` excludes large or sensitive files in `data/` and generated artifacts in `outputs/`.

## Getting Started

### Setup

1. Clone the repository.
2. Open the project in your preffered IDE.
3. Open the `.RProj` file.
4. Run `renv::restore()`. This will restore the environment needed to reproduce the analysis done in this project. 


### Running the Pipeline

Run scripts in order. For example, to execute data preparation:

```r
source("R/00_data_prep")   # or source("R/00_data_prep.R") if you add the extension
```

Add additional numbered scripts (e.g., `R/01_analysis.R`, `R/02_models.R`) and run sequentially.

## Data

- Place original datasets in `data/raw/` (not tracked if large/sensitive).
- Write intermediate, cleaned data to `data/interim/` from preprocessing scripts.
- Do not commit sensitive or proprietary data. Update `.gitignore` accordingly.

## Reproducibility

- Prefer relative paths via `here::here()` or similar.
- Pin package versions with `renv` (recommended) and include `renv.lock` in version control.
- Set seeds where randomness is used (e.g., `set.seed(123)` in scripts).

## Development Notes

- Keep scripts small and focused; number them to convey order.
- Save figures to `outputs/figures/` and tables to `outputs/tables/` from scripts.
- Document assumptions and decisions in `docs/` or within the scripts.

## License

No license selected yet. Consider adding one (e.g., MIT, CC BY-NC). Create a `LICENSE` file when ready.

## Citation

If you publish the thesis or related paper, add citation details here. Optionally include a `CITATION.cff` file for GitHub’s citation support.

## Contact

Author: [Your Name]
Contact: [your.email@example.com]

