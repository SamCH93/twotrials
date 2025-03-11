# Combined P-value Functions for Compatible Effect Estimation and Hypothesis Testing in Drug Regulation

This repository contains 

1. `./package` The R package **twotrials** to conduct combined *p*-value
function inference based on the results from two trials.

2. `./paper` Code and data to reproduce result from the preprint: *Pawel, S.,
   Roos, M., Held. L. (2025). Combined P-value Functions for Compatible Effect
   Estimation and Hypothesis Testing in Drug Regulation*

To cite our work, use the following BibTeX reference

```BibTeX
@article{PawelRoosHeld2025,
  year = {2025},
  author = {Samuel Pawel and Ma\l{}gorzata Roos and Leonhard Held},
  title = {Combined \textit{P}-value Functions for Compatible Effect Estimation and Hypothesis Testing in Drug Regulation}
}
```

## Reproducing the paper with Docker

Make sure to have Docker and Make installed, then run `make docker` from the
root directory of this git repository. This will install all necessary
dependencies. RStudio Server can then be opened from a browser
(<http://localhost:8787>), and the R scripts in `./paper`, e.g., `est2TR.R` which
contains all code for the results from the paper, can be rerun (make sure to set
the working directory to `./paper` when running R interactively).
