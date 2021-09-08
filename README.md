# Rural economy
> Exploring economic dynamics in rural areas of the United States.

Interactive geography dashboard: [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/antonbabkin/rurec.git/binder?urlpath=voila/render/nbs/geography.ipynb)

## Research team

- Anton Babkin (UConn)
- Richard A. Dunn (UConn)
- Brent Hueth (UW-Madison)
- Thomas Flory (UW-Madison)
- Griffin O'Neill (UConn)

## Installation

1. Clone this git project.
2. Create and activate conda environment from `environment.yml` file.
3. Run initialization cells from the [UI notebook](nbs/ui.ipynb).


# Project structure
 
- InfoGroup preparation: validate, clean, convert
- ERS codes preparation: download, combine, format
  - Other geo codes will be added as well
- Rurality: merge ERS codes into InfoGroup
- Dynamics: summary statistics by geography
