# Egocentrism in Strategic Decisions

Replication code and data. Participants completed a Qualtrics survey with the Pods-and-Dots game (`interface/`) embedded in an iframe, so the game data exists in two places: the game's own database, and a copy that the game passed back to Qualtrics. `analysis/01_build.R` merges them.

The manuscript is in preparation and is not included.

```
data/
  qualtrics_data.csv       survey responses + game log (participant IDs replaced with random IDs)
  fly_app_data.sqlite      database written by the game
analysis/                  run from the repo root, in order
  01_build.R               -> data/built/round_level_data.csv, participant_level_data.csv
  02_tables.R              LaTeX tables
  03_figures.R             Figures 2-6
  04_text_stats.R          prints statistics quoted in the text
  fig1_interface.py        Figure 1
tables_and_charts/
  main/                    main text and Appendices C-E
  pass_comps/              Appendix A (passed comprehension checks on first try)
  exclude_early/           Appendix B (excludes rounds 1-2)
  adv_only/, coop_only/    Appendix D
interface/                 the game (Flask)
```

## Requirements

Tested with these versions:

- **R 4.5.2**, with packages tidyverse 2.0.0, fixest 0.13.2, DBI 1.2.3, RSQLite 2.4.5, janitor 2.2.1, jsonlite 2.0.0, gridExtra 2.3, patchwork 1.3.2
- **Python 3.14**, with matplotlib 3.10.8 (Figure 1) and Flask 3.1.2 (the game; `interface/requirements.txt`)

```
install.packages(c("tidyverse", "fixest", "DBI", "RSQLite", "janitor", "jsonlite", "gridExtra", "patchwork"))
pip install matplotlib -r interface/requirements.txt
```

## Reproducing the results

```
Rscript analysis/01_build.R
Rscript analysis/02_tables.R
Rscript analysis/03_figures.R
Rscript analysis/04_text_stats.R
python  analysis/fig1_interface.py
```

## The experimental interface (game)

To play locally, double-click `interface/run_interface_locally.bat` (Windows) or `interface/run_interface_locally.command` (Mac). It starts the game and opens it in your browser inside a fixed-size frame, as in the Qualtrics survey; close the terminal window to stop. Responses are saved to `interface/data/`. Rounds are drawn from `interface/stimuli/`, which `interface/generate_stimuli.R` generates. To run the game in a survey, host `interface/` on any web server with persistent storage and embed its URL in an iframe.
