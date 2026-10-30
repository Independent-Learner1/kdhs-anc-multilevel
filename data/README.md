# Data

DHS microdata cannot be redistributed. Register at <https://dhsprogram.com/data/>, request the
**Women's Individual Recode (IR)** file for your survey (for the 2022 KDHS: `KEIR8CFL.SAV`) and
save it in `data/raw/`. The folder is git-ignored, so the file will never be pushed to GitHub.
Then set `paths.data_file` in `config/config.yml` (or the `KDHS_DATA` environment variable).
