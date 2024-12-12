# Repository Structure

This GitHub repository is organized into the three following folders, containing all the relevant files except for this ReadMe.

## Memos

As a part of this project, memos are created to relay the progress made, summarize its intent, and describe the conclusions drawn.

- `Boyko_Alex_data_memo.html`: A report detailing the data source chosen for this project. 
- `Boyko_Alex_data_memo.html`: The Quarto document used to create the above report.

## Scripts

All R code used to create this project, outside of the app files, are stored in this folder.

- `00_Data_Cleaning.R`: The process in which data was loaded, cleaned, and saved.
- `01_Graphic_Creation.R`: Demos of the graphics used in the dashboard without interactivity.

## The Low Down

The folder serves as the home base for all the files needed to run the Shiny app with the same name.

- `app.R`: The script used to format, display, and publish the Shiny dashboard.

### Data

All data derives from the API utilized within the `cfbfastR` package. Once data has been loaded, cleaned, and reconfigured, it is stored in this folder.

- `game_stats.rda`: Contains the box score information for all games played for every FBS player this season.
- `percentile_data.rda`: Contains the percentile rankings in nine key offensive statistics for all qualified FBS players.
- `player_data.rda`: Contains the biographic and statistical information for all qualified FBS players, as well as the visual and categorical information needed to create visualizations.
- `team_data.rda`: Contains the cumulative statistical information for every FBS team, as well as the visual and categorical information needed to create visualizations.
