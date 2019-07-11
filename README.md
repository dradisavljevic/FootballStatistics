# FootballStatistics

Statistical analysis done in R on the data gathered through [SrbijaSportScraper](https://github.com/dradisavljevic/SrbijaSportScraper). Data is from Serbian and Montenegrin football leagues.

## Motivation

Trying out different statistical analysis on the gathered data as well as trying to find any pattern that can lead to prediction system in the future.

## Data overview

serbianData.csv - contains information from the first 4 levels of the Serbian football league system from the 2006/2007 season to the 2018/2019 season. The .csv file is located inside the SRB folder.

montenegroData.csv - contains information from the first 2 levels of Montenegro football league system from the 2013/2014 season to the 2018/2019 season, as well as the third level leagues from the 2016/2017 to the 2018/2019 season. .csv file is located inside the  MNE folder.

## Prerequisites

* R - latest version

## Project overview

* statistics.R - Main script. Within this one all the other commands are executed.
* prepare_data.R - Script holding functions that prepare and cleanse data for the analysis.
* descriptive.R - Script containing functions that get descriptive statistics on the data.
* match_result_stat.R - Functions that convert the data to per match information and get statistics on it.
* opening_weekend.R - Script within which data of the opening weekends is extracted and analysis are performed on it.

## ToDo

- [ ] Present results in R markdown generated static webpage.
- [ ] Perform couple of Hypothesis tests.
