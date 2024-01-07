# Are point in tennis IID? Evidence from a dynamic binary panel data model

## Objective

The objective is to evaluate whether points in tennis are independent and identically distributed. Verifying the hypothesis of independence and identical distribution means proving that a player faces each point in the same way throughout the match, without being influenced by possible errors, the match's score, or the opponent's skill. The probability of scoring a point during the players' serves is assessed in each match.
To address the issue, Magnus and Klaassen proposed a dynamic model for binary response panel data. The implemented algorithm is Feasible Generalized Least Squares (FGLS). In addition to this, other statistical methods were used for parameter estimation, such as the use of instrumental variables.
The importance of the point was calculated using Markov Chains.

The algorithm is applied to all matches played in the year 2019 in the four Grand Slam tennis tournaments, specifically:
- Australian Open 2019
- Roland Garros 2019
- US Open 2019
- Wimbledon 2019

## Technical Details

The algorithm is written in R with version 4.1.3 (2022-03-10).

The first stable released version is V1.

## Documentation Files

Documentation files supporting the Readme are located in the 'documentation' folder and include the following:

- *MAGNUS, KLAASSEN - Are Points in Tennis Independent and Identically Distributed.pdf*: a guide article for implementing the model and calculating parameter estimates.

## Data

For each tournament, three files are available, found in the data_input repository (one repository for each tournament):

1) 2019_*tournament*_matches.csv
2) 2019_*tournament*_points.csv
3) ranking_*data*.csv

These three files, once appropriately combined, will form the final dataframe containing, in panel format, every point played in the i-th tournament.

# Development

### Data Preparation

First, run the .R files related to each tournament from the script folder, specifically:

- 11_AO_2019.R
- 21_RG_2019.R
- 31_US_2019.R
- 41_W_2019.R

These four files are related to the data preparation phase. They load a set of functions useful for calculating the importance of each point within the match for each of the serving players (A|B) in each match (Markov Chain).

Additionally, two components are calculated:

- Fixed component: represented by the relative quality of one player compared to the other and the absolute quality of the match. These two variables remain constant throughout the i-th match but change between different matches. A logarithmic transformation of each player's respective ranking is used to calculate it.

- Dynamic component: varies from point to point and includes three main variables. The first is the outcome of the previous point (dichotomous and calculated from the response y_t), the second is a dichotomous variable indicating whether we are in the first point of the game or not, and the third is the importance of the individual point within the match.

Intermediate files contain the data on which the final fgls model needs to be adapted. These files are appropriately saved in the data_temp folder.

### Modeling

Once the data is in a format suitable for analysis, scripts will be launched to adapt the final model:

- 12_AO_fgls.R
- 22_RG_fgls.R
- 32_US_fgls.R
- 42_W_fgls.R

In each script, for each tournament, prepared data is loaded (for player A and player B, respectively).

The fgls model is adapted, aiming for convergence of parameters related to fixed effects and player and match variables, iterating multiple times.

Once parameter convergence is achieved, the final model is calculated on the data suitably transformed following the logic of the article, with the victory or defeat of the individual point as the response variable. The final model was then cleaned of possible interactions between variables using a stepwise forward procedure.

In the data_output repository, for each tournament, two files are saved:
- df_*tournamentname*_2019_final.rds: contains the final transformed df to which the linear model has been adapted.
- mod_*tournamentname*_2019.rds: contains the summary of the final model with correct and consistent parameter estimates.