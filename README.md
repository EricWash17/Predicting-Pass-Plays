# Predicting-Pass-Plays
Knowing when an opponent will run or pass can dramatically alter how a team plays defense. Using ESPN API play-by-play college football data collected by Reddit users, I implemented a XGBoost classification model to predict passing plays. I trained and validated the model using the 2014-2016 seasons, and tested it on 2017 season data. The model achieved 70% accuracy. 

# Model variables include:

## Game variables
- Downs (1-4)
- Distance to 1st down
- Yardline (Yards to goaline)
- Time in seconds (Start = 0, End = 3600)
- Point difference ( <0 = losing, >0 = winning)
- Quarter (1-5, with 5 indicating OT)
- Goaline indicator (within opponent's 10 yardline)
- Fg Range indicator (Within opponent's 30 yardline)

Interactions
- Down * distance
- Score * time

## Calculated variables 
(All calculated data was lagged, meaning the variables were based on plays prior to the given prediction) 

Within Game Calculations
- Moving average of last 5 pass and run plays, repectively. 
- Consecutive passes and runs
- Percent of total plays that were passes in the given game

Season Calculations
- Pass probability (Historical probability of passing on given down and distance that season)
- Season pass percentage (Pass plays as percent of total plays that season)

# Models
Although I tried stacking several types of models (Random Forest, Neural Net, Lasso Logistic, Naive Bayes), the XGBoost alone performed the best. 

# Missing information that could improve this model include:
- QB scrambles. In the current data set, there is no way to differentiate between called QB runs and QB scrambles. In other words, a team could call a pass play that, due to QB improvisation, turns into a run play.  
- Formation information.

# Code 
Run the code in the following order:
- Prepare Data
- XGB Tune (optional)
- FinalModel
- ly (optional)
- Other Models
