rm(list=ls())
gc()

setwd("C:/Users/Eric/Desktop/NCSTATE/Projects/PlayCall/data")

library(data.table)
library(dplyr)
library(stringr)
library(Hmisc)
library(chron)
library(dummies)

### Loading Data
yr = as.character(seq(from = 2014, to = 2017, by = 1))
wk = as.character(seq(from = 1, to = 15, by = 1))

plays = data.table()
for (j in yr){  
  for (i in wk) {
    read = paste(j,"/","PBP - ",j," - Week ",i,".csv", sep = "")
    week = fread(read)
    plays = rbind(plays, week, fill = TRUE)
  }
}
#Eliminating unneeded variable
plays[,wallclock := NULL]

# Reading in csv that had different name
week = fread('2017/PBP - 2017 - Week 1 (2).csv')
plays = rbind(plays, week)
rm(list = c('week','wk','i','read','yr','j'))

# Deleting Bowl Games
bowls = c('400953325','400953323','400953324','400953322')
plays = plays[ !(plays$gameId %in% bowls)]

# Changing duplicate week 1s
wk0 = c('400869090','400935282','400938887')
plays[, week := ifelse(gameId %in% wk0,0,week)]

rm(list = c('bowls','wk0'))

# Inspected scores to detect and correct data errors
plays$homeScore[plays$homeScore == 1] <- 0
plays$awayScore[plays$awayScore == 1] <- 0
# Making negative scores equal to the score in the previous row
plays[ , homeScore := ifelse(homeScore < 0, shift(homeScore),homeScore)]
plays[ , awayScore := ifelse(awayScore < 0, shift(awayScore),awayScore)]

exam = unique(plays[, c('gameId','offenseTeam','year')])
see = setDT(exam)[,.(.N), by = c('offenseTeam','year')]
exam = setDT(see)[N >= 9,]
fbs = unique(exam$offenseTeam) # Hawaii spelled differently in 2015

plays = plays[awayTeam %in% fbs & homeTeam %in% fbs,] # 530458

rm(list = c('fbs','exam','see'))

# Making name consistent
plays[, awayTeam := ifelse(awayTeam == "Hawaii","Hawai'i" ,awayTeam)]
plays[, homeTeam := ifelse(homeTeam == "Hawaii","Hawai'i" ,homeTeam)]
plays[, offenseTeam := ifelse(offenseTeam == "Hawaii","Hawai'i" ,offenseTeam)]
plays[, defenseTeam := ifelse(defenseTeam == "Hawaii","Hawai'i" ,defenseTeam)]


### Keeping only Run and Pass plays
p = c("pass","sack","passing","interception","sacked","scramble")
r = c("rush","rushing","run")
o = c("fumble","safety","penalty")
t = c("fumble","interception","fumble")

# creating objects for play types that have the corresponding words listed above
pass = levels(as.factor(plays$type))[grepl(paste(capitalize(p), collapse = "|"),
                                           levels(as.factor(plays$type)))]
rush = levels(as.factor(plays$type))[grepl(paste(capitalize(r), collapse = "|"),
                                           levels(as.factor(plays$type)))]
other = levels(as.factor(plays$type))[grepl(paste(capitalize(o), collapse = "|"),
                                            levels(as.factor(plays$type)))]
to = levels(as.factor(plays$type))[grepl(paste(capitalize(t), collapse = "|"),
                                         levels(as.factor(plays$type)))]
# Keeping plays that were a pass or run
plays = plays[, keep := ifelse(type %in% pass,TRUE, 
                               ifelse(type %in% rush,TRUE,
                                      ifelse(type %in% other & 
                                               (grepl(paste(p, collapse = "|"),description) | 
                                                  grepl(paste(r, collapse = "|"),description)), TRUE, FALSE)))]
plays = plays[keep == TRUE,]

# Fixing data entry errors
plays = plays[ !(plays$distance == 65 & plays$type == 'Pass Reception')] #Typo
plays = plays[ !(plays$down == 0)] # Dropping
plays[ , distance := ifelse(distance < 0, shift(distance),distance)]

#Cleaning typo and turning TOs to 0 yards gained
plays[, yardsGained := ifelse(yardsGained  == 10907 | type %in% to,0,yardsGained) ]

# Flagging plays that have a pass term in the desciption
plays[, pass := ifelse(grepl(paste(p, collapse = "|"),description),1,0)]
plays[, keep := NULL]
rm(list = c('o','p','r','pass','other','rush','t','to'))
table(plays$pass)

### Clock
# Turning clock to seconds (0 (Start) - 3600 (End))
ch = strptime(plays$clock, format = "%M:%S") 
ti = times(format(ch, "%H:%M:%S"))
secs = (as.numeric(as.POSIXct(strptime(ti, format = "%H:%M:%S"))) - 
          as.numeric(as.POSIXct(strptime("0", format = "%S"))))

plays[ , clock := NULL]
plays = cbind(plays,secs)
plays[ , clock := 900 - secs + 900* pmin(3,quarter-1)] # maxtime = 3600 seconds
plays[ , quarter := ifelse(quarter > 4,5,quarter)] # Making all OTs = 5th qtr

rm(list = c('ch','ti','secs'))

# Flagging plays with 3 minutes or less in 2nd and 4th quarters
plays[ , under3 := ifelse((clock > 1620 & clock < 1800) | (clock > 3420 & clock < 3600),1,0)]


## Cleaning and subseting Data
plays = plays[,c("gameId","week","year","homeTeam", "awayTeam","offenseTeam","defenseTeam","quarter", "clock", 
                 "homeScore","awayScore", "down","distance","yardLine", "yardsGained", "isScoringPlay",
                 "description","type","driveIndex", "playIndex", "under3", "pass")]

### Splitting to make only one team on offense per game, will rbind later

plays = plays[order(gameId,driveIndex,playIndex),]
plays[, run := 1-pass]
plays[,home := ifelse(homeTeam == offenseTeam,1,0)]
home = plays[home == 1,]
home[,score := homeScore - awayScore] # Pt difference
home = home[order(gameId,clock),-c("homeScore","awayScore")]

# Flagging first obs for each game
home[,first := ifelse(gameId == lag(gameId),FALSE,TRUE)]
home[1, first := TRUE]
# Know score on next play
home[, score := ifelse(first == TRUE, 0, lag(score))]
# Making yardline uniform for home and away --> yards until goal
home[, yardLine := 100 - yardLine]

away = plays[home == 0,]
away = away[,score := awayScore - homeScore]
away = away[order(gameId,clock),-c("homeScore","awayScore")]
away[,first := ifelse(gameId == lag(gameId),FALSE,TRUE)]
away[1, first := TRUE]
away[, score := ifelse(first == TRUE, 0, lag(score))]

# Computing consecutive passes and runs prior to play
cons = function(data){
  data[, cons_pass := lag(sequence(rle(as.character(pass))$lengths)*pass), by = gameId]
  data[first == 1,cons_pass := 0 ]
  data[, cons_run := lag(sequence(rle(as.character(run))$lengths)*run), by = gameId]
  data[first == 1,cons_run := 0 ]
}

home = cons(home)
away = cons(away)

# Calculating moving average of yards gained for last 5 plays by type (pass or run)
get.mav <- function(x,n=5){
  require(zoo)
  if(is.na(x[1])) x[1] <- mean(x,na.rm=TRUE)
  x <- na.locf(x,na.rm=FALSE)
  if(length(x)<n) return(x)
  c(x[1:(n-1)],rollapply(x,width=n,mean,align="right"))  
}

ma = function(data){
  p = data[pass==1,] %>%
    group_by(gameId) %>% 
    mutate(pass_ma = get.mav(yardsGained, n = 5))
  
  r = data[pass==0,] %>%
    group_by(gameId) %>% 
    mutate(rush_ma = get.mav(yardsGained, n = 5))
  
  pr = rbind(p,r)
  pr = data.table(pr)
  pr = pr[order(gameId,clock),]
  
  new = cbind(data,pr[,c('pass_ma','rush_ma')])
  new = data.table(new)
  setkey(new,gameId)
  new[, ptot := cumsum(pass), by = gameId] # cumulative sum of pass plays by game
  new[, rtot := cumsum(run), by = gameId] # cumulative sum of run plays by game
  new[, pass_ma := ifelse(ptot < 6, mean(p$yardsGained),lag(pass_ma))] # Using global avg until 6th pass
  new[, rush_ma := ifelse(rtot < 6, mean(r$yardsGained),lag(rush_ma))] # Using global avg until 6th rush
  new[, early_p := ifelse(ptot < 6,1,0)] # Flagging plays where global avg used
  new[, early_r := ifelse(rtot < 6,1,0)] # Flagging plays where global avg used
  new[, plays := cumsum(pmax(pass,1)), by = gameId] # cumulative sum of plays in game 
  new[, p_perc := ptot/plays, by = gameId] # Percent pass plays up to each play by game
  new[, mx := max(plays), by = gameId] # Flagging last play per game per team
  new[, imx := ifelse(mx == plays,1,0)]
  #### Enter binary for 1st down, 2nd short, etc
  new$pass_ma <- na.locf(new$pass_ma,na.rm=FALSE) # backfilling NAs
  new$rush_ma <- na.locf(new$rush_ma,na.rm=FALSE)
  return(new)
}

home = ma(home)
away = ma(away)

### Combing to make final dataset
plays = rbind(home,away)
rm(list = c('home','away'))

plays[, dwn1 := ifelse(down == 1,TRUE,FALSE)]
plays[, dwn2L := ifelse(down == 2 & distance >= 5,TRUE,FALSE)]
plays[, dwn2S := ifelse(down == 2 & distance < 5,TRUE,FALSE)]
plays[, dwn3L := ifelse(down == 3 & distance >= 5,TRUE,FALSE)]
plays[, dwn3S := ifelse(down == 3 & distance < 5,TRUE,FALSE)]
plays[, dwn4 := ifelse(down == 4,TRUE,FALSE)]


####################################################################
# x = subset info, y name of new column name
specific = function(x,y){
  new = plays[x==TRUE, c('gameId','offenseTeam','pass','week','year','driveIndex','playIndex')]
  new = new[order(offenseTeam,year,week,driveIndex,playIndex)]
  new[,ptot := cumsum(pass), by = gameId]
  new[,plays := cumsum(pmax(pass,1)), by = gameId]
  new[, mx := max(plays), by = c('gameId','offenseTeam')] # Flagging last play per game per team
  new[, imx := ifelse(mx == plays,1,0)]
  new = new[imx == 1,]
  new[,p_prob := cumsum(ptot)/cumsum(mx), by = c('offenseTeam','year')]
  new[,p_prob := lag(p_prob)]
  new[, frst := ifelse(offenseTeam != lag(offenseTeam),TRUE,FALSE)]
  new[ , type := as.character(y)]
  new = new[,-c("pass","ptot","plays","mx","imx",'driveIndex','playIndex')]
  return(new)
}

dwn1 = specific(plays$dwn1,'dwn1')
dwn2L = specific(plays$dwn2L,'dwn2L')
dwn2S = specific(plays$dwn2S,'dwn2S')
dwn3L = specific(plays$dwn3L,'dwn3L')
dwn3S = specific(plays$dwn3S,'dwn3S')
dwn4 = specific(plays$dwn4,'dwn4')

type_prob = rbind(dwn1,dwn2L,dwn2S,dwn3L,dwn3S,dwn4)
type_prob[, frst := ifelse(is.na(frst) == TRUE,TRUE,frst)]

rm(list = c('dwn1','dwn2L','dwn2S','dwn3L','dwn3S','dwn4'))
plays[, type := ifelse(dwn1 == TRUE,'dwn1',
                       ifelse(dwn2L == TRUE,'dwn2L',
                              ifelse(dwn2S == TRUE,'dwn2S',
                                     ifelse(dwn3L == TRUE,'dwn3L',
                                            ifelse(dwn3S == TRUE,'dwn3S','dwn4')))))]

# Reading in tendency from last game of last yr for first game of this yr
ly = fread('ly2.csv')
ly = data.table(ly)
ly[, year := 2015]
type_prob = left_join(type_prob,ly,by = c('offenseTeam','type','year'))
type_prob = data.table(type_prob)
type_prob[,p_prob := ifelse(is.na(p_prob) | frst == TRUE,lyp_prob,p_prob)]

plays[, t := TRUE]
al = specific(plays$t,'all')
colnames(al)[5] = 'pt_perc'
plays[, t := NULL]

type_prob = left_join(type_prob,al[,c('offenseTeam','year','pt_perc','week')],
                      by = c('offenseTeam','year','week'))
type_prob = data.table(type_prob)
type_prob[,pt_perc := ifelse(is.na(pt_perc) | frst == TRUE,lypt_perc,pt_perc)]

plays = left_join(plays,type_prob[,c("offenseTeam","week","year","type","p_prob","pt_perc",'frst')], 
                  by = c("offenseTeam","type","year","week"))

# Deleting rows where no previous data 
# i.e. team was not division 1 last yr or did not have 4th dwn attempt yet 
plays = plays[!is.na(plays[,'p_prob']), ] 
plays = data.table(plays)
#plays = plays[frst != TRUE, ] 

# Lagging 
plays[ , p_perc := ifelse(plays <= 5 ,pt_perc ,lag(p_perc))]

plays = plays[order(gameId,clock), -c('homeTeam','awayTeam','isScoringPlay','description','type',
                                      'driveIndex','playIndex','run','first','mx','imx','rtot')]

rm(list = c('ly','al','type_prob'))

# Don't want anything to have 0, because always a chance a team passes.
plays = plays[, p_prob := ifelse(p_prob == 0,.001,
                                 ifelse(p_prob == 1, .999,p_prob))]

plays = plays[, p_perc := ifelse(p_perc == 0,.001,
                                 ifelse(p_perc == 1, .999,p_perc))]

#####################################################################
# Loading S&P data
sp = fread('SP.csv')

# Fixing names that don't match
#test = data.table(unique(plays[,defenseTeam]))
#colnames(test)[1] <- "defenseTeam"
test = left_join(plays,sp,by = c("defenseTeam","year"))
new_DF <- test[is.na(test$Adj_Def_SP),]

# names that didn't match
abv = as.list(unique(new_DF$defenseTeam))

# names from S&P
nm = as.list(c("Florida State","Georgia Southern","Appalachian State",
               "Middle Tennessee","Mississippi State","Louisiana Tech",
               "California","UL-Lafayette","Massachusetts","New Mexico State",
               "Eastern Michigan","Connecticut","Coastal Carolina",
               "Western Kentucky","UL-Monroe","Virginia Tech",
               "Central Michigan","Ohio State","East Carolina",
               "Pittsburgh","Miami-OH","North Carolina","Miami-FL",
               "Western Michigan","Hawaii","Washington State",
               "Virginia","Florida Atlantic","Northern Illinois",
               "Central Florida","Florida International","South Florida"))

# Function to change name
loop = function(row,col, x,y){
  plays[row == x, col] <-y
  return(plays)
}

# Looping through function to match names with S&P
for (i in 1:length(abv)){
  plays = loop(row = plays$offenseTeam,col = "offenseTeam",x = abv[i], y = nm[i])
}

for (i in 1:length(abv)){
  plays = loop(row = plays$defenseTeam,col = "defenseTeam",x = abv[i], y = nm[i])
}

# Merging data with S&P data
plays = inner_join(plays,sp,by = c("defenseTeam","year"))
plays = data.table(plays)

min_wk = plays[, min(week), by = c('offenseTeam','year')]
plays[, min_wk := min(week), by = c('offenseTeam','year')]
plays[, new_coach := ifelse(week == min_wk & new_coach == 1, 1,0) ]

rm(list = c('abv','new_DF','nm','test','i','sp'))

################################################################################
# Splitting data
plays = plays[order(offenseTeam,year,week,clock),]
test = plays[,c("year","week","clock","down","distance","yardLine","pass","p_prob","new_coach",
                "pt_perc","p_perc",'plays',"cons_pass","cons_run","under3","score","home","rush_ma",
                "pass_ma","Adj_Rushing","Adj_Passing","Off_Ratings","early_p","early_r","quarter")]    

test = data.table(test)
test[, rz := ifelse(yardLine < 10,1,0)]
test[, timescore := score*clock]
test[ ,dwnyrds := ifelse(down == 2,distance*2,
                         ifelse(down==3,distance*3,
                                ifelse(down==4,distance*3,distance)))]
test[, fg_range := ifelse(yardLine < 30,1,0)]
test[, dist := ifelse(distance < 4,1,
                      ifelse(distance < 8,2,3))]


Train = test[year == 2014 | year ==2016,-c('year','week')]
Valid = test[year == 2015,-c('year','week') ]

New.Train = rbind(Train,Valid)
Test = test[year == 2017,-c('year','week')]
