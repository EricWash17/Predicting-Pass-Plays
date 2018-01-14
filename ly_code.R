rm(list=ls())
gc()

setwd("C:/Users/Eric/Desktop/NCSTATE/Projects/PlayCall/data")

library(data.table)
library(dplyr)

ly = fread('2013/PBP - 2013 - Week 11.csv')
ly2 = fread('2013/PBP - 2013 - Week 12.csv')
ly3 = fread('2013/PBP - 2013 - Week 13.csv')

ly = rbind(ly,ly2,ly3)
rm(list = c('ly2','ly3'))

### Keeping only Run and Pass plays
p = c("pass","sack","passing","interception","sacked")
r = c("rush","rushing","run")
o = c("fumble","safety","penalty")
t = c("fumble","interception","fumble")

pass = levels(as.factor(ly$type))[grepl(paste(capitalize(p), collapse = "|"),
                                           levels(as.factor(ly$type)))]
rush = levels(as.factor(ly$type))[grepl(paste(capitalize(r), collapse = "|"),
                                           levels(as.factor(ly$type)))]
other = levels(as.factor(ly$type))[grepl(paste(capitalize(o), collapse = "|"),
                                            levels(as.factor(ly$type)))]
to = levels(as.factor(ly$type))[grepl(paste(capitalize(t), collapse = "|"),
                                         levels(as.factor(ly$type)))]
ly = ly[, keep := ifelse(type %in% pass,TRUE, 
                               ifelse(type %in% rush,TRUE,
                                      ifelse(type %in% other & 
                                               (grepl(paste(p, collapse = "|"),description) | 
                                                  grepl(paste(r, collapse = "|"),description)), TRUE, FALSE)))]
ly = ly[keep == TRUE,]

#Cleaning typo and turning TOs to 0 yards gained
ly[, yardsGained := ifelse(yardsGained  == 10907 | type %in% to,0,yardsGained) ]

# Flagging pass ly
ly[, pass := ifelse(grepl(paste(p, collapse = "|"),description),1,0)]
ly[, keep := NULL]
rm(list = c('o','p','r','pass','other','rush','t','to'))

ly = ly[order(gameId,driveIndex,playIndex),]
ly[, run := 1-pass]
ly[,home := ifelse(homeTeam == offenseTeam,1,0)]
home = ly[home == 1,]
away = ly[home == 0,]

ma = function(data){
  new = data.table(data)
  setkey(new,gameId)
  new[, ptot := cumsum(pass), by = gameId]
  new[, rtot := cumsum(run), by = gameId]
  new[, plays := cumsum(pmax(pass,1)), by = gameId]
  new[, mx := max(plays), by = gameId]
  new[, imx := ifelse(mx == plays,1,0)]
  new[, plays := NULL]
}

home = ma(home)
away = ma(away)
ly = rbind(home,away)

ly_try = ly[imx == 1, c('gameId','offenseTeam','ptot','rtot','mx','week')]
ly_try = ly_try[order(offenseTeam,week),]
ly_try = data.table(ly_try)
ly_try[ , pt_perc := cumsum(ptot)/cumsum(mx), by = offenseTeam]
ly_try[ , rt_perc := cumsum(rtot)/cumsum(mx), by = offenseTeam]

ly_try = ly_try[, c('offenseTeam','pt_perc','rt_perc')]
exam = ly_try[,mean(pt_perc),by = offenseTeam]
exam2 = ly_try[,mean(rt_perc),by = offenseTeam]
ly_try = inner_join(exam,exam2,by='offenseTeam')
colnames(ly_try) = c('offenseTeam','lypt_perc','lyrt_perc')
ly_try = data.table(ly_try)

ly_try[,offenseTeam := ifelse(offenseTeam == "Hawaii","Hawai'i" ,offenseTeam)]


write.csv(ly_try,"ly.csv")
