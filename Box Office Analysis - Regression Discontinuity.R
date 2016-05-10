###-----Part 1-----###
#a) regression: log(subsequentEarnings)~releaseRank
DB <- read.csv("rockeyDB.csv")
DBwithout0 <- DB[DB$subsequentEarnings!=0,]
regression1 <- lm(log(subsequentEarnings)~releaseRank, data = DBwithout0)
summary(regression1)

###-----Part 2-----###
#a) empty data frame
finalDB <- matrix(data = NA, nrow = length(unique(DB$releaseDate)), ncol = 3)
finalDB <- data.frame(finalDB)
colnames(finalDB) <- c("date", "firstWeekBoxOfficeDiff", "laterLogBoxOfficeDiff")
#b)
#populate date
finalDB$date <- unique(DB$releaseDate)
#for loop
for (i in 1:nrow(finalDB)){
    #subset based on date
    date <- finalDB[i,1]
    DBforDate <- DB[DB$releaseDate==date, ]
    #populate first week box office difference
    finalDB[i,2] <- log(DBforDate[DBforDate$releaseRank==1,3]) - log(DBforDate[DBforDate$releaseRank==2,3])
    #populate later log box office difference
    finalDB[i,3] <- log(DBforDate[DBforDate$releaseRank==1,4]) - log(DBforDate[DBforDate$releaseRank==2,4])
}
summary(finalDB)
#remove Inf, -Inf and NA
finalDB <- subset(finalDB,firstWeekBoxOfficeDiff!= Inf &
                      firstWeekBoxOfficeDiff!= -Inf &
                      laterLogBoxOfficeDiff!= Inf &
                      laterLogBoxOfficeDiff!= -Inf)
finalDB <- finalDB[complete.cases(finalDB), ]


###-----Part 3-----###
#b) subset where first and second place were close
close <- subset(finalDB, firstWeekBoxOfficeDiff < 0.1)
#c) 
#find treatment group
DB[DB$releaseDate=="4/4/2008" & DB$releaseRank==1,]
#find control group
DB[DB$releaseDate=="4/4/2008" & DB$releaseRank==2,]
#d) regression discontinuity
t.test(close$laterLogBoxOfficeDiff, mu=0)
