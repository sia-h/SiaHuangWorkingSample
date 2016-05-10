rm(list = ls())
setwd("D:/R_wd")

###-----1-----###
library(foreign)
filename = "Wegmans Survey 1"
yogurtlab = read.spss(paste(filename,".sav",sep=""),
                    to.data.frame=TRUE, use.value.labels=TRUE, trim_values=TRUE)
yogurt = read.spss(paste(filename,".sav",sep=""),
                      to.data.frame=TRUE, use.value.labels=FALSE, trim_values=TRUE)
summary(yogurt)
# Compare desired and actual population gender
popSex = c(0, 0.87, 0.13, 0)
genderTable = cbind(popSex, prop.table(table(yogurt$Question33Areyou)))
# Drop blank(1st row) and "Prefer not to say"(4th row) responses
genderTable = genderTable[-c(1,4), ]
colnames(genderTable) = c("population","sample")
# Test whether sample(female 889, male 105) matches with population for gender
popSex = c(0.87, 0.13)
sampleSex = c(889,105)
chisq.test(sampleSex, p=popSex)
# Weight
weight = popSex / prop.table(sampleSex)


###-----2-----###
# Subset importance ratings (Q6)
Q6 = yogurt[,47:59]
impvars = c("AllNatural","Blended","CalorieLevel","Consistency","FatLevel","FruitOnTheBottom",
               "Organic","Price","ProteinLevel","rbSTFree","SideBySideCup","Taste","Texture")
colnames(Q6) = impvars
# Define 5("unsure") as NA
Q6[Q6[,1:13]==5] = NA
# Average importance ratings
impMean = colMeans(Q6[,impvars], na.rm=TRUE)
impSE = apply(Q6[,impvars], 2, sd, na.rm=TRUE) / sqrt(colSums(!is.na(Q6[,impvars])))
impDF = data.frame(impMean, impSE)
impDF = impDF[order(impDF$impMean, decreasing=TRUE),]
# Error Bar Plot
library(ggplot2)
dodge =  position_dodge(width = 0.75)
library(RColorBrewer)
green = rev(brewer.pal(9,"Greens"))
green = green[-(7:9)]
library(grDevices)
green_range = colorRampPalette(green)
order = factor(rownames(impDF), levels = rownames(impDF))
impDF2 = cbind(impDF, order)
plot = ggplot(impDF2, aes(x=order, y=impMean, ymax=impMean+impSE, ymin=impMean-impSE, fill=order))
plot + geom_bar(position=dodge, stat="identity", width=0.75) +
    scale_fill_manual(values=green_range(13))+
    geom_errorbar(position=dodge, width=0.9) +
    labs(x="Importance", y="Average Importance Rating") +
    theme(axis.text.x=element_text(angle=30, hjust=1, size=12)) +
    theme(panel.grid.major=element_blank(), panel.background=element_blank())


###-----3-----###
# Subset brand attribute ratings(all natural=106&132, price=110&138, taste=113&140) for Fage(Q24) and Oikos(Q30)
Q4=yogurt[,c(106,110,113,132,138,140)]
# Define 6("unsure") as NA
Q4[Q4[,1:6]==6] = NA
# Test same variable(attribute ratings) using two different samples(brands)
t.test(Q4[,1], Q4[,4], na.rm=TRUE)
t.test(Q4[,2], Q4[,5], na.rm=TRUE)
t.test(Q4[,3], Q4[,6], na.rm=TRUE)

###-----5-----###
# Add cooking column for importance ratings(Q6)
Q6$cooking = yogurt$Question12DoyouuseGreekYogurtforcooking
usageDF = Q6[,c("AllNatural","Organic","rbSTFree","Price","cooking")]
var = c("AllNatural","Organic","rbSTFree","Price")
cook = usageDF$cooking=="Yes"
# t-test compare differences in means
for (i in 1:4){
    cat(paste("***-----", var[i], "-----***"), fill=TRUE)
    print(t.test(usageDF[cook==TRUE,var[i]], usageDF[cook==FALSE,var[i]], na.rm=TRUE))
}
