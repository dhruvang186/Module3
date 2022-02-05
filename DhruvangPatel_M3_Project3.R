#name
print("Dhruvang Patel")

#install plyr package
r=getOption("repos")
r["CRAN"]="http://cran.us.r-project.org"
options(repos=r)
install.packages("plyr")
library(plyr)

#install FSA package
install.packages("FSA")
library(FSA) 

#install FSAdata package
install.packages("FSAdata")
library(FSAdata)

#install magrittr package
install.packages("magrittr")
library(magrittr)

#install dplyr package
install.packages("dplyr")
library(dplyr)

#install tidyverse package
install.packages("tidyverse")
library(tidyverse)

#install tidyr package
install.packages("tidyr")
library(tidyr)

#2.Import the dataset
bio <- read.csv("C:/Users/dhruvang/Downloads/inchBio.csv")
bio

#3.Display head, tail, and structure of bio
#First five
head(bio, 5)
#Last five
tail(bio, 5)
#structure of bio
structure(bio)

#4. Create object with spices record
counts <- table(bio$species)
counts

#5. Display just names of spices
unique(bio$species)

#6. Create object with different species
tmp <- table(bio$species)
tmp

#7. Create subset and display first five
tmp2 <- subset(bio, select = species)
head(tmp2, 5)

#8. Creating table and addressing the class
w <- table(bio$species)
w
class(w)

#9. Converting table name
t <- as.data.frame(w)
t

#10. Extract Frequency information 
t$Freq

#11.Creating cSpec table for species 
cSpec<-table(bio$species)
cSpec

#12. Creating cSpecPct table with percent of record
cSpecPct <- prop.table(cSpec)*100
cSpecPct
class(cSpecPct)

#13.Change to dataframe
u <- as.data.frame(cSpecPct)
u
class(u)

#14. creating barplot
barplot(cSpec, main = "Fish Count",
        ylab = "COUNTS",
        col = "light green",
        las=2,
        cex.names = 0.60)

#15.Creating barplot named Fish relative Frequency
barplot(cSpecPct, main = "Fish Relative Frequency",
        ylim=c(0,40),
        ylab = "COUNTS",
        col.lab="light blue")

#16. Rearrange in descending order
d <- u[order(-u$Freq),]
d

#17.Rename colums in table
names(d)[names(d)=="Var1"] <- "Species"
names(d)[names(d)=="Freq"] <- "RelFreq"
d

#18. Add variable to the table
t$Freq
tdesc <- t[order(-t$Freq),]
tdesc$Freq

d <- d %>% mutate(cumfreq=cumsum(d$RelFreq), counts=tdesc$Freq, cumcounts=cumsum(tdesc$Freq))
d

#19. Creating parameter variable
def_par <- par(no.readonly = TRUE)
def_par

#20.Creating Barplot
pc <- barplot(d$counts, width = 1, space = 0.15,border = NA, axes = F, ylim = c(0,3.05*228), ylab = "Cummulative Counts", names.arg = d$Species, las=2, cex.names = 0.70, main = "Species Pareto", d$counts,na.rm=TRUE)

#21. Add cumulative counts
pc <- barplot(d$counts, width = 1, space = 0.15,border = NA, axes = F, ylim = c(0,3.05*228), ylab = "Cummulative Counts", names.arg = d$Species, las=2, cex.names = 0.70, main = "Species Pareto")
lines(pc, d$cumcounts, type = "b", cex = 0.7, pch = 19, col="cyan4")

#22. Placing grey box around plot
pc <- barplot(d$counts, width = 1, space = 0.15,border = NA, axes = F, ylim = c(0,3.05*228), ylab = "Cummulative Counts", names.arg = d$Species, las=2, cex.names = 0.70, main = "Species Pareto")
lines(pc, d$cumcounts, type = "b", cex = 0.7, pch = 19, col="cyan4")
box(col = "grey62")

#23. Add left axis in plot
pc <- barplot(d$counts, width = 1, space = 0.15,border = NA, axes = F, ylim = c(0,3.05*228), ylab = "Cummulative Counts", names.arg = d$Species, las=2, cex.names = 0.70, main = "Species Pareto")
lines(pc, d$cumcounts, type = "b", cex = 0.8, pch = 19, col="cyan4")
box(col = "grey62")
axis(side = 2, at = c(0, d$cumcounts), las = 1, col.axis = "grey62", col = "grey62", cex.axis = 0.8)

#24,25. Add axis on right and add name in title
pc <- barplot(d$counts, width = 1, space = 0.15,border = NA, axes = F, ylim = c(0,3.05*228), ylab = "Cummulative Counts", names.arg = d$Species, las=2, cex.names = 0.70, main = "Species Pareto\nDhruvang Patel")
lines(pc, d$cumcounts, type = "b", cex = 0.8, pch = 19, col="cyan4")
box(col = "grey62")
axis(side = 2, at = c(0, d$cumcounts), las = 1, col.axis = "grey62", col = "grey62", cex.axis = 0.8)
axis(side = 4, at = c(0, d$cumcounts), labels = paste(c(0, round(d$cumfreq)) ,"%",sep=""), 
     las = 1, col.axis = "cyan4", col = "cyan4", cex.axis = 0.8)

