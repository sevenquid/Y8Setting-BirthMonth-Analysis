ageCor <- function(gender = "EVERYONE") {  #gender = "M" or "F" or "EVERYONE" (default)
    library(data.table)
    library(tidyr)
    library(MASS)
    setwd("~/Micky/Y8 Setting against Birthday Analysis")
    

    #read in pupils
    currentpupils <- read.csv("Current pupils.csv", stringsAsFactors = F)
    currentpupils <- currentpupils[, c(9, 12, 13, 15, 18)]
    colnames(currentpupils)[1] <- "UPN"
    currentpupils$UPN <- suppressWarnings(as.numeric(currentpupils$UPN))
    
    pastpupils <- read.csv("Past pupils.csv", stringsAsFactors = F)
    pastpupils <- pastpupils[, c(2, 9, 8, 10, 12)]
    colnames(pastpupils)[1] <- "UPN"
    pastpupils$UPN <- suppressWarnings(as.numeric(pastpupils$UPN))
    
    lastyearpupils <- read.csv("Previous Academic Year Pupils.csv", stringsAsFactors = F)
    lastyearpupils <- lastyearpupils[, c(9, 12, 13, 15, 18)] #lastyearpupils$FORM_YEAR == 12
    colnames(lastyearpupils)[1] <- "UPN"
    lastyearpupils$UPN <- suppressWarnings(as.numeric(lastyearpupils$UPN))
    
    allpupils <- rbind(currentpupils, pastpupils, lastyearpupils)
    allpupils <- unique(allpupils, by = "UPN")


    #read in and clean setting data
    b2014 <- read.csv("2014-15 Year 8 Boys.csv", skip = 2, stringsAsFactors = F)
    b2014 <- b2014[, c(1, 2, 5)]
    b2014[44, 3] <- 2
    colnames(b2014) <- c("UPN", "Name", "Set")
    g2014 <- read.csv("2014-15 Year 8 Girls.csv", skip = 2, stringsAsFactors = F)
    g2014 <- g2014[1:92, 1:3]
    g2014[1:26, 3] <- 1
    g2014[30:54, 3] <- 2
    g2014[58:78, 3] <- 3
    g2014[82:92, 3] <- 4
    colnames(g2014) <- c("UPN", "Name", "Set")
    
    b2013 <- read.csv("2013-14 Year 8 Boys.csv", skip = 3, stringsAsFactors = F)
    b2013 <- b2013[1:108, c(1, 3, 2)]
    b2013[grep("2[a-z]", b2013[, 3]), ][, 3] <- 2
    colnames(b2013) <- c("UPN", "Name", "Set")
    g2013 <- read.csv("2013-14 Year 8 Girls.csv", skip = 1, stringsAsFactors = F)
    g2013 <- g2013[2:85, c(1, 3)]
    g2013[1:24, 3] <- 1
    g2013[27:47, 3] <- 2
    g2013[50:69, 3] <- 3
    g2013[72:84, 3] <- 4
    colnames(g2013) <- c("UPN", "Name", "Set")
    
    b2012 <- read.csv("2012-13 Year 8 Boys.csv", skip = 4, stringsAsFactors = F)
    b2012 <- b2012[1:87, 3:4]
    b2012[2:25, 3] <- 1
    b2012[29:50, 3] <- 2
    b2012[54:70, 3] <- 3
    b2012[74:87, 3] <- 4
    colnames(b2012) <- c("UPN", "Name", "Set")
    g2012 <- read.csv("2012-13 Year 8 Girls.csv", stringsAsFactors = F)
    g2012 <- g2012[, c(2, 3, 7)]
    g2012[59, 3] <- 3
    colnames(g2012) <- c("UPN", "Name", "Set")
    
    b2011 <- read.csv("2011-12 Year 8 Boys.csv", skip = 4, stringsAsFactors = F)
    b2011 <- b2011[, c(3, 4, 5, 2)]
    b2011 <- cbind(as.character(b2011[, 1]), 
                   paste0(b2011[, 2], ", ", b2011[, 3]), 
                   b2011[, 4])
    colnames(b2011) <- c("UPN", "Name", "Set")
    g2011 <- read.csv("2011-12 Year 8 Girls.csv", skip = 4, stringsAsFactors = F)
    g2011 <- g2011[, c(3, 4, 5, 2)]
    g2011 <- cbind(g2011[, 1], paste0(g2011[, 2], ", ", g2011[, 3]), g2011[, 4])
    colnames(g2011) <- c("UPN", "Name", "Set")
    
    b2010 <- read.csv("2010-11 Year 8 Boys.csv", skip = 3, stringsAsFactors = F)
    b2010 <- cbind(rep("", 83), b2010[1:83, c(2, 3, 1)])
    b2010 <- cbind(as.character(b2010[, 1]), 
                   paste0(b2010[, 2], ", ", b2010[, 3]), 
                   b2010[, 4])
    colnames(b2010) <- c("UPN", "Name", "Set")
    g2010 <- read.csv("2010-11 Year 8 Girls.csv", skip = 3, stringsAsFactors = F)
    g2010 <- cbind(rep("", 77), g2010[1:77, c(2, 3, 1)])
    g2010 <- cbind(as.character(g2010[, 1]), 
                   paste0(g2010[, 2], ", ", g2010[, 3]), 
                   g2010[, 4])
    colnames(g2010) <- c("UPN", "Name", "Set")
    
    b2009 <- read.csv("2009-10 Year 8 Boys.csv", skip = 3, stringsAsFactors = F)
    b2009 <- cbind(rep("", 83), b2009[1:83, c(2, 3, 1)])
    b2009 <- cbind(as.character(b2009[, 1]),
                   paste0(b2009[, 2], ", ", b2009[, 3]), 
                   b2009[, 4])
    colnames(b2009) <- c("UPN", "Name", "Set")
    g2009 <- read.csv("2009-10 Year 8 Girls.csv", skip = 3, stringsAsFactors = F)
    g2009 <- cbind(rep("", 83), g2009[1:83, c(2, 3, 1)])
    g2009 <- cbind(as.character(g2009[, 1]),
                   paste0(g2009[, 2], ", ", g2009[, 3]), 
                   g2009[, 4])
    colnames(g2009) <- c("UPN", "Name", "Set")
    
    b2008 <- read.csv("2008-09 Year 8 Boys.csv", skip = 4, stringsAsFactors = F)
    b2008 <- cbind(rep("", 83), b2008[1:83, c(2, 3, 1)])
    b2008 <- cbind(as.character(b2008[, 1]),
                   paste0(b2008[, 2], ", ", b2008[, 3]), 
                   b2008[, 4])    
    colnames(b2008) <- c("UPN", "Name", "Set")
    g2008 <- read.csv("2008-09 Year 8 Girls.csv", skip = 1, stringsAsFactors = F)
    g2008 <- cbind(rep("", 83), g2008[1:83, c(1, 2, 4)])
    g2008 <- cbind(as.character(g2008[, 1]),
                   paste0(g2008[, 2], ", ", g2008[, 3]), 
                   g2008[, 4])
    colnames(g2008) <- c("UPN", "Name", "Set")
    
    b2007 <- read.csv("2007-08 Year 8 Boys.csv", skip = 2, stringsAsFactors = F)
    b2007 <- b2007[1:70, c(1, 2, 3, 6)]
    b2007[1:17, 4] <- 1
    b2007[c(20:25, 29:39), 4] <- 2
    b2007[c(26:28, 42:58), 4] <- 3
    b2007[61:70, 4] <- 4
    b2007 <- cbind(as.character(b2007[, 1]),
                   paste0(b2007[, 2], ", ", b2007[, 3]), 
                   b2007[, 4])
    colnames(b2007) <- c("UPN", "Name", "Set")
    g2007 <- read.csv("2007-08 Year 8 Girls.csv", skip = 0, stringsAsFactors = F)
    g2007 <- g2007[2:79, c(1, 2, 4)]
    g2007 <- cbind(rep("", 78), g2007)
    g2007 <- cbind(as.character(g2007[, 1]),
                   paste0(g2007[, 2], ", ", g2007[, 3]), 
                   g2007[, 4])
    colnames(g2007) <- c("UPN", "Name", "Set")
    
    b2006 <- read.csv("2006-07 Year 8 Boys.csv", skip = 2, stringsAsFactors = F)
    b2006 <- b2006[1:101, c(1, 2, 3, 6)]
    b2006[1:24, 4] <- 1
    b2006[27:49, 4] <- 1.75
    b2006[52:73, 4] <- 2.5
    b2006[78:90, 4] <- 3.25
    b2006[93:101, 4] <- 4
    b2006 <- cbind(b2006[, 1],
                   paste0(b2006[, 2], ", ", b2006[, 3]), 
                   b2006[, 4])
    colnames(b2006) <- c("UPN", "Name", "Set")
    g2006 <- read.csv("2006-07 Year 8 Girls Amended.csv", skip = 1, stringsAsFactors = F)
    g2006 <- g2006[, c(1, 2, 3)]
    g2006[1:25, 4] <- 1
    g2006[29:50, 4] <- 2
    g2006[53:68, 4] <- 3
    g2006[71:80, 4] <- 4
    g2006 <- cbind(g2006[, 1],
                   paste0(g2006[, 2], ", ", g2006[, 3]), 
                   g2006[, 4])
    colnames(g2006) <- c("UPN", "Name", "Set")
    
    b2005 <- read.csv("2005-06 Year 8 Boys.csv", skip = 0, stringsAsFactors = F)
    b2005 <- b2005[, c(1, 2, 3)]
    b2005 <- cbind(rep("", 82), b2005)
    b2005 <- cbind(as.character(b2005[, 1]),
                   paste0(b2005[, 2], ", ", b2005[, 3]), 
                   b2005[, 4])
    colnames(b2005) <- c("UPN", "Name", "Set")
    g2005 <- read.csv("2005-06 Year 8 Girls.csv", skip = 2, stringsAsFactors = F)
    g2005 <- g2005[, c(8, 1, 2, 7)]
    g2005[6, 4] <- 1
    g2005[45, 4] <- 2
    g2005 <- cbind(as.character(g2005[, 1]),
                   paste0(g2005[, 2], ", ", g2005[, 3]), 
                   g2005[, 4])
    colnames(g2005) <- c("UPN", "Name", "Set")
    
    b2004 <- read.csv("2004-05 Yr8.csv", skip = 0, stringsAsFactors = F)
    b2004 <- bg2004[1:76, 2]
    b2004 <- cbind(rep("", 76), b2004, rep("", 76))
    b2004[1:24, 3] <- 1
    b2004[27:46, 3] <- 2
    b2004[49:68, 3] <- 3
    b2004[71:76, 3] <- 4
    colnames(b2004) <- c("UPN", "Name", "Set")
    
    b2003 <- read.csv("2003-04 Y8.csv", skip = 0, stringsAsFactors = F)
    b2003 <- b2003[1:74, ]
    b2003 <- cbind(rep("", 74), b2003)
    b2003 <- cbind(as.character(b2003[, 1]),
                   paste0(b2003[, 2], ", ", b2003[, 3]), 
                   b2003[, 4])
    colnames(b2003) <- c("UPN", "Name", "Set")
    
    
    #b2013, b2006 not included because 5 sets instead of 4
    #g2004 not included because sets 1, 2A, 2a, 3
    
    dfM <- rbind(b2014, b2012, b2011, b2010, b2009, b2008, b2007, b2005, b2004)
    dfF <- rbind(g2014, g2013, g2012, g2011, g2010, g2009, g2008, g2007, g2006,
                 g2005)
    dfM <- cbind(dfM, rep("M", nrow(dfM)))
    dfF <- cbind(dfF, rep("F", nrow(dfF)))
    colnames(dfM)[4] <- "GENDER"   
    colnames(dfF)[4] <- "GENDER" 
    df <- rbind(dfM, dfF)
    
    df <- df[df$Set != "" & !is.na(df$Set), ]
    df$UPN <- suppressWarnings(as.numeric(df$UPN))
    df$Set <- suppressWarnings(as.numeric(df$Set))
    

    #merge, UPN as index
    base1 <- merge(allpupils, df, by = "UPN")
    base1 <- base1[!is.na(base1$UPN), c("DATE_OF_BIRTH", "GENDER", "Set")]
    

    #collect rows that don't have a UPN, then try to look them up and add to base1
    noUPN <- df[is.na(df$UPN) | df$UPN == "", ]
    noUPN <- separate(noUPN, Name, c("Surname", "Firstname"), ", ", extra = "drop")
    
    unmatched <- character(0)
    for (i in 1:nrow(noUPN)) {
        possiblerows <- 
            allpupils[toupper(allpupils$SURNAME) == toupper(noUPN[i, "Surname"]), ]
        if (nrow(possiblerows) == 0) next
        for (j in 1:nrow(possiblerows)) {
            if (grepl(toupper(noUPN[i, "Firstname"]), toupper(possiblerows$FIRST_NAMES[j]))) {
                base2 <- data.frame(
                    DATE_OF_BIRTH = possiblerows$DATE_OF_BIRTH[j], 
                    GENDER = noUPN[i, "GENDER"], 
                    Set = noUPN[i, "Set"])
                base1 <- rbind(base1, base2)
                break
            }
            else if (j == nrow(possiblerows)) {
                unmatched <- c(unmatched, paste0(noUPN[i, "Firstname"], " ", noUPN[i, "Surname"]))
            }
        }    
    }
    write.csv(unmatched, "unmatched.csv")

    
    #prepare base1 for analysis - set 1st Sept as month/week/day 0 of year
    base1$DATE_OF_BIRTH <- as.Date(base1$DATE_OF_BIRTH, format = "%d/%m/%Y")
    #by month
    #base1$DATE_OF_BIRTH <- strftime(base1$DATE_OF_BIRTH, format = "%m")
    #base1$DATE_OF_BIRTH <- (as.numeric(base1$DATE_OF_BIRTH) - 9) %% 12  
    #by week
    #base1$DATE_OF_BIRTH <- strftime(base1$DATE_OF_BIRTH, format = "%U")
    #base1$DATE_OF_BIRTH <- (as.numeric(base1$DATE_OF_BIRTH) - 35) %% 53  
    #by day
    base1$DATE_OF_BIRTH <- strftime(base1$DATE_OF_BIRTH, format = "%j")
    base1$DATE_OF_BIRTH <- (as.numeric(base1$DATE_OF_BIRTH) - 244) %% 365  
                            #244) %% 366 in leap years - not yet catered for
    
    
    #calculate spearman's correlation 
    #between birth month/week/day and Y8 set
    if (gender == "EVERYONE") {
        x <- base1$DATE_OF_BIRTH
        y <- base1$Set
        n <- length(base1$Set)
        
    } else {
        x <- base1$DATE_OF_BIRTH[base1$GENDER == gender]
        y <- base1$Set[base1$GENDER == gender]
        n <- length(base1$Set[base1$GENDER == gender])
    }
    tbl <- table(x, y)
    c(colSums(tbl), cor(x, y, method = "spearman"))
}


siglevel <- function(totals_and_z = c(350, 325, 270, 170, 0.05), n = 1000) {
    totals <- as.integer(totals_and_z[1:4])
    z <- totals_and_z[5]
    rho <- numeric(0)
    for (i in 1:n) {
        x <- numeric(0)
        y <- numeric(0)
        for (j in 1:sum(totals)) {
            x <- c(x, sample(0:365, 1))
        }
        for (j in 1:4) {
            y <- c(y, rep.int(j, totals[j]))
        }
        rho <- c(rho, cor(x, y, method = "spearman"))
    }
    print("rho =")
    print(z)
    print("p-value for this rho is:")
    p <- sum(rho > z) / n
    print(p)
    if (p < 0.05) {
        print("Significant at 5% level. Reject H0: Sufficient evidence that there is
              positive correlation between birth date and Y8 set.")
    }
}


#result <- ageCor()
#siglevel(result, 10000)
