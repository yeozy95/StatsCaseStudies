#
# Case Studies: For Sept 19
#
# Note: Please read through this.  Let me know sooner rather than later
# if something isn't clear, because my ability to repond from Abu Dhabi may
# be pretty limited.  You are welcome to do some Googling about ANOVA and
# contrast sum ("contr.sum").
#
# My plan: to give an individual in-class exercise next Wednesday that will
# ask you to do some one-way ANOVA and including construction of a model
# matrix by hand.  So you want to come prepared.
#
# Then we'll have a team competition using the same data set.
# At that point my hope is that I can give you a pretty interesting
# and non-trivial Case Study to work on over the long break, where you can't just
# use lm() directly: you will NEED to construct a model matrix and we aren't
# just doing standard work with factors.
#
################################################################################
#
# PART A: See ArrayDesign.png for an image relating to these data.
#         Each slide has 16 * 24 = 384 wells.  Image processing
#         of the biological material in a well produces 130
#         variables summarizing the characteristics.  Each well
#         is a mini-experiment associated with a particular gene.
#         Because of possible slide-specific variability in the
#         processing, we start by conducting a normalization.
#
# 0. Read all the instructions and play with one of the CSV files
#    before doing any real work!  You should start by working
#    individually.  After you've given it a fair shot, get together
#    with your Team and construct a Team solution for this Part A.
#
# 1. Use lapply() followed by do.call() to read all the CSV files
#    in SlideFiles/ and merge into one big, tall data.frame.

folder <- "SlideFiles"
files <- dir(folder, full.names = TRUE)
df <- do.call("rbind", 
              lapply(list.files(folder,full = TRUE), 
                     read.csv, stringsAsFactors = FALSE))
nrow(df) == 2688 * length(files)

# Checking for NA values
table(subset(unlist(df[,1:130]), is.na(unlist(df[,1:130]))))
any(is.na(df[,1:130]))
unlist(lapply(df, function(x) any(is.na(x))))
which(is.na(df$X1))
which(is.na(df$X2))
which(is.na(df$X99))

#
# 2. For each slide (there are 33) and for each well (there are
#    384 per slide I think?) there are seven rows.  Do some
#    aggregation so that the data set becomes 1/7th the size.  Specifically,
#    calculate the median value for each of the 130 "data variables"
#    for each slide and each well.  So you're doing lots and lots of
#    calculations of medians of 7 values.  These sets of 7 values are
#    repeated measurements that are not independent, hence the desire
#    to do this aggregation.  How you do this is up to you... perhaps
#    as part of your work in Step #1, above.

# Define identifier of aggregation
df$slideWell <- paste0(df$slide, ".", df$well)
df$slideWell <- factor(df$slideWell, levels = unique(df$slideWell))

# Aggregate df by identifier
aggDf <- data.frame(matrix(ncol=ncol(df), nrow=nrow(df)/7))
colnames(aggDf) <- colnames(df)
aggValues <- aggregate(df[,1:130], FUN = function(x) {median(x, na.rm=TRUE)}, by = list(df$slideWell))
any(is.na(aggValues))
which(is.na(aggValues$X99))
df[7454*7,]
df[df$slideWell == "20.158",]
aggLabels <- aggregate(df[,131:138], FUN = unique, by = list(df$slideWell))
aggDf[,1:130] <- aggValues[,2:131]
aggDf[,131:138] <- aggLabels[,2:9]

# 3. For each of the "data variables" in columns 1:130, conduct
#    a slide-by-slide normalization.  Specifically, for each slide,
#    calculate the mean and standard deviation of the `type=="rf"`
#    measurements for that slide.  Then use these statistics to
#    normalize all the values for that slide.  The "rf" cases are
#    "negative control" spots on the microarray.  Details are
#    unimportant for your work here.  In total, you are then doing
#    130 * 33 = 4290 separate normalizations.  For this part, you
#    you should use two nested for() loops.  Brute force, it's okay!

normDf1 <- aggDf
for (i in 1:33) {
  slideDf <- aggDf[aggDf$slide == i,]
  rfMeans <- unlist(lapply(slideDf[slideDf$type == "rf",1:130], mean))
  rfSds <- unlist(lapply(slideDf[slideDf$type == "rf",1:130], sd))
  names(rfMeans) <- NULL
  names(rfSds) <- NULL
  for (j in 1:130) {
    slideDf[,j] <- (slideDf[,j] - rfMeans[j]) / rfSds[j]
  }
  normDf1[normDf$slide == i,] <- slideDf
}

normDf1$type <- factor(normDf1$type, levels = c("rf", "bscl2", "fit1", "rfnotDL", "gene"))

# 4. Implement an alternative approach to #3 above, but here use
#    one for() loop over the 130 variables.  Each step of the loop
#    should use ANOVA (a linear model via lm()) to obtain the
#    slide means and an appropriate standard deviation for the
#    normalization, again based only on the `type=="rf"` measurements.
#    This will not be exactly the same normalization, but should be
#    close.  Make sure you understand why.  Your goal is to
#    impress me with your use of the result of lm() in obtaining
#    a very elegant solution.

normDf2 <- aggDf
normDf2$slide <- factor(normDf2$slide, levels = unique(normDf2$slide))
rfDf <- normDf2[normDf2$type == "rf",]
for (i in 1:130) {
  lm.rfBySlide <- lm(rfDf[,i] ~ slide, data = rfDf)
  out <- summary(lm.rfBySlide)
  baselineMean <- out$coefficients[,1][1]
  rfMeans <- unlist(lapply(out$coefficients[,1], function(x) {baselineMean + x}))
  rfMeans[1] <- baselineMean
  rfSds <- unlist(out$coefficients[,2])
  names(rfMeans) <- NULL
  names(rfSds) <- NULL
  normalize <- function(j) {
      normalized <- (normDf2[normDf2$slide == j,i] - rfMeans[j]) / rfSds[j]
      return(normalized)
  }
  normDf2[,i] <- as.vector(do.call("cbind",(lapply(1:33, normalize))))
  # for (j in 1:33) {
  #   normDf2[normDf2$slide == j,i] <- (normDf2[normDf2$slide == j,i] - rfMeans[j]) / rfSds[j]
  # }
}

normDf2$type <- factor(normDf2$type, levels = c("rf", "bscl2", "fit1", "rfnotDL", "gene"))

# 5. For one variable and one slide, produce a scatterplot of
#    the normalizations from #3 and #4, above.  Before you do this,
#    ask yourself what you expect to see.  Then do it.  Were you right?

variable = 1
slide = 1
plot(normDf1[normDf1$slide == 1,1], normDf2[normDf2$slide == 1,1])

# 6. Now consider variable `X99` only, after having completed the
#    normalizations.  Start with some graphical exploration.  Then:

plot(normDf1[,99], normDf2[,99])
boxplot(X99 ~ type, normDf1)
boxplot(X99 ~ type, normDf2)

#    a. Using one-way ANOVA, are any of the categories of `type`
#       significantly different from "rf"?

anovaTest <- function(normDf, baseline, factorLevel) {
  normDf$type <- factor(normDf$type, levels = factorLevel)
  if (baseline == "single") {
    lm.normDf <- lm(X99 ~ type, data = normDf)
  }
  else if (baseline == "overall") {
    lm.normDf <- lm(X99 ~ type, data = normDf, 
                    contrasts=list(type = c("contr.sum", "contr.sum")))
  }
  print(summary(lm.normDf))
  return(lm.normDf)
}

factorLevel = c("rf", "bscl2", "fit1", "rfnotDL", "gene")
anovaTest(normDf1, "single", factorLevel)
anovaTest(normDf2, "single", factorLevel)

# It appears that bscl2, rfnotDL, and gene are significantly different from rf.

#    b. Using one-way ANOVA, are any of the categories of `type`
#       significantly higher or lower than the overall average,
#       approximately?  Because this isn't a perfectly balanced
#       design, the question isn't technically precise but is very
#       close.

anovaTest(normDf1, "overall", factorLevel)
anovaTest(normDf2, "overall", factorLevel)

# Based on the p-values, they all seem to be pretty different from the overall average.

#    c. Construct the model matrices (or design matrices) by hand
#       to reproduce the coefficient estimates for a. and b. above.

construct_design_matrix <- function(normDf, baseline, factorLevel) {
  normDf$type <- factor(normDf$type, levels = factorLevel)
  lm.all <- anovaTest(normDf, baseline)
  if (baseline == "single") {
    X <- cbind(rep(1, nrow(normDf1)),
               as.numeric(normDf1$type == factorLevel[2]),
               as.numeric(normDf1$type == factorLevel[3]),
               as.numeric(normDf1$type == factorLevel[4]),
               as.numeric(normDf1$type == factorLevel[5]))
    colnames(X) <- c("intercept", factorLevel[2:5])
    naRowNum <- which(is.na(normDf1$X99))
    X <- X[-naRowNum,]
    coefficientMat <- solve(t(X) %*% X) %*% t(X) %*% normDf1$X99[-naRowNum]
    # R deals with NA rows in model matrix by removing them also
    # print(all(model.matrix(lm.all) == X))

  }
  else if (baseline == "overall") {
    X <- model.matrix(lm.all)
    # How to construct model matrix with contr.sum by hand?
    means <- aggregate(normDf$X99, function(x) mean(x, na.rm=TRUE), by = list(normDf$type))
    contrasts(normDf$type) <- "contr.sum"
    Xcontrasts <- contrasts(normDf$type)
    XMat <- cbind(rep(1,5),Xcontrasts)
    coefficientMat <- solve(XMat)%*%means$x
  }
  return(list(X, coefficientMat))
}

factorLevel = c("rf", "bscl2", "fit1", "rfnotDL", "gene")
anovaTest(normDf1, "single", factorLevel)
construct_design_matrix(normDf1, "single", factorLevel)[2]
anovaTest(normDf1, "overall", factorLevel)
construct_design_matrix(normDf1, "overall", factorLevel)[2]

#    d. Go back to your model in b, and relevel the type factor so
#       that "rfnotDL" is the first level.  Refit the model.  Prove
#       that you understand the constraint used with "contr.sum"
#       for the coefficients and that you can obtain all five
#       coefficients either by fitting the model twice or by doing
#       one fit plus a little algebra.

factorLevel <- c("rfnotDL", "bscl2", "fit1", "rf", "gene")

anovaTest(normDf1, "overall", factorLevel)
construct_design_matrix(normDf1, "overall", factorLevel)[2]

################################################################################
#
# PART B: More (but different) basketball!  Although you should have
#         an individual solution, you may talk with each other and
#         get (and provide) R tips.  We'll just do the first part this week.
#
# URL: http://stats.ncaa.org/team/inst_team_list?sport_code=WBB&division=1
#
# 1. Use scan() to pull this first page into R.  Study it.

page <- "http://stats.ncaa.org/team/inst_team_list?sport_code=WBB&division=1"
x <- scan(page, what="", sep="\n")
length(x)
head(x)
tail(x)

# 2. 12911 is the code for this 2017-18 season of results for Women's
#    basketball, Division I.
#
# 3. Construct a 2-column data frame containing the team names and the
#    team numbers.  Yale's team number is 813, for example.

# Extract html code related to team information
nRow <- length(x)
cleanedX <- c()
for (i in 1:nRow) {
  if (grepl("<a href=\"/team/", x[i]) && grepl("</a>", x[i])) {
    str <- trimws(x[i])
    cleanedX <- c(cleanedX, str)
  }
}
cleanedX <- cleanedX[3:length(cleanedX)]

# Extract team name and team number
teamDf <- data.frame(matrix(nrow = length(cleanedX), ncol = 2))
colnames(teamDf) <- c("team.name", "team.number")
for (i in 1:length(cleanedX)) {
  str <- cleanedX[i]
  teamNum <- sub(pattern = "<a href=\"/team/([0-9]+).*", "\\1", str)
  teamName <- sub(pattern = "<a href=\"/team/[0-9]+/12911\">(.*)</a>", "\\1", str)
  teamName <- gsub("&amp;", "", teamName, fixed = TRUE)
  teamName <- gsub("&#x27;s", "", teamName, fixed = TRUE)
  teamDf[i,] <- c(teamName, teamNum)
}

# 4. Next, consider the following:

code <- "12911"
baseurl <- paste('http://stats.ncaa.org/team/index/', code, '?org_id=', sep="")
thisurl <- paste(baseurl, 813, sep="")
thisurl         # Copy this into a browser to see Yale's results!

# Your challenge for 4: construct the urls for all the teams, scan
# the team pages from the web (perhaps using try() and being nice
# with little time delays), and then SAVE a copy to a data folder
# "RawFiles_2017_2018" using writeLines().  Make up a friendly
# file name with the team numbers.

teamUrls <- c()
for (i in 1:nrow(teamDf)) {
  url <- paste(baseurl, teamDf$team.number[i], sep="")
  teamUrls <- c(teamUrls, url)
}

for (i in 1:length(teamUrls)) {
  print(paste0(c(i, teamDf$team.name[i], teamDf$team.number[i]), sep = " "))
  page <- teamUrls[i]
  tryCatch({
    x <- scan(page, what="", sep="\n") 
  },
  error = function(err) {
    message(paste("Error:", teamDf$team.name[i], teamDf$team.number[i]))
  })
  Sys.sleep(runif(1, 1.0, 2.0))
  filePath <- paste0("RawFiles_2017_2018/", teamDf$team.number[i], ".txt")
  fileConn <- file(filePath)
  writeLines(x, fileConn)
  close(fileConn)
}

# PLEASE: We don't want to hammer this site with traffic; it is
# slow enough as it is.  Test your code for 3-4 teams.  Make sure it
# is doing exactly what you want.  Then -- and only then -- run it
# for all the teams.  Hopefully only once.
#
# Reasoning: once you have a local copy of these files from the web,
# you can do everything far more quickly and without generating repeated
# web traffic.
#
# I'm not asking you to process the hundreds of team files for this week.
# At this point, we just want to have local copies of the team files so
# we don't have to rely upon the web network connection.


