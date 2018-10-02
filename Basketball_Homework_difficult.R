#
# Hoops Challenge (difficult version)
#
# - Assemble a data set of game results for
#   the NCAA Women's Basketball season of 2017-18
#
# - Analyze the games from that season to better understand
#   and formally model the strengths of the 349 teams.
#
# - Return from break prepared to use what you have learned
#   for the purpose of predicting the results of one additional
#   game (assumed to have been played at the end of this season).
#

# Assembling a dataset---------------------------------------------------------
# We want (at least): date, team name and ID, opponent name and ID, team score, opponent score, game location, and number of overtime periods (if applicable).

# read all html files
folder <- "RawFiles_2017_2018"
files <- dir(folder, full.names = TRUE)
fileFullPathList <- list.files(folder,full = TRUE)
htmlList <- sapply(fileFullPathList, 
                   scan, what="", sep="\n", simplify = FALSE)
fileNameList <- list.files(folder,full = FALSE)

for (i in 1:length(htmlList)) {
  assign(fileNameList[i],htmlList[[i]])
}

masterDf <- data.frame(matrix(nrow = 0, ncol = 9))
colnames(masterDf) <- c("date", 
                    "team.name", "team.ID", 
                    "opponent.name", "opponent.ID", 
                    "team.score", "opponent.score", 
                    "game.location", "no.overtime")

# How to get game location and no. of overtime games?
for (i in 1:length(htmlList)) {
# for (i in 1:1) {
  x.html <- htmlList[[i]]
  team.ID <- gsub(".html", "", fileNameList[i])
  nRow = length(x.html)
  dateVec = c()
  opponentNameVec = c()
  opponentIDVec = c()
  teamScoreVec = c()
  opponentScoreVec = c()
  for (j in 1:nRow) {
    opponent.ID = ""
    str <- trimws(x.html[j])
    if (grepl('<legend><a href="http://', str) &&
        grepl('target="ATHLETICS_URL">', str)) {
      team.name <- sub(pattern = '.*target="ATHLETICS_URL">(.*)</a>.*', 
                       "\\1", str)
    } else if (grepl('<td class="smtext">[0-9]{2}/[0-9]{2}/[0-9]{4}</td>',
                     str)) {
      date <- sub(pattern = '<td class="smtext">(.*)</td>',
                  "\\1", str)
    } else if (grepl('<a href="/team/[0-9]+/12911">', str)) {
          opponent.ID <- sub(pattern = '.*team/([0-9]+)/12911">.*',
                             "\\1", str)
          opponent.name <- trimws(sub(pattern = '.*12911">(.*)</a>.*',
                               "\\1", str))
    } else if (grepl('target="TEAM_WIN">', str)) {
      gameResult <- trimws(sub(pattern = '.*target="TEAM_WIN">(.*)</a>', 
                               "\\1", str))
      team.score <- trimws(sub(pattern = '[A-Z] ([0-9]+) -.*', 
                               "\\1", gameResult))
      opponent.score <- trimws(sub(pattern = '.* - ([0-9]+).*', 
                                   "\\1", gameResult))
    }
    # Only extracts games against opponent with link on the website
    if (opponent.ID != "") {
      dateVec = c(dateVec, date)
      opponentIDVec = c(opponentIDVec, opponent.ID)
      opponentNameVec = c(opponentNameVec, opponent.name)
      teamScoreVec = c(teamScoreVec, team.score)
      opponentScoreVec = c(opponentScoreVec, opponent.score)
    }
  }
  print(dateVec)
  # print(opponentIDVec)
  # print(opponentNameVec)
  nGames = length(opponentIDVec)
  x.df <- data.frame(
    "date" = dateVec,
    "team.name" = rep(team.name, nGames),
    "team.ID" = rep(team.ID, nGames),
    "opponent.name" = opponentNameVec,
    "opponent.ID" = opponentIDVec,
    "team.score" = teamScoreVec,
    "opponent.score" = opponentScoreVec,
    "game.location" = rep(NA, nGames),
    "no.overtime" = rep(NA, nGames)
  )
  masterDf <- rbind(masterDf, x.df)
}

write.csv(masterDf, "masterDF.csv", row.names = FALSE)
