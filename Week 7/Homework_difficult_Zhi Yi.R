# Include Date and clean opponent names properly 

# Data cleaning and munging
files <- dir("../RawFiles_2017_2018", full.names = TRUE)
all <- lapply(files, scan, what = "", sep = "\n")

# Write a function that does all the regex, then use lapply to apply to the different teams
cleanBballData <- function(x){
  team.name <- gsub(".*ATHLETICS_URL\">(.*)</a>.*", "\\1", 
                    x[grep("ATHLETICS_URL", x)])
  team.id <- gsub(".*/team/(\\d*)/roster/.*", "\\1", x[grep("Roster", x)])
  scores <- gsub(".*TEAM_WIN\">(.*)</a>", "\\1", x[grep(".*TEAM_WIN", x)])
  dates <- gsub("<td class=\"smtext\">(.*)</td>", "\\1", 
                x[grep("<td class=\"smtext\"(.*)</td>", x)])
  opponent <- x[grep("<td class=\"smtext\">$", x)+2]
  opponent <- gsub(".*/team/\\d*/12911\">(.*)</a>", "\\1", opponent)
  opponent.id <- x[grep("<td class=\"smtext\">$", x)+2]
  opponent.id <- gsub(".*/team/(\\d*)/12911\".*", "\\1", opponent.id)
  tmp <- data.frame(team.name, team.id, opponent, 
                    opponent.id, scores, stringsAsFactors = FALSE)
  tmp$location <- ifelse(grepl("@", tmp$opponent), gsub(".*@(.*)", "\\1", tmp$opponent),
                         tmp$opponent)
  tmp$team.score <- gsub(".{2}(\\d*) .*", "\\1", tmp$scores)
  tmp$opponent.score <- gsub(".{2}\\d* - (\\d*).*", "\\1", tmp$scores)
  tmp$OT <- ifelse(grepl("OT", tmp$scores), gsub(".*\\((\\d*)OT\\)", "\\1", tmp$scores),
                   NA)
  return(tmp)
}

y <- lapply(all, cleanBballData)
z <- do.call(rbind, y)
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
# Take the win margin and just analyze it 

played.yale <- sort(unique(z[grep("Yale", z$opponent), "team.id"]))
yale.played <-sort(unique(z[grep("Yale", z$team.name), "opponent.id"]))

played.ark <- sort(unique(z[grep("108", z$opponent.id), "team.id"]))
ark.played <- sort(unique(z[grep("108", z$team.id), "opponent.id"]))
