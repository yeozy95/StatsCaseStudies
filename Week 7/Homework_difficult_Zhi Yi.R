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
  opponent <- ifelse(grepl("@", opponent), ifelse(grepl("<br/>", opponent),
                                                  gsub("(.*?)<br/>.*", "\\1", opponent),
                                                  gsub(".*@ (.*)", "\\1", opponent)),
                     opponent)
  opponent.id <- x[grep("<td class=\"smtext\">$", x)+2]
  opponent.id <- gsub(".*/team/(\\d*)/12911\".*", "\\1", opponent.id)
  tmp <- data.frame(team.name, date = dates, team.id, opponent, 
                    opponent.id, scores, stringsAsFactors = FALSE)
  tmp$location <- ifelse(grepl("@", tmp$opponent), gsub(".*@(.*)", "\\1", tmp$opponent),
                         tmp$opponent)
  tmp$team.score <- gsub(".{2}(\\d*) .*", "\\1", tmp$scores)
  tmp$opponent.score <- gsub(".{2}\\d* - (\\d*).*", "\\1", tmp$scores)
  tmp$OT <- ifelse(grepl("OT", tmp$scores), gsub(".*\\((\\d*)OT\\)", "\\1", tmp$scores),
                   NA)
  return(tmp)
}

# Apply the cleaning function to all the data
y <- lapply(all, cleanBballData)
z <- do.call(rbind, y)
# Take the dataframe with names and ID from the previous exercise
team.name.id <- read.csv("team_name_id.csv", as.is = TRUE)
# There are some opponent.id with names instead of ID. use as.numeric to coerce characters
# to NA
z$opponent.id <- as.numeric(z$opponent.id)
# This doesn't work.. can't think of any other way to fix the opponent ID issue
z$opponent.id <- ifelse(is.na(z$opponent.id), team.name.id[match(trimws(z$opponent), team.name.id$names), "no"],
                        opponent.id)


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
