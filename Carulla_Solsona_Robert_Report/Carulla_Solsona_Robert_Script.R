if(!require("rvest")) install.packages("rvest")
if(!require("lubridate")) install.packages("lubridate")
if(!require("stringr")) install.packages("stringr")
if(!require("Hmisc")) install.packages("Hmisc")

require(rvest)
require(lubridate)
require(stringr)
require(Hmisc)

platform <- c("all", "ps4", "xboxone", "switch", "pc", "wii-u", "3ds", "vita", "ios")

# ----------------------------- METASCORE --------------------------------------------------------------------------------------------------- 
datalist.meta <- list()

for (i in 1:length(platform)){
  url <- paste0("http://www.metacritic.com/browse/games/score/metascore/all/", platform[i], "/filtered?sort=desc")
  myurl <- read_html(url)
  
  games_console <- myurl %>%
    html_nodes("div.product_item  a") %>%
    html_text()
  
  metacritic.ratings <- myurl %>%
    html_nodes("div.metascore_w") %>%
    html_text() %>%
    as.numeric()
  
  dates <- myurl %>%
    html_nodes("div.product_date") %>%
    html_text()
  
  dates <- gsub("\n", "", dates)
  dates <- gsub(" ", "", dates)
  dates <- mdy(dates)
  
  games_console <- gsub("\n", "", games_console)
  games_console <- str_trim(games_console)
  
  console <- str_extract(games_console, "\\(.*\\)")
  console <- str_replace(console, "\\(", "")
  console <- str_replace(console, "\\)", "")
  
  games <- str_replace(games_console, "\\(.*\\)", "")
  games <- str_trim(games)
  
  datalist.meta[[i]] <- data.frame(game=games, console=console, metacritic.ratings=metacritic.ratings, date=dates)
}

all.meta <- datalist.meta[[1]]
PS4.meta <- datalist.meta[[2]][, -2]
xboxone.meta <- datalist.meta[[3]][, -2]
switch.meta <- datalist.meta[[4]][, -2]
pc.meta <- datalist.meta[[5]][, -2]
wiiu.meta <- datalist.meta[[6]][, -2]
ds3.meta <- datalist.meta[[7]][, -2]
vita.meta <- datalist.meta[[8]][, -2]
ios.meta <- datalist.meta[[9]][, -2]


# -------------------------------------------------------- USER SCORE -------------------------------------------------------------


datalist.users <- list()

for (i in 1:length(platform)){
  url <- paste0("http://www.metacritic.com/browse/games/score/userscore/all/", platform[i], "/filtered?sort=desc")
  myurl <- read_html(url)
  
  games_console <- myurl %>%
    html_nodes("div.product_item  a") %>%
    html_text()
  
  user.ratings <- myurl %>%
    html_nodes("div.metascore_w") %>%
    html_text() %>%
    as.numeric()
  
  dates <- myurl %>%
    html_nodes("div.product_date") %>%
    html_text()
  
  dates <- gsub("\n", "", dates)
  dates <- gsub(" ", "", dates)
  dates <- mdy(dates)
  
  games_console <- gsub("\n", "", games_console)
  games_console <- str_trim(games_console)
  
  console <- str_extract(games_console, "\\(.*\\)")
  console <- str_replace(console, "\\(", "")
  console <- str_replace(console, "\\)", "")
  
  games <- str_replace(games_console, "\\(.*\\)", "")
  games <- str_trim(games)
  
  datalist.users[[i]] <- data.frame(game=games, console=console, user.ratings=user.ratings, date=dates)
}

all.users <- datalist.users[[1]]
PS4.users <- datalist.users[[2]][, -2]
xboxone.users <- datalist.users[[3]][, -2]
switch.users <- datalist.users[[4]][, -2]
pc.users <- datalist.users[[5]][, -2]
wiiu.users <- datalist.users[[6]][, -2]
ds3.users <- datalist.users[[7]][, -2]
vita.users <- datalist.users[[8]][, -2]
ios.users <- datalist.users[[9]][, -2]


# ----------------------------- Games of all times -----------------------------------------------

# mean of the top 100 for metascore all games and videoconsoles 
mean(all.meta$metacritic.ratings)

mean(10*all.users$user.ratings) # it seems that the users don't think the same

hist(all.meta$metacritic.ratings) # it doesn't follow a normal
hist(10*all.users$user.ratings) # it could follor a normal


# Are they rating in different form?

t.test(all.meta$metacritic.ratings-10*all.users$user.ratings, alternative = "greater") # Yes, it seems that in general the magazines give higher rates to the videogames


# Max grade given by magazines 
max(all.meta$metacritic.ratings)
# The game is:
all.meta$game[which(all.meta$metacritic.ratings == max(all.meta$metacritic.ratings))]
# Min grade given by magazines
min(all.meta$metacritic.ratings)
# The game is:
all.meta$game[which(all.meta$metacritic.ratings == min(all.meta$metacritic.ratings))] # There are several ties
table(all.meta$metacritic.ratings) # 39 games with a 94


# The most old game of the magazines
min(all.meta$date)
# The game is:
all.meta$game[which(all.meta$date == min(all.meta$date))]

# The most new game of the magazines
max(all.meta$date)
# The game is:
all.meta$game[which(all.meta$date == max(all.meta$date))]

# difference between dates
as.numeric(diff.Date(c(min(all.meta$date), max(all.meta$date))))/365 # 22.15 years

# we create a new categorical variable from the dates
b <- c(ymd("2001-01-01"), ymd("2006-01-01"), ymd("2011-01-01"), ymd("2016-01-01"))
all.meta$interval.time <- cut2(all.meta$date, b)
all.meta$interval.time[72] <- as.factor("[2016-01-01,2018-04-20]")

# We can see which is the best interval of time for videogames regarding the magazines criteria
table(all.meta$interval.time) # it seems as if from 2001 to 2011 we had better games, is it true? or there was a trend to rate better the games?


# Max grade given by users 
max(all.users$user.ratings)
# The game is:
all.users$game[which(all.users$user.ratings == max(all.users$user.ratings))]
# Min grade given by magazines
min(all.users$user.ratings)
# The game is:
all.users$game[which(all.users$user.ratings == min(all.users$user.ratings))] # There are several ties
table(all.users$user.ratings) # 31 games with a 90, we can see that the range is a little bit lower and the lowest rate of magazines is the best rate of users



# The most old game of the magazines
min(all.users$date)
# The game is:
all.users$game[which(all.users$date == min(all.users$date))]

# The most new game of the magazines
max(all.users$date)
# The game is:
all.users$game[which(all.users$date == max(all.users$date))]

# difference between dates
as.numeric(diff.Date(c(min(all.users$date), max(all.users$date))))/365 # 22.07 years (almost the same as before)

# we create a new categorical variable from the dates
all.users$interval.time <- cut2(all.users$date, b)
all.users$interval.time[6] <- as.factor("[2016-01-01,2018-04-20]")

# We can see which is the best interval of time for videogames regarding the magazines criteria
table(all.users$interval.time) # Regarding the users criteria, it seems that the best interval of time of videogames was between 2001 and 2006 and then it decreases again


# How many games coincide in the top 100 of metascore and userscore?
sum(all.users$game %in% all.meta$game) # Only a 21% of the games are in common


# consoles in the top 100
par(las=2)
barplot(table(all.meta$console), main = "Games vs. Platform by metascore") # the best console could be x360, pc, ps2 or ps3, the games of ps or N64 were as well rated as the games of nowadays?

barplot(table(all.users$console), main = "Games vs. Platform by users") # Clearly users prefer pc or ps2











