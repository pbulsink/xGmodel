

game_number <- readline(prompt="Enter NHL Game Number: ")
setwd("~/ProgrammingStuff/R/NHLScraper/")
#Emmanuel Perry's Scraper from corsica.hockey and used with his permission######
#################################################################################
###########              START HERE TO LOAD ALL FUNCTIONS             ###########  
#################################################################################





### DRYSCRAPE ###
# Last edit: Manny (2017-07-02)



## Description
# Dryscrape contains all functions and tools related to scraping data for Corsica 
# Dependencies: Rcurl, rjson, dplyr, lubridate, doMC, user_functions, rvest



## Dependencies
require(RCurl); require(rjson); require(dplyr); 
require(lubridate); require(doMC); require(rvest)


## Objects
c(20001:21230,
  30111:30117, 30121:30127, 30131:30137, 30141:30147, 30151:30157, 30161:30167, 30171:30177, 30181:30187,
  30211:30217, 30221:30227, 30231:30237, 30241:30247,
  30311:30317, 30321:30327,
  30411:30417
) %>% 
    as.character() ->
    ds.all_games

c("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36",
  "Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36",
  "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.5.2171.95 Safari/537.36",
  "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.86 Safari/537.36"
) ->
    ds.user_agents

c("season", "game_id", "game_date", "session", 
  "event_index", "game_period", "game_seconds",
  "event_type", "event_description", "event_detail",
  "event_team", "event_player_1", "event_player_2", "event_player_3",
  "event_length", "coords_x", "coords_y", "players_substituted",
  "home_on_1", "home_on_2", "home_on_3", "home_on_4", "home_on_5", "home_on_6", 
  "away_on_1", "away_on_2", "away_on_3", "away_on_4", "away_on_5", "away_on_6", 
  "home_goalie", "away_goalie", "home_team", "away_team",
  "home_skaters", "away_skaters", "home_score", "away_score",
  "game_score_state", "game_strength_state", "highlight_code"
) ->
    ds.pbp_colnames

c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ",
  "CGY", "CHI", "COL", "DAL", "DET", "EDM",
  "FLA", "L.A", "MIN", "MTL", "N.J", "NSH",
  "NYI", "NYR", "OTT", "PHI", "PIT", "S.J",
  "STL", "T.B", "TOR", "VAN", "WPG", "WSH",
  "PHX", "ATL", "VGK", "L.V"
) ->
    ds.team_list

data.frame(event = c("FAC", "HIT", "GvTk", "GOAL", "SHOT", "MISS", "BLOCK", "PENL",
                     "STOP", "PRDY", "PSTR", "PEND", "PERD", "SOC", "GEnd", "SOut",
                     "error", "TAKE", "GIVE", "early intermission", "nothing", "nothing"
),
code = as.character(c(502, 503, 504, 505, 506, 507, 508, 509,
                      516, 517, 518, 519, 520, 521, 522, 0, 
                      9999, 1401, 1402, -2147483648, 1, 5
)
)
) ->
    ds.espn_codes


## Meta Functions
# Get PBP
ds.get_pbp <- function(season, game_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
    
    ## Description
    # get_pbp() imports the PBP page corresponding to a given year and game ID and returns a list object
    
    url <- paste("http://www.nhl.com/scores/htmlreports/",
                 as.character(season),
                 "/PL0",
                 as.character(game_id),
                 ".HTM",
                 sep = ""
    )
    
    raw_text <- NULL
    
    while(class(raw_text) != "character" & try_tolerance > 0) {
        
        try(
            url %>%
                getURL(header = FALSE,
                       .opts = curlOptions(referer = "nhl.com",
                                           verbose = FALSE,
                                           followLocation = TRUE,
                                           useragent = agents[sample(1:length(agents), 1)]
                       )
                )
        ) ->
            raw_text
        
        try_tolerance <- try_tolerance - 1
        
    }
    
    html <- read_html(raw_text)
    
    all <- html_nodes(html, "td")
    body <- html_nodes(html, ".bborder")
    full_text <- html_text(all)
    body_text <- html_text(body)
    
    pbp_list <- list(full_text, body_text)
    
    return(pbp_list)
    
}

# Get Shifts
ds.get_shifts <- function(season, game_id, venue, source, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
    
    ## Description
    # get_shifts() imports the shift report page corresponding to a given year, game ID and venue and returns a list object
    
    if(tolower(source) == "htm") {
        
        if(tolower(venue) == "home") {
            
            url <- paste("http://www.nhl.com/scores/htmlreports/", 
                         season, 
                         "/TH0", 
                         game_id, 
                         ".HTM", 
                         sep = ""
            )
            
            
        } else if(tolower(venue) == "away") {
            
            url <- paste("http://www.nhl.com/scores/htmlreports/", 
                         season, 
                         "/TV0", 
                         game_id, 
                         ".HTM", 
                         sep = ""
            )
            
        }
        
        raw_text <- NULL
        
        while(class(raw_text) != "character" & try_tolerance > 0) {
            
            try(
                url %>%
                    getURL(header = FALSE,
                           .opts = curlOptions(referer = "nhl.com",
                                               verbose = FALSE,
                                               followLocation = TRUE,
                                               useragent = agents[sample(1:length(agents), 1)]
                           )
                    )
            ) ->
                raw_text
            
            try_tolerance <- try_tolerance - 1
            
        }
        
        html <- read_html(raw_text)
        
        outer_text <- html_text(html_nodes(html, ".border"))
        inner_text <- html_text(html_nodes(html, ".bborder"))
        
        shifts_list <- list(outer_text, inner_text)
        
        return(shifts_list)
        
    } else if(tolower(source) == "json") {
        
        year <- substr(season, 0, 4)
        
        url <- paste("http://www.nhl.com/stats/rest/shiftcharts?cayenneExp=gameId=",
                     as.character(year),
                     "0",
                     as.character(game_id),
                     sep = ""
        )
        
        raw_text <- NULL
        json_check <- NULL
        
        while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
            
            try(
                url %>%
                    getURL(header = FALSE,
                           .opts = curlOptions(referer = "nhl.com",
                                               verbose = FALSE,
                                               followLocation = TRUE,
                                               useragent = agents[sample(1:length(agents), 1)]
                           )
                    )
            ) ->
                raw_text
            
            json_check <- try(fromJSON(raw_text), silent = TRUE)
            
            try_tolerance <- try_tolerance - 1
            
        }
        
        raw_json <- try(fromJSON(raw_text), silent = TRUE)
        
        if(class(raw_json) == "try-error") {raw_json <- NULL}
        
        return(raw_json)
        
    }
    
}

# Get Roster
ds.get_roster <- function(season, game_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
    
    ## Description
    # get_roster() imports the Roster page corresponding to a given year and game ID and returns a character vector
    
    url <- paste("http://www.nhl.com/scores/htmlreports/",
                 as.character(season),
                 "/RO0",
                 as.character(game_id),
                 ".HTM",
                 sep = ""
    )
    
    raw_text <- NULL
    
    while(class(raw_text) != "character" & try_tolerance > 0) {
        
        try(
            url %>%
                getURL(header = FALSE,
                       .opts = curlOptions(referer = "nhl.com",
                                           verbose = FALSE,
                                           followLocation = TRUE,
                                           useragent = agents[sample(1:length(agents), 1)]
                       )
                )
        ) ->
            raw_text
        
        try_tolerance <- try_tolerance - 1
        
    }
    
    html <- read_html(raw_text)
    
    all <- html_nodes(html, "tr")
    full_text <- html_text(all)
    
    return(full_text)
    
}

# Get Highlights
ds.get_highlights <- function(season, game_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
    
    ## Description
    # get_highlights() imports the highlights page corresponding to a given year and game ID and returns a JSON list object
    
    url <- paste("http://live.nhle.com/GameData/",
                 as.character(season),
                 "/",
                 substr(as.character(season), 0, 4),
                 "0",
                 as.character(game_id),
                 "/gc/gcgm.jsonp",
                 sep = ""
    )
    
    raw_text <- NULL
    json_check <- NULL
    
    while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
        
        try(
            url %>%
                getURL(header = FALSE,
                       .opts = curlOptions(referer = "nhl.com",
                                           verbose = FALSE,
                                           followLocation = TRUE,
                                           useragent = agents[sample(1:length(agents), 1)]
                       )
                )
        ) ->
            raw_text
        
        clean_text <- gsub("^.+?\\(\\{", "\\{", raw_text)
        json_check <- try(fromJSON(clean_text), silent = TRUE)
        
        try_tolerance <- try_tolerance - 1
        
    }
    
    clean_text <- gsub("^.+?\\(\\{", "\\{", raw_text)
    
    raw_json <- try(fromJSON(clean_text), silent = TRUE)
    
    if(class(raw_json) == "try-error") {raw_json <- NULL}
    
    return(raw_json)
    
}

# Get Coordinates
ds.get_coordinates <- function(season, game_id, source, date, away_team, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
    
    ## Description
    # get_coordinates() imports the event coordinates corresponding to a given year and game ID and returns a list object
    
    if(tolower(source) == "espn") {
        
        day <- gsub("-", "", as.character(date))
        
        url <- paste("http://scores.espn.go.com/nhl/scoreboard?date=", 
                     day, 
                     sep = ""
        )
        
        raw_text <- NULL
        
        while(class(raw_text) != "character" & try_tolerance > 0) {
            
            try(
                url %>%
                    getURL(header = FALSE,
                           .opts = curlOptions(referer = "sports.espn.go.com",
                                               verbose = FALSE,
                                               followLocation = TRUE,
                                               useragent = agents[sample(1:length(agents), 1)]
                           )
                    )
            ) ->
                raw_text
            
            try_tolerance <- try_tolerance - 1
            
        }
        
        game_ids <- unique(unlist(regmatches(raw_text, gregexpr("gameId=[0-9]+", raw_text))))
        teams <- toupper(gsub("team/_/name/|>|</div>", "", unique(unlist(regmatches(raw_text, gregexpr("team/_/name/[a-zA-Z]+|>(Coyotes|Thrashers)</div>", raw_text))))))
        
        teams[which(teams == "PHX")] <- "ARI"
        teams[which(teams == "TB")] <- "T.B"
        teams[which(teams == "NJ")] <- "N.J"
        teams[which(teams == "SJ")] <- "S.J"
        teams[which(teams == "LA")] <- "L.A"
        teams[which(teams == "COYOTES")] <- "ARI"
        teams[which(teams == "THRASHERS")] <- "ATL"
        
        if (as.numeric(season) < 20110000) {teams[which(teams == "WPG")] <- "ATL"}
        
        matrix(unique(teams), 
               byrow = TRUE, 
               ncol = 2
        ) %>% 
            data.frame() ->
            team_mat
        
        cbind(game_ids, 
              team_mat
        ) %>% 
            data.frame() %>% 
            rename(awayteam = X1, 
                   hometeam = X2
            ) ->
            url_match
        
        game_url <- first(as.character(url_match$game_ids[which(as.character(url_match$awayteam) == as.character(away_team) | as.character(url_match$hometeam) == as.character(away_team))]))
        
        url <- paste("http://sports.espn.go.com/nhl/gamecast/data/masterFeed?lang=en&isAll=true&rand=0&", 
                     game_url, 
                     sep = ""
        )
        
        raw_text <- NULL
        
        while(class(raw_text) != "character" & try_tolerance > 0) {
            
            try(
                url %>%
                    getURL(header = FALSE,
                           .opts = curlOptions(referer = "sports.espn.go.com",
                                               verbose = FALSE,
                                               followLocation = TRUE,
                                               useragent = agents[sample(1:length(agents), 1)]
                           )
                    )
            ) ->
                raw_text
            
            try_tolerance <- try_tolerance - 1
            
        }
        
        events <- unlist(regmatches(raw_text, gregexpr("<Play.*?/Play>", raw_text)))
        
        if(length(events) > 0) {
            
            do.call(cbind, 
                    strsplit(events, "[\\[~]")
            ) %>%
                t() %>%
                data.frame() %>%
                select(5, 3, 4, 6, 7, 11) ->
                event_mat
            
            colnames(event_mat) <- c("event_code",
                                     "xcoord",
                                     "ycoord",
                                     "time",
                                     "period",
                                     "description"
            )
            
            event_mat$event_type <- ds.espn_codes$event[match(event_mat$event_code, ds.espn_codes$code)]
            event_mat$seconds <- 1200*(nabs(event_mat$period) - 1) + ds.seconds_from_ms(event_mat$time)
            
            return(event_mat)
            
        } else {return(NULL)}
        
    } else if(tolower(source) == "nhl") {
        
        year <- substr(season, 0, 4)
        
        url <- paste("https://statsapi.web.nhl.com/api/v1/game/",
                     as.character(year),
                     "0",
                     as.character(game_id),
                     "/feed/live?site=en_nhl",
                     sep = ""
        )
        
        raw_text <- NULL
        json_check <- NULL
        
        while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
            
            try(
                url %>%
                    getURL(header = FALSE,
                           .opts = curlOptions(referer = "nhl.com",
                                               verbose = FALSE,
                                               followLocation = TRUE,
                                               useragent = agents[sample(1:length(agents), 1)]
                           )
                    )
            ) ->
                raw_text
            
            json_check <- try(fromJSON(raw_text), silent = TRUE)
            
            try_tolerance <- try_tolerance - 1
            
        }
        
        raw_json <- try(fromJSON(raw_text), silent = TRUE)
        
        if(class(raw_json) == "try-error") {
            
            return(NULL)
            
        } else {
            
            event_mat <- dcapply(raw_json$liveData$plays$allPlays, 
                                 ds.parse_event, 
                                 "rbind", 
                                 cores = 1
            )
            
            event_mat$game_id <- na_if_null(nabs(raw_json$gameData$game$pk))
            
            return(event_mat)
            
        }
        
    }
    
}

# Get Team Profile
ds.get_team_profile <- function(team_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
    
    ## Description
    # get_team_profile() imports the team profile page corresponding to a given team ID and returns a JSON list object
    
    url <- paste("https://statsapi.web.nhl.com/api/v1/teams/",
                 as.character(team_id),
                 sep = ""
    )
    
    raw_text <- NULL
    json_check <- NULL
    
    while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
        
        try(
            url %>%
                getURL(header = FALSE,
                       .opts = curlOptions(referer = "nhl.com",
                                           verbose = FALSE,
                                           followLocation = TRUE,
                                           useragent = agents[sample(1:length(agents), 1)]
                       )
                )
        ) ->
            raw_text
        
        json_check <- try(fromJSON(raw_text))
        
        try_tolerance <- try_tolerance - 1
        
    }
    
    raw_json <- try(fromJSON(raw_text))
    
    if(class(raw_json) == "try-error") {raw_json <- NULL}
    
    return(raw_json)
    
}

# Get Player Profile
ds.get_player_profile <- function(player_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
    
    ## Description
    # get_player_profile() imports the player profile page corresponding to a given player ID and returns a JSON list object
    
    url <- paste("https://statsapi.web.nhl.com/api/v1/people/",
                 as.character(player_id),
                 sep = ""
    )
    
    raw_text <- NULL
    json_check <- NULL
    
    while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
        
        try(
            url %>%
                getURL(header = FALSE,
                       .opts = curlOptions(referer = "nhl.com",
                                           verbose = FALSE,
                                           followLocation = TRUE,
                                           useragent = agents[sample(1:length(agents), 1)]
                       )
                )
        ) ->
            raw_text
        
        json_check <- try(fromJSON(raw_text))
        
        try_tolerance <- try_tolerance - 1
        
    }
    
    raw_json <- try(fromJSON(raw_text))
    
    if(class(raw_json) == "try-error") {raw_json <- NULL}
    
    return(raw_json)
    
}

# Get Schedule
ds.get_schedule <- function(start, end, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
    
    ## Description
    # get_schedule() imports the schedule page corresponding to a given date range and returns a JSON list object
    
    url <- paste("https://statsapi.web.nhl.com/api/v1/schedule?startDate=",
                 as.character(start),
                 "&endDate=",
                 as.character(end),
                 sep = ""
    )
    
    raw_text <- NULL
    json_check <- NULL
    
    while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
        
        try(
            url %>%
                getURL(header = FALSE,
                       .opts = curlOptions(referer = "nhl.com",
                                           verbose = FALSE,
                                           followLocation = TRUE,
                                           useragent = agents[sample(1:length(agents), 1)]
                       )
                )
        ) ->
            raw_text
        
        json_check <- try(fromJSON(raw_text))
        
        try_tolerance <- try_tolerance - 1
        
    }
    
    raw_json <- try(fromJSON(raw_text))
    
    if(class(raw_json) == "try-error") {raw_json <- NULL}
    
    return(raw_json)
    
}

# Parse PBP Event
ds.parse_event <- function(x) {
    
    ## Description
    # parse_event() parses a single event from the PBP JSON object and returns a data frame
    
    x$players %>%
        sapply(function(p) as.character(p$player$id)) %>%
        unlist() %>%
        c(rep(NA, 
              times = (4 - length(x$players))
        )
        ) ->
        player_ids
    
    data.frame(game_date = NA,
               game_id = NA,
               season = NA,
               session = NA,
               event_id = na_if_null(nabs(x$about$eventIdx)),
               event_code = na_if_null(as.character(x$result$eventCode)),
               event_type = na_if_null(as.character(x$result$eventTypeId)),
               event_description = na_if_null(as.character(x$result$description)),
               event_detail = na_if_null(as.character(x$result$secondaryType)),
               datetime = na_if_null(as.character(parse_date_time(x$about$dateTime, "y-m-d.H:M:S."))),
               game_period = na_if_null(nabs(x$about$period)),
               period_time_elapsed = na_if_null(as.character(x$about$periodTime)),
               period_time_remaining = na_if_null(as.character(x$about$periodTimeRemaining)),
               event_team = na_if_null(as.character(x$team$id)),
               event_player_1 = na_if_null(player_ids[1]),
               event_player_2 = na_if_null(player_ids[2]),
               event_player_3 = na_if_null(player_ids[3]),
               event_player_4 = na_if_null(player_ids[4]),
               coords_x = na_if_null(x$coordinates$x),
               coords_y = na_if_null(x$coordinates$y),
               highlight_id = na_if_null(nabs(x$about$eventId))
    ) ->
        event_df
    
    return(event_df)
    
}

# Parse Highlight
ds.parse_highlight <- function(x) {
    
    ## Description
    # parse_highlight() parses a single highlight from the Highlights JSON object and returns a data frame
    
    data.frame(game_date = NA,
               game_id = NA,
               season = NA,
               session = NA,
               event_id = na_if_null(x$id),
               highlight_id = na_if_null(x$feeds[[1]]$neulionId),
               event_team_1 = na_if_null(x$t1),
               event_team_2 = na_if_null(x$t2),
               event_period = na_if_null(x$p),
               event_seconds = na_if_null(x$sip),
               event_type = na_if_null(x$type)
    ) ->
        highlight_df
    
    return(highlight_df)
    
}

# Parse Game
ds.parse_game <- function(x) {
    
    ## Description
    # parse_game() parses a single game from the Schedule >> Date JSON object and returns a data frame
    # parse_game() is an inner function for parse_date()
    
    data.frame(game_id = na_if_null(nabs(x$gamePk)),
               game_date = na_if_null(as.character(as.Date(x$gameDate))),
               season = na_if_null(as.character(x$season)),
               session = na_if_null(as.character(x$gameType)),
               game_status = na_if_null(as.character(x$status$detailedState)),
               away_team_id = na_if_null(nabs(x$teams$away$team$id)),
               home_team_id = na_if_null(nabs(x$teams$home$team$id)),
               game_venue = na_if_null(as.character(x$venue$name)),
               game_datetime = na_if_null(as.character(parse_date_time(x$gameDate, "y-m-d.H:M:S.")))
    ) ->
        game_df
    
    return(game_df)
    
}

# Parse Date
ds.parse_date <- function(x) {
    
    ## Description
    # parse_date() parses a single date from the Schedule JSON object and returns a data frame
    # parse_date() uses an inner function parse_game()
    
    date_df <- dcapply(x$games,
                       ds.parse_game,
                       "rbind",
                       cores = 1
    )
    
    return(date_df)
    
}

# Parse Player
ds.parse_player <- function(x) {
    
    ## Description
    # parse_player() parses a single player from the PBP JSON object and returns a data frame
    
    data.frame(player_id = x$person$id,
               player_name = x$person$fullName,
               player_number = x$jerseyNumber,
               position = x$position$code
    ) ->
        player_df
    
    return(player_df)
    
}

# Seconds from MS
ds.seconds_from_ms <- function(ms) {
    
    ## Description
    # seconds_from_ms() returns a numeric vector of representation in seconds of a given vector in M:S format
    
    strsplit(as.character(ms), ":") %>%
        unlist() %>%
        nabs() %>%
        matrix(ncol = 2,
               byrow = TRUE
        ) ->
        time_mat
    
    seconds <- 60*time_mat[, 1] + time_mat[, 2]
    
    return(seconds)
    
}

# Clean Nums
ds.clean.nums <- function(x) {
    
    ## Description
    # clean_nums() returns a list of player number identifiers for a given event description
    
    t <- gsub("#|ONGOAL - ", "", as.character(unlist(x)))
    t2 <- list(c(t, rep(NA, times = (3 - length(t)))))
    return(t2)
    
}

#Parse Shifts
ds.parse_shifts <- function(player, venue, inner, outer) {
    
    ## Description
    # parse_shifts() returns a matrix containing shift information for a single player 
    
    if(tolower(venue) == "home") {
        
        index <- which(outer[-1] == player)
        
        inner[which(inner == "Shift #" | inner == "Présence #Shift #")[index]:(which(inner == "SHF" | inner == "PR/SHF")[index]-3)] %>%
            matrix(ncol = 6,
                   byrow = TRUE
            ) %>%
            data.frame() %>%
            mutate(num_first_last = player,
                   venue = venue
            ) %>%
            filter(X2 != "Per") %>%
            data.frame() ->
            shift_mat
        
    } else if(tolower(venue) == "away") {
        
        index <- which(outer[-1] == player)
        
        inner[which(inner == "Shift #" | inner == "Présence #Shift #")[index]:(which(inner == "SHF" | inner == "PR/SHF")[index]-3)] %>%
            matrix(ncol = 6,
                   byrow = TRUE
            ) %>%
            data.frame() %>%
            mutate(num_first_last = player,
                   venue = venue
            ) %>%
            filter(X2 != "Per") %>%
            data.frame() ->
            shift_mat
        
    }
    
    return(shift_mat)
    
}

# Parse Shift
ds.parse_shift <- function(x) {
    
    ## Description
    # parse_shift() parses a single shift from the Shifts JSON object and returns a data frame
    
    data.frame(game_date = NA,
               game_id = na_if_null(nabs(x$gameId)),
               season = NA,
               session = NA,
               shift_number = na_if_null(nabs(x$eventNumber)),
               shift_period = na_if_null(nabs(x$period)),
               shift_start = na_if_null(as.character(x$startTime)),
               shift_end = na_if_null(as.character(x$endTime)),
               shift_duration = na_if_null(as.character(x$duration)),
               team = na_if_null(as.character(x$teamAbbrev)),
               player_id = na_if_null(as.character(x$playerId)),
               player_name_fist = na_if_null(as.character(x$firstName)),
               player_name_last = na_if_null(as.character(x$lastName))
    ) ->
        shift_df
    
    return(shift_df)
    
}

# Is On
ds.is_on <- function(player, pbp, venue) {
    
    ## Description
    # is_on() returns a numeric vector indicating 1 if a given player is on ice during the event corresponding to the \
    # row index in the given PBP pbject
    
    regex <- paste(player, 
                   ",|",
                   player,
                   "$",
                   sep = ""
    )
    
    if(venue == "Home") {
        
        data.frame(cumsum(1*(grepl(regex, pbp$players_substituted) == TRUE & pbp$event_type == "ON" & pbp$event_team == pbp$home_team) - 
                              1*(grepl(regex, pbp$players_substituted) == TRUE & pbp$event_type == "OFF" & pbp$event_team == pbp$home_team)
        )
        ) ->
            is_on
        
    } else if(venue == "Away") {
        
        data.frame(cumsum(1*(grepl(regex, pbp$players_substituted) == TRUE & pbp$event_type == "ON" & pbp$event_team == pbp$away_team) - 
                              1*(grepl(regex, pbp$players_substituted) == TRUE & pbp$event_type == "OFF" & pbp$event_team == pbp$away_team)
        )
        ) ->
            is_on
        
    }
    
    colnames(is_on) <- player
    
    return(is_on)
    
}

# Is On
ds.find_goalie <- function(players, roster) {
    
    ## Description
    # find_goalie() returns a vector containing all goaltenders in a given player vector
    
    index <- which(players %in% roster$team_num[which(roster$player_position == "G")])
    goalie <- na_if_null(players[index])
    
    return(goalie)
    
}

# Fix Names
ds.fix_names <- function(name_vect) {
    
    ## Description
    # fix_names() returns a vector of player names corrected for multiple spelling variants
    
    name_vect[which(name_vect == "PK.SUBBAN" | name_vect == "P.K.SUBBAN")] <- "P.K..SUBBAN"
    name_vect[which(name_vect == "TJ.OSHIE" | name_vect == "T.J.OSHIE")] <- "T.J..OSHIE"
    name_vect[which(name_vect == "BJ.CROMBEEN" | name_vect == "B.J.CROMBEEN" | name_vect == "BRANDON.CROMBEEN")] <- "B.J..CROMBEEN"
    name_vect[which(name_vect == "ILJA.BRYZGALOV")] <- "ILYA.BRYZGALOV"
    name_vect[which(name_vect == "CAMERON.BARKER")] <- "CAM.BARKER"
    name_vect[which(name_vect == "CHRIS.VANDE VELDE")] <- "CHRIS.VANDEVELDE"
    name_vect[which(name_vect == "DANIEL.CARCILLO")] <- "DAN.CARCILLO"
    name_vect[which(name_vect == "DANIEL.CLEARY")] <- "DAN.CLEARY"
    name_vect[which(name_vect == "DANIEL.GIRARDI")] <- "DAN.GIRARDI"
    name_vect[which(name_vect == "DAVID JOHNNY.ODUYA")] <- "JOHNNY.ODUYA"
    name_vect[which(name_vect == "DAVID.BOLLAND")] <- "DAVE.BOLLAND"
    name_vect[which(name_vect == "DWAYNE.KING")] <- "DJ.KING"
    name_vect[which(name_vect == "EVGENII.DADONOV")] <- "EVGENY.DADONOV"
    name_vect[which(name_vect == "FREDDY.MODIN")] <- "FREDRIK.MODIN"
    name_vect[which(name_vect == "HARRISON.ZOLNIERCZYK")] <- "HARRY.ZOLNIERCZYK"
    name_vect[which(name_vect == "J P.DUMONT" | name_vect == "JEAN-PIERRE.DUMONT")] <- "J-P.DUMONT"
    name_vect[which(name_vect == "JEAN-FRANCOIS.JACQUES")] <- "J-F.JACQUES"
    name_vect[which(name_vect == "JONATHAN.AUDY-MARCHESSAULT")] <- "JONATHAN.MARCHESSAULT"
    name_vect[which(name_vect == "JOSHUA.HENNESSY")] <- "JOSH.HENNESSY"
    name_vect[which(name_vect == "KRISTOPHER.LETANG")] <- "KRIS.LETANG"
    name_vect[which(name_vect == "KRYSTOFER.BARCH")] <- "KRYS.BARCH"
    name_vect[which(name_vect == "MARTIN.ST LOUIS")] <- "MARTIN.ST. LOUIS"
    name_vect[which(name_vect == "MATTHEW.CARLE")] <- "MATT.CARLE"
    name_vect[which(name_vect == "MATTHEW.DUMBA")] <- "MATT.DUMBA"
    name_vect[which(name_vect == "JOSEPH.CORVO")] <- "JOE.CORVO"
    name_vect[which(name_vect == "TOBY.ENSTROM")] <- "TOBIAS.ENSTROM"
    name_vect[which(name_vect == "MICHAEL.SANTORELLI")] <- "MIKE.SANTORELLI"
    name_vect[which(name_vect == "MICHAEL.CAMMALLERI")] <- "MIKE.CAMMALLERI"
    name_vect[which(name_vect == "MICHAEL.FERLAND")] <- "MICHEAL.FERLAND"
    name_vect[which(name_vect == "PIERRE.PARENTEAU" | name_vect == "PIERRE-ALEX.PARENTEAU" | name_vect == "PA.PARENTEAU" | name_vect == "P.A.PARENTEAU" | name_vect == "P-A.PARENTEAU")] <- "P.A..PARENTEAU"
    name_vect <- gsub("ALEXANDER.|ALEXANDRE.", "ALEX.", name_vect)
    name_vect <- gsub("CHRISTOPHER.", "CHRIS.", name_vect)
    name_vect[which(name_vect == "NICOLAS.PETAN")] <- "NIC.PETAN"
    name_vect[which(name_vect == "NIKOLAI.KULEMIN")] <- "NIKOLAY.KULEMIN"
    name_vect[which(name_vect == "MATTHEW.BENNING")] <- "MATT.BENNING"
    name_vect[which(name_vect == "JAMES.HOWARD")] <- "JIMMY.HOWARD"
    name_vect[which(name_vect == "EMMANUEL.FERNANDEZ")] <- "MANNY.FERNANDEZ"
    name_vect[which(name_vect == "EMMANUEL.LEGACE")] <- "MANNY.LEGACE"
    name_vect[which(name_vect == "SIMEON.VARLAMOV")] <- "SEMYON.VARLAMOV"
    name_vect[which(name_vect == "MAXIME.TALBOT")] <- "MAX.TALBOT"
    name_vect[which(name_vect == "MITCHELL.MARNER")] <- "MITCH.MARNER"
    name_vect[which(name_vect == "ANDREW.MILLER")] <- "DREW.MILLER"
    name_vect[which(name_vect == "EDWARD.PURCELL")] <- "TEDDY.PURCELL"
    name_vect[which(name_vect == "NICKLAS.GROSSMAN")] <- "NICKLAS.GROSSMANN"
    
    return(name_vect)
    
}


## General Functions
# Who
ds.who <- function(player_id) {
    
    ## Description
    # who() searches a given player ID and returns the player's full name
    
    player <- ds.get_player_profile(player_id)
    
    full_name <- player$people[[1]]$fullName
    
    return(full_name)
    
}

# Scrape Team Profile
ds.scrape_team_profile <- function(team_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
    
    ## Description
    # scrape_team_profile() collects and parses the data for a team corresponsing to a given team ID
    # A data frame is returned
    
    team_id_ <- nabs(team_id)
    
    team <- ds.get_team_profile(team_id_, try_tolerance, agents)
    
    data.frame(team_id = na_if_null(nabs(team$teams[[1]]$id)),
               team_name = na_if_null(team$teams[[1]]$name),
               team_alias = na_if_null(team$teams[[1]]$abbreviation),
               team_venue = na_if_null(team$teams[[1]]$venue$name),
               team_location = na_if_null(team$teams[[1]]$locationName),
               team_city = na_if_null(team$teams[[1]]$venue$city),
               team_division_id = na_if_null(nabs(team$teams[[1]]$division$id)),
               team_division_name = na_if_null(team$teams[[1]]$division$name),
               team_conference_id = na_if_null(nabs(team$teams[[1]]$conference$id)),
               team_conference_name = na_if_null(team$teams[[1]]$conference$name),
               franchise_id = na_if_null(nabs(team$teams[[1]]$franchiseId)),
               is_active = na_if_null(as.logical(team$teams[[1]]$active))
    ) ->
        team_df
    
    return(team_df)
    
}

# Scrape Player Profile
ds.scrape_player_profile <- function(player_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
    
    ## Description
    # scrape_player_profile() collects and parses the data for a player corresponsing to a given player ID
    # A data frame is returned
    
    player_id_ <- nabs(player_id)
    
    player <- ds.get_player_profile(player_id_, try_tolerance, agents)
    
    data.frame(player_id = na_if_null(nabs(player$people[[1]]$id)),
               player_name_first = na_if_null(as.character(player$people[[1]]$firstName)),
               player_name_last = na_if_null(as.character(player$people[[1]]$lastName)),
               player_name_full = na_if_null(as.character(player$people[[1]]$fullName)),
               player_jerseynum = na_if_null(nabs(player$people[[1]]$primaryNumber)),
               player_position = na_if_null(as.character(player$people[[1]]$primaryPosition$code)),
               player_birth_date = na_if_null(as.character(as.Date(player$people[[1]]$birthDate))),
               player_birth_city = na_if_null(as.character(player$people[[1]]$birthCity)),
               player_birth_country = na_if_null(as.character(player$people[[1]]$birthCountry)),
               player_nationality = na_if_null(as.character(player$people[[1]]$nationality)),
               player_height = na_if_null(as.character(player$people[[1]]$height)),
               player_weight = na_if_null(nabs(player$people[[1]]$weight)),
               player_handedness = na_if_null(as.character(player$people[[1]]$shootsCatches)),
               is_active = na_if_null(as.logical(player$people[[1]]$active)),
               is_rookie = na_if_null(as.logical(player$people[[1]]$rookie))
    ) ->
        player_df
    
    return(player_df)
    
}

# Scrape Schedule
ds.scrape_schedule <- function(start, end, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
    
    ## Description
    # scrape_schedule() collects and parses the schedule data for a range corresponsing to a given start and end date
    # A data frame is returned
    
    start_ <- as.character(start); end_ <- as.character(end)
    
    sched <- ds.get_schedule(start_, end_, try_tolerance, agents)
    
    sched_df <- dcapply(sched$dates,
                        ds.parse_date, 
                        "rbind", 
                        cores = 1
    )
    
    return(sched_df)
    
}

# Scrape Game
ds.scrape_game <- function(season, game_id, try_tolerance = 3, agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
    
    ## Description
    # scrape_game() collects and parses the data for a game corresponsing to a given season and game ID
    # A list object containing c([[1]] = PBP, [[2]] = Shifts, [[3]] = Highlights) is returned
    
    season_ <- as.character(season); game_id_ <- as.character(game_id)
    
    pbp <- ds.get_pbp(season_, game_id_, try_tolerance, agents)
    home_shifts <- ds.get_shifts(season_, game_id_, venue = "home", source = "htm", try_tolerance, agents)
    away_shifts <- ds.get_shifts(season_, game_id_, venue = "away", source = "htm", try_tolerance, agents)
    roster <- ds.get_roster(season, game_id_, try_tolerance, agents)
    highlights <- ds.get_highlights(season_, game_id_, try_tolerance, agents)
    
    pbp_full <- pbp[[1]]
    pbp_body <- pbp[[2]]
    
    home_shifts_outer <- home_shifts[[1]]
    home_shifts_inner <- home_shifts[[2]]
    away_shifts_outer <- away_shifts[[1]]
    away_shifts_inner <- away_shifts[[2]]
    
    highlight_df <- dcapply(highlights$video$events,
                            ds.parse_highlight,
                            "rbind",
                            cores = 1
    )
    
    matrix(pbp_body, 
           byrow = TRUE, 
           ncol = 8
    ) %>% 
        data.frame() %>% 
        filter(X2 != "Per") ->
        pbp_raw
    
    highlight_df <- dcapply(highlights$video$events,
                            ds.parse_highlight,
                            "rbind",
                            cores = 1
    )
    
    if(!is.null(pbp_raw) & nrow(pbp_raw) > 0) {
        
        gsub("^[a-zA-Z]*, ", "", pbp_full[grep("^[a-zA-Z]*, ", pbp_full)]) %>% 
            as.Date(format = "%B %d, %Y") %>% 
            first() %>% 
            as.character() ->
            game_date_
        
        game_id_unique <- paste(substr(season_, 0, 4), 
                                "0", 
                                as.character(game_id_),
                                sep = ""
        )
        
        session_ <- ifelse(nabs(game_id_) > 30000,
                           "P",
                           "R"
        )
        
        home_team_ <- gsub(" On Ice", "", pbp_body[8])
        away_team_ <- gsub(" On Ice", "", pbp_body[7])
        
        # Removing this
        #home_team_[which(home_team_ == "PHX")] <- "ARI"; away_team_[which(away_team_ == "PHX")] <- "ARI"
        
        coordinates_df <- NULL#ds.get_coordinates(season_, game_id_, source = "espn", date = game_date_, away_team = away_team_, try_tolerance, agents)
        
        if(!is.null(coordinates_df)) {
            
            coordinates_df %>%
                filter(nabs(period) < 5,
                       event_type == "GOAL"
                ) %>%
                group_by(seconds) %>%
                summarise(dupes = n()) %>%
                filter(dupes > 1) %>%
                data.frame() ->
                dupe_check
            
        } else {dupe_check <- data.frame()}
        
        if(is.null(coordinates_df) == TRUE | nrow(dupe_check) > 0) {
            
            coordinates_df <- ds.get_coordinates(season_, game_id_, source = "nhl", date = game_date_, away_team = away_team_, try_tolerance, agents)
            
            coordinates_df %>%
                rename(time = period_time_elapsed,
                       xcoord = coords_x,
                       ycoord = coords_y,
                       period = game_period,
                       description = event_description
                ) %>%
                mutate(event_code = NA,
                       seconds = 1200*(nabs(period) - 1) + ds.seconds_from_ms(time),
                       event_type = as.character(event_type)
                ) %>%
                select(event_code,
                       xcoord,
                       ycoord,
                       time,
                       period,
                       description,
                       event_type,
                       seconds
                ) %>%
                data.frame() ->
                coordinates_df
            
            coordinates_df$event_type[which(coordinates_df$event_type == "MISSED_SHOT")] <- "MISS"
            coordinates_df$event_type[which(coordinates_df$event_type == "BLOCKED_SHOT")] <- "BLOCK"
            coordinates_df$event_type[which(coordinates_df$event_type == "FACEOFF")] <- "FAC"
            coordinates_df$event_type[which(coordinates_df$event_type == "GIVEAWAY")] <- "GIVE"
            coordinates_df$event_type[which(coordinates_df$event_type == "TAKEAWAY")] <- "TAKE"
            coordinates_df$event_type[which(coordinates_df$event_type == "PENALTY")] <- "PENL"
            
            coordinates_df %>%
                filter(nabs(period) < 5,
                       event_type == "GOAL"
                ) %>%
                group_by(seconds) %>%
                summarise(dupes = n()) %>%
                filter(dupes > 1) %>%
                data.frame() ->
                dupe_check
            
            if(nrow(dupe_check) > 0) {coordinates_df <- NULL}
            
        }
        
        pbp_raw %>%
            filter(X4 != "",
                   X2 != ""
            ) %>%
            mutate(game_date = game_date_,
                   game_id = game_id_unique,
                   season = as.character(season_),
                   session = session_,
                   home_team = home_team_,
                   away_team = away_team_,
                   time_elapsed = regmatches(X4, regexpr("[0-9]+:[0-9]{2}", X4)),
                   game_seconds = 1200*(nabs(X2) - 1) + ds.seconds_from_ms(time_elapsed),
                   event_team = unlist(lapply(regmatches(as.character(X6), gregexpr(paste("(^", paste(ds.team_list, collapse = "|^"), ")", sep = ""), as.character(X6))), na_if_null)),
                   event_player_1 = unlist(lapply(regmatches(as.character(X6), gregexpr("#[0-9]+|ONGOAL - [0-9]+", as.character(X6))), ds.clean.nums))[seq(1, 3*length(X6), 3)],
                   event_player_2 = unlist(lapply(regmatches(as.character(X6), gregexpr("#[0-9]+|ONGOAL - [0-9]+", as.character(X6))), ds.clean.nums))[seq(2, 3*length(X6), 3)],
                   event_player_3 = unlist(lapply(regmatches(as.character(X6), gregexpr("#[0-9]+|ONGOAL - [0-9]+", as.character(X6))), ds.clean.nums))[seq(3, 3*length(X6), 3)],
                   event_zone = gsub(". [zZ]one", "", unlist(lapply(regmatches(as.character(X6), gregexpr("[a-zA-Z]{3}. [zZ]one", as.character(X6))), na_if_null))),
                   event_detail = gsub(",|, |[A-Z]+ |#[0-9]+ |[A-Z]{2,}.", "", unlist(lapply(regmatches(as.character(X6), gregexpr(", [a-zA-Z|-]+,|[A-Z] .+[(].{4,}[)],|[A-Z] .+[(][a-zA-Z]{3,}[)],", as.character(X6))), na_if_null)))
            ) %>%
            rename(game_period = X2,
                   event_type = X5,
                   event_description = X6
            ) %>%
            select(game_period,
                   event_type,
                   event_description,
                   game_date:event_detail
            ) %>%
            data.frame() ->
            pbp_df
        
        bind_rows(
            pbp_df %>%
                filter(event_type == "FAC") %>%
                mutate(event_player_1 = paste(away_team, event_player_1, sep = ""),
                       event_player_2 = paste(home_team, event_player_2, sep = ""),
                       event_player_3 = NA
                ),
            
            pbp_df %>%
                filter(event_type %in% c("HIT", "BLOCK", "PENL")) %>%
                group_by(event_team) %>%
                mutate(event_player_1 = paste(first(event_team), event_player_1, sep = ""),
                       event_player_2 = paste(unique(c(home_team, away_team))[which(unique(c(home_team, away_team)) != first(event_team))], event_player_2, sep = ""),
                       event_player_3 = NA
                ),
            
            pbp_df %>%
                filter(event_type %in% c("SHOT", "MISS", "GIVE", "TAKE")) %>%
                group_by(event_team) %>%
                mutate(event_player_1 = paste(first(event_team), event_player_1, sep = ""),
                       event_player_2 = NA,
                       event_player_3 = NA
                ),
            
            pbp_df %>%
                filter(event_type %in% c("GOAL")) %>%
                group_by(event_team) %>%
                mutate(event_player_1 = paste(first(event_team), event_player_1, sep = ""),
                       event_player_2 = paste(first(event_team), event_player_2, sep = ""),
                       event_player_3 = paste(first(event_team), event_player_3, sep = "")
                ),
            
            pbp_df %>%
                filter(event_type %in% c("FAC", "HIT", "BLOCK", "PENL", "SHOT", "MISS", "GIVE", "TAKE", "GOAL") == FALSE) %>%
                data.frame()
        ) %>%
            mutate(event_player_1 = gsub(paste(paste(ds.team_list, collapse = "NA|"), "NA", sep = ""), NA, event_player_1),
                   event_player_2 = gsub(paste(paste(ds.team_list, collapse = "NA|"), "NA", sep = ""), NA, event_player_2),
                   event_player_3 = gsub(paste(paste(ds.team_list, collapse = "NA|"), "NA", sep = ""), NA, event_player_3)
            ) %>%
            data.frame() ->
            pbp_df
        
    } else {return(list(NULL, NULL, NULL, NULL, NULL))}
    
    if(!is.null(roster)) {
        
        regmatches(as.character(roster[1]), gregexpr("[0-9]+(\\\r\\\n|\\\n)[A-Z]+(\\\r\\\n|\\\n)[A-Z )(-]+(\\\r\\\n|\\\n)", as.character(roster[1]))) %>%
            unlist() %>%
            strsplit("(\\\r\\\n|\\\n)") %>%
            unlist() %>%
            matrix(ncol = 3,
                   byrow = TRUE
            ) %>%
            data.frame() %>%
            rename(player_number = X1,
                   player_position = X2,
                   player_name = X3
            ) ->
            pos_match
        
        pos_match$name_match <- gsub("[^A-Z]|\\([A-Z]+\\)", "", pos_match$player_name)
        
    }
    
    if(!is.null(home_shifts) & !is.null(away_shifts) & length(home_shifts_outer[-1]) > 0 & length(away_shifts_outer[-1]) > 0) {
        
        bind_rows(
            data.frame(team_name = home_shifts_outer[1],
                       team = home_team_,
                       venue = "Home",
                       num_first_last = home_shifts_outer[-1]
            ),
            
            data.frame(team_name = away_shifts_outer[1],
                       team = away_team_,
                       venue = "Away",
                       num_first_last = away_shifts_outer[-1]
            )
        ) %>%
            data.frame() %>%
            filter(grepl("[A-Z0-9]", num_first_last) == TRUE) %>%
            mutate(game_date = game_date_,
                   game_id = game_id_unique,
                   season = as.character(season_),
                   session = session_,
                   player_number = unlist(regmatches(as.character(num_first_last), gregexpr("^[0-9]+", as.character(num_first_last)))),
                   team_num = paste(team, player_number, sep = "")
            ) %>%
            data.frame() ->
            roster_df
        
        strsplit(gsub("^[0-9]+ ", 
                      "", 
                      roster_df$num_first_last
        ), 
        ", "
        ) %>%
            unlist() %>%
            as.character() %>%
            matrix(ncol = 2,
                   byrow = TRUE
            ) %>%
            data.frame() ->
            name_mat
        
        roster_df$first_name <- name_mat[, 2]
        roster_df$last_name <- name_mat[, 1]
        roster_df$player_name <- paste(roster_df$first_name, roster_df$last_name, sep = ".")
        roster_df$name_match <- gsub("[^A-Z]|\\([A-Z]+\\)", "", roster_df$player_name)
        roster_df$player_position <- pos_match$player_position[match(roster_df$name_match, pos_match$name_match)]
        
        bind_rows(
            do.call(rbind,
                    lapply(as.list(home_shifts_outer[-1]),
                           ds.parse_shifts,
                           venue = "Home",
                           outer = home_shifts_outer,
                           inner = home_shifts_inner
                    )
            ) %>%
                data.frame() %>%
                mutate(team = home_team_),
            
            do.call(rbind,
                    lapply(as.list(away_shifts_outer[-1]),
                           ds.parse_shifts,
                           venue = "Away",
                           outer = away_shifts_outer,
                           inner = away_shifts_inner
                    )
            ) %>%
                data.frame() %>%
                mutate(team = away_team_)
        ) %>%
            data.frame() %>%
            rename(shift_number = X1,
                   game_period = X2,
                   shift_start = X3,
                   shift_end = X4,
                   shift_duration = X5
            ) %>%
            select(shift_number:shift_duration,
                   num_first_last,
                   team,
                   venue
            ) %>%
            mutate(game_date = game_date_,
                   game_id = game_id_unique,
                   season = as.character(season_),
                   session = session_,
                   home_team = home_team_,
                   away_team = away_team_
            ) %>%
            data.frame() ->
            shifts_df
        
        shifts_df$player_name <- roster_df$player_name[match(shifts_df$num_first_last, roster_df$num_first_last)]
        shifts_df$game_period <- as.character(shifts_df$game_period)
        shifts_df$game_period[which(shifts_df$game_period == "OT")] <- "4"
        shifts_df$team_num <- paste(shifts_df$team, gsub("[^0-9]", "", shifts_df$num_first_last), sep = "")
        
        do.call(rbind,
                strsplit(as.character(shifts_df$shift_start), " / ")
        ) %>%
            data.frame() ->
            start_mat
        
        do.call(rbind,
                strsplit(as.character(shifts_df$shift_end), " / ")
        ) %>%
            data.frame() ->
            end_mat
        
        shifts_df$start_seconds <- 1200*(nabs(shifts_df$game_period) - 1) + ds.seconds_from_ms(start_mat[, 1])
        shifts_df$end_seconds <- 1200*(nabs(shifts_df$game_period) - 1) + ds.seconds_from_ms(end_mat[, 1])
        
        shifts_df$end_seconds[which(shifts_df$end_seconds < shifts_df$start_seconds)] <- shifts_df$start_seconds[which(shifts_df$end_seconds < shifts_df$start_seconds)] + ds.seconds_from_ms(shifts_df$shift_duration[which(shifts_df$end_seconds < shifts_df$start_seconds)])
        
        shifts_df %>%
            filter(ds.seconds_from_ms(shift_duration) + (start_seconds - 1200*(nabs(game_period) - 1)) <= 1200) %>%
            data.frame() ->
            shifts_df
        
    } else {
        
        shifts <- ds.get_shifts(season_, game_id_, venue = NULL, source = "json", try_tolerance, agents)
        
        shifts_df <- dcapply(shifts$data,
                             ds.parse_shift,
                             "rbind",
                             cores = 1
        )
        
        if(!is.null(shifts_df)) {
            
            shifts_df %>%
                mutate(game_date = game_date_,
                       season = as.character(season_),
                       session = session_,
                       home_team = home_team_,
                       away_team = away_team_
                ) %>%
                data.frame() ->
                shifts_df
            
            year <- substr(season, 0, 4)
            
            url <- paste("https://statsapi.web.nhl.com/api/v1/game/",
                         as.character(year),
                         "0",
                         as.character(game_id),
                         "/feed/live?site=en_nhl",
                         sep = ""
            )
            
            raw_text <- NULL
            json_check <- NULL
            
            while({class(raw_text) != "character" | class(json_check) != "list"} & try_tolerance > 0) {
                
                try(
                    url %>%
                        getURL(header = FALSE,
                               .opts = curlOptions(referer = "nhl.com",
                                                   verbose = FALSE,
                                                   followLocation = TRUE,
                                                   useragent = agents[sample(1:length(agents), 1)]
                               )
                        )
                ) ->
                    raw_text
                
                json_check <- try(fromJSON(raw_text), silent = TRUE)
                
                try_tolerance <- try_tolerance - 1
                
            }
            
            raw_json <- try(fromJSON(raw_text), silent = TRUE)
            
            if(class(raw_json) == "try-error") {raw_json <- NULL}
            
            home_roster <- raw_json$liveData$boxscore$teams$home
            away_roster <- raw_json$liveData$boxscore$teams$away
            
            home_player_data <- dcapply(home_roster$players,
                                        ds.parse_player,
                                        "rbind",
                                        cores = 1
            )
            
            away_player_data <- dcapply(away_roster$players,
                                        ds.parse_player,
                                        "rbind",
                                        cores = 1
            )
            
            bind_rows(
                home_player_data %>%
                    mutate(team = home_roster$team$abbreviation,
                           team_name = toupper(home_roster$team$name),
                           venue = "Home"
                    ),
                
                away_player_data %>%
                    mutate(team = away_roster$team$abbreviation,
                           team_name = toupper(away_roster$team$name),
                           venue = "Away"
                    )
            ) %>%
                data.frame() ->
                player_data
            
            player_data$team_num <- paste(player_data$team, player_data$player_number, sep = "")
            
            name_match <- dcapply(player_data$player_id,
                                  ds.scrape_player_profile,
                                  "rbind",
                                  cores = 1
            )
            
            player_data %>%
                mutate(first_name = toupper(name_match$player_name_first[match(player_id, name_match$player_id)]),
                       last_name = toupper(name_match$player_name_last[match(player_id, name_match$player_id)]),
                       num_first_last = NA,
                       game_date = game_date_,
                       game_id = game_id_unique,
                       season = as.character(season_),
                       session = session_,
                       home_team = home_team_,
                       away_team = away_team_,
                       player_name = paste(first_name, last_name, sep = "."),
                       name_match = gsub("[^A-Z]|\\([A-Z]+\\)", "", player_name),
                       player_position = substr(position, 0, 1)
                ) %>%
                select(team_name,
                       team,
                       venue,
                       num_first_last,
                       game_date,
                       game_id,
                       season,
                       session,
                       player_number,
                       team_num,
                       first_name,
                       last_name,
                       player_name,
                       name_match,
                       player_position
                ) %>%
                data.frame() ->
                roster_df
            
            shifts_df %>%
                rename(game_period = shift_period) %>%
                mutate(num_first_last = NA,
                       venue = ifelse(team == home_team_,
                                      "Home",
                                      "Away"
                       ),
                       game_date = game_date_,
                       game_id = game_id_unique,
                       season = as.character(season_),
                       session = session_,
                       home_team = home_team_,
                       away_team = away_team_,
                       player_name = player_data$player_name[match(player_id, player_data$player_id)],
                       team_num = player_data$team_num[match(player_id, player_data$player_id)],
                       start_seconds = 1200*(nabs(game_period) - 1) + ds.seconds_from_ms(shift_start),
                       end_seconds = 1200*(nabs(game_period) - 1) + ds.seconds_from_ms(shift_end)
                ) %>%
                select(shift_number,
                       game_period,
                       shift_start,
                       shift_end,
                       shift_duration,
                       num_first_last,
                       team,
                       venue,
                       game_date,
                       game_id,
                       season,
                       session,
                       home_team,
                       away_team,
                       player_name,
                       team_num,
                       start_seconds,
                       end_seconds
                ) %>%
                data.frame() ->
                shifts_df
            
            shifts_df$end_seconds[which(shifts_df$end_seconds < shifts_df$start_seconds)] <- shifts_df$start_seconds[which(shifts_df$end_seconds < shifts_df$start_seconds)] + ds.seconds_from_ms(shifts_df$shift_duration[which(shifts_df$end_seconds < shifts_df$start_seconds)])
            
            shifts_df %>%
                filter(ds.seconds_from_ms(shift_duration) + (start_seconds - 1200*(nabs(game_period) - 1)) <= 1200) %>%
                data.frame() ->
                shifts_df
            
        } else {return(list(NULL, NULL, NULL, NULL, NULL))}
        
    }
    
    if(!is.null(highlight_df)) {
        
        highlight_df %>%
            filter(nabs(event_period) < 5,
                   event_type == 505
            ) %>%
            group_by(event_id) %>%
            summarise(dupes = n()) %>%
            filter(dupes > 1) %>%
            data.frame() ->
            dupe_check
        
        if(nrow(dupe_check) > 0) {
            
            highlight_df <- NULL
            
        } else {
            
            highlight_df %>%
                mutate(game_date = game_date_,
                       game_id = game_id_unique,
                       season = as.character(season_),
                       session = session_,
                       home_team = home_team_,
                       away_team = away_team_
                ) %>%
                data.frame() ->
                highlight_df
            
        }
        
    }
    
    if(!is.null(coordinates_df)) {
        
        coordinates_df %>%
            mutate(game_date = game_date_,
                   game_id = game_id_unique,
                   season = as.character(season_),
                   session = session_,
                   home_team = home_team_,
                   away_team = away_team_
            ) %>%
            data.frame() ->
            coordinates_df
        
    }
    
    game_list <- list(pbp_df,
                      roster_df,
                      shifts_df,
                      highlight_df,
                      coordinates_df
    )
    
    return(game_list)
    
}

# Compile Games
ds.compile_games <- function(games, season, pause = 1, try_tolerance = 3, 
                             agents = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36") {
    
    ## Description
    # compile_games() collects, parses and compiles all game data corresponding to a given vector of game IDs and season
    # A list object containing c([[1]] = PBP, [[2]] = Roster, [[3]] = Shifts) is returned
    
    foreach(g = as.character(games)) %do% {
        
        cat(g,
            "...",
            "\n",
            sep = ""
        )
        
        inner <- ds.scrape_game(season, g, try_tolerance, agents)
        Sys.sleep(pause)
        
        return(inner)
        
    } -> nested_games
    
    unpacked <- do.call(Map, c(rbind, nested_games))
    
    pbp <- unpacked[[1]]
    roster <- unpacked[[2]]
    shifts <- unpacked[[3]]
    highlights <- unpacked[[4]]
    coords <- unpacked[[5]]
    
    roster$player_name <- ds.fix_names(roster$player_name)
    shifts$player_name <- ds.fix_names(shifts$player_name)
    
    bind_rows(
        shifts %>%
            filter(!is.na(shift_duration)) %>%
            group_by(game_id,
                     game_date,
                     season,
                     session,
                     home_team,
                     away_team,
                     team,
                     game_period,
                     start_seconds
            ) %>%
            rename(game_seconds = start_seconds, event_team = team) %>%
            summarise(event_type = "ON",
                      players_substituted = paste(unique(team_num), collapse = ", ")
            ) %>%
            data.frame(),
        
        shifts %>%
            filter(!is.na(shift_duration)) %>%
            group_by(game_id,
                     game_date,
                     season,
                     session,
                     home_team,
                     away_team,
                     team,
                     game_period,
                     end_seconds
            ) %>%
            rename(game_seconds = end_seconds, event_team = team) %>%
            summarise(event_type = "OFF",
                      players_substituted = paste(unique(team_num), collapse = ", ")
            ) %>%
            data.frame()
    ) -> shift_summary
    
    if(!is.null(highlights)) {
        
        highlights$event_match <- ifelse(highlights$event_type == 505,
                                         "GOAL",
                                         "SHOT"
        )
        
        left_join(pbp,
                  highlights %>%
                      mutate(game_seconds = 1200*(nabs(event_period) - 1) + nabs(event_seconds)) %>%
                      rename(highlight_code = highlight_id) %>%
                      select(game_id, game_seconds, event_match, highlight_code) %>%
                      data.frame(),
                  by = c("game_id" = "game_id", "game_seconds" = "game_seconds", "event_type" = "event_match")
        ) %>%
            data.frame() ->
            new_pbp
        
    } else {
        
        pbp %>%
            mutate(highlight_code = NA
            ) %>%
            data.frame() ->
            new_pbp
        
    }
    
    if(!is.null(coords)) {
        
        left_join(new_pbp,
                  coords %>%
                      rename(coords_x = xcoord,
                             coords_y = ycoord
                      ) %>%
                      select(game_id, seconds, event_type, coords_x, coords_y) %>%
                      data.frame(),
                  by = c("game_id" = "game_id", "game_seconds" = "seconds", "event_type" = "event_type")
        ) %>%
            data.frame() ->
            new_pbp
        
    } else {
        
        new_pbp %>%
            mutate(coords_x = NA,
                   coords_y = NA
            ) %>%
            data.frame() ->
            new_pbp
        
    }
    
    new_pbp %>%
        group_by(game_id, game_seconds, event_description) %>%
        slice(1) %>%
        data.frame() ->
        new_pbp
    
    new_pbp$game_period <- nabs(new_pbp$game_period)
    shift_summary$game_period <- nabs(shift_summary$game_period)
    
    bind_rows(new_pbp,
              shift_summary
    ) %>%
        mutate(priority = 1*(event_type %in% c("TAKE", "GIVE", "MISS", "HIT", "SHOT", "BLOCK")) +
                   2*(event_type == "GOAL") +
                   3*(event_type == "STOP") +
                   4*(event_type == "PENL") +
                   5*(event_type == "OFF") +
                   6*(event_type == "ON") +
                   7*(event_type == "FAC")
        ) %>% 
        group_by(game_id) %>%
        arrange(game_period,
                game_seconds,
                priority
        ) %>%
        mutate(event_index = cumsum(!is.na(game_id))) %>%
        data.frame() ->
        new_pbp
    
    home_on_mat <- dcapply(as.list(unique(shifts$team_num)),
                           ds.is_on,
                           "cbind",
                           cores = 1,
                           pbp = arrange(new_pbp,
                                         game_id,
                                         event_index
                           ),
                           venue = "Home"
    )
    
    away_on_mat <- dcapply(as.list(unique(shifts$team_num)),
                           ds.is_on,
                           "cbind",
                           cores = 1,
                           pbp = arrange(new_pbp,
                                         game_id,
                                         event_index
                           ),
                           venue = "Away"
    )
    
    which(home_on_mat == 1, 
          arr.ind = TRUE
    ) %>%
        data.frame() %>%
        group_by(row) %>%
        summarise(home_on_1 = colnames(home_on_mat)[unique(col)[1]],
                  home_on_2 = colnames(home_on_mat)[unique(col)[2]],
                  home_on_3 = colnames(home_on_mat)[unique(col)[3]],
                  home_on_4 = colnames(home_on_mat)[unique(col)[4]],
                  home_on_5 = colnames(home_on_mat)[unique(col)[5]],
                  home_on_6 = colnames(home_on_mat)[unique(col)[6]]
        ) %>%
        data.frame() ->
        home_on_df
    
    which(away_on_mat == 1, 
          arr.ind = TRUE
    ) %>%
        data.frame() %>%
        group_by(row) %>%
        summarise(away_on_1 = colnames(away_on_mat)[unique(col)[1]],
                  away_on_2 = colnames(away_on_mat)[unique(col)[2]],
                  away_on_3 = colnames(away_on_mat)[unique(col)[3]],
                  away_on_4 = colnames(away_on_mat)[unique(col)[4]],
                  away_on_5 = colnames(away_on_mat)[unique(col)[5]],
                  away_on_6 = colnames(away_on_mat)[unique(col)[6]]
        ) %>%
        data.frame() ->
        away_on_df
    
    do.call(c,
            home_on_df[, -1] %>%
                split(1:nrow(home_on_df)) %>%
                lapply(ds.find_goalie,
                       roster
                )
    ) %>%
        as.character() ->
        home_goalie
    
    do.call(c,
            away_on_df[, -1] %>%
                split(1:nrow(away_on_df)) %>%
                lapply(ds.find_goalie,
                       roster
                )
    ) %>%
        as.character() ->
        away_goalie
    
    new_pbp %>%
        arrange(game_id,
                event_index
        ) %>%
        mutate(home_on_1 = NA,
               home_on_2 = NA,
               home_on_3 = NA,
               home_on_4 = NA,
               home_on_5 = NA,
               home_on_6 = NA,
               home_goalie = NA,
               away_on_1 = NA,
               away_on_2 = NA,
               away_on_3 = NA,
               away_on_4 = NA,
               away_on_5 = NA,
               away_on_6 = NA,
               away_goalie = NA
        ) ->
        full_pbp
    
    full_pbp$home_on_1[home_on_df$row] <- home_on_df$home_on_1
    full_pbp$home_on_2[home_on_df$row] <- home_on_df$home_on_2
    full_pbp$home_on_3[home_on_df$row] <- home_on_df$home_on_3
    full_pbp$home_on_4[home_on_df$row] <- home_on_df$home_on_4
    full_pbp$home_on_5[home_on_df$row] <- home_on_df$home_on_5
    full_pbp$home_on_6[home_on_df$row] <- home_on_df$home_on_6
    full_pbp$home_goalie[home_on_df$row] <- home_goalie
    
    full_pbp$away_on_1[away_on_df$row] <- away_on_df$away_on_1
    full_pbp$away_on_2[away_on_df$row] <- away_on_df$away_on_2
    full_pbp$away_on_3[away_on_df$row] <- away_on_df$away_on_3
    full_pbp$away_on_4[away_on_df$row] <- away_on_df$away_on_4
    full_pbp$away_on_5[away_on_df$row] <- away_on_df$away_on_5
    full_pbp$away_on_6[away_on_df$row] <- away_on_df$away_on_6
    full_pbp$away_goalie[away_on_df$row] <- away_goalie
    
    full_pbp$event_player_1 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$event_player_1, sep = "."), 
                                                        paste(roster$game_id, roster$team_num, sep = ".")
    )
    ]
    full_pbp$event_player_2 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$event_player_2, sep = "."), 
                                                        paste(roster$game_id, roster$team_num, sep = ".")
    )
    ]
    full_pbp$event_player_3 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$event_player_3, sep = "."), 
                                                        paste(roster$game_id, roster$team_num, sep = ".")
    )
    ]
    full_pbp$home_on_1 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$home_on_1, sep = "."), 
                                                   paste(roster$game_id, roster$team_num, sep = ".")
    )
    ]
    full_pbp$home_on_2 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$home_on_2, sep = "."), 
                                                   paste(roster$game_id, roster$team_num, sep = ".")
    )
    ]
    full_pbp$home_on_3 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$home_on_3, sep = "."), 
                                                   paste(roster$game_id, roster$team_num, sep = ".")
    )
    ]
    full_pbp$home_on_4 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$home_on_4, sep = "."), 
                                                   paste(roster$game_id, roster$team_num, sep = ".")
    )
    ]
    full_pbp$home_on_5 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$home_on_5, sep = "."), 
                                                   paste(roster$game_id, roster$team_num, sep = ".")
    )
    ]
    full_pbp$home_on_6 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$home_on_6, sep = "."), 
                                                   paste(roster$game_id, roster$team_num, sep = ".")
    )
    ]
    full_pbp$away_on_1 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$away_on_1, sep = "."), 
                                                   paste(roster$game_id, roster$team_num, sep = ".")
    )
    ]
    full_pbp$away_on_2 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$away_on_2, sep = "."), 
                                                   paste(roster$game_id, roster$team_num, sep = ".")
    )
    ]
    full_pbp$away_on_3 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$away_on_3, sep = "."), 
                                                   paste(roster$game_id, roster$team_num, sep = ".")
    )
    ]
    full_pbp$away_on_4 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$away_on_4, sep = "."), 
                                                   paste(roster$game_id, roster$team_num, sep = ".")
    )
    ]
    full_pbp$away_on_5 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$away_on_5, sep = "."), 
                                                   paste(roster$game_id, roster$team_num, sep = ".")
    )
    ]
    full_pbp$away_on_6 <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$away_on_6, sep = "."), 
                                                   paste(roster$game_id, roster$team_num, sep = ".")
    )
    ]
    full_pbp$home_goalie <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$home_goalie, sep = "."), 
                                                     paste(roster$game_id, roster$team_num, sep = ".")
    )
    ]
    full_pbp$away_goalie <- roster$player_name[match(paste(full_pbp$game_id, full_pbp$away_goalie, sep = "."), 
                                                     paste(roster$game_id, roster$team_num, sep = ".")
    )
    ]
    
    full_pbp %>%
        group_by(game_id) %>%
        arrange(event_index) %>%
        mutate(home_skaters = 6 - 1*(is.na(home_on_1) == TRUE) -
                   1*(is.na(home_on_2) == TRUE) -
                   1*(is.na(home_on_3) == TRUE) -
                   1*(is.na(home_on_4) == TRUE) -
                   1*(is.na(home_on_5) == TRUE) -
                   1*(is.na(home_on_6) == TRUE) -
                   1*(!is.na(home_goalie)),
               away_skaters = 6 - 1*(is.na(away_on_1) == TRUE) -
                   1*(is.na(away_on_2) == TRUE) -
                   1*(is.na(away_on_3) == TRUE) -
                   1*(is.na(away_on_4) == TRUE) -
                   1*(is.na(away_on_5) == TRUE) -
                   1*(is.na(away_on_6) == TRUE) -
                   1*(!is.na(away_goalie)),
               home_score = cumsum(event_type == "GOAL" & event_team == home_team) - 1*(event_type == "GOAL" & event_team == home_team),
               away_score = cumsum(event_type == "GOAL" & event_team == away_team) - 1*(event_type == "GOAL" & event_team == away_team),
               event_length = nabs(lead(game_seconds, 1) - game_seconds)
        ) %>%
        ungroup() %>%
        mutate(game_strength_state = paste(ifelse(is.na(home_goalie) == TRUE,
                                                  "E",
                                                  home_skaters
        ),
        ifelse(is.na(away_goalie) == TRUE,
               "E",
               away_skaters
        ),
        sep = "v"
        ),
        game_score_state = paste(home_score,
                                 away_score,
                                 sep = "v"
        )
        ) %>%
        select(one_of(ds.pbp_colnames)) %>%
        arrange(game_id,
                event_index
        ) %>%
        data.frame() ->
        full_pbp
    
    full_pbp$event_team[which(full_pbp$event_team == "PHX")] <- "ARI"
    
    new_game_list <- list(full_pbp,
                          roster,
                          shifts
    )
    
    return(new_game_list)
    
}



###############################################################################################################
###############################################################################################################



### STATS ###
# Last edit: Manny (2017-05-06)


## Description
# Stats contains all functions and tools related to compiling stats from raw data for Corsica 
# Dependencies: dplyr, Kmisc, doMC, user_functions


## Dependencies
#require(dplyr); require(Kmisc); require(doMC); require(glmnet); require(survival)


## Objects
c("G" = 0.75,
  "A1" = 0.7,
  "A2" = 0.55,
  "iSF" = 0.075,
  "iBLK" = 0.05,
  "iPENT" = -0.15,
  "iPEND" = 0.15,
  "iFOW" = 0.01,
  "iFOL" = -0.01,
  "CF" = 0.05,
  "CA" = -0.05,
  "GF" = 0.15,
  "GA" = -0.15
) ->
    st.game_score_weights

c("SHOT", 
  "GOAL"
) ->
    st.shot_events

c("SHOT", 
  "GOAL",
  "MISS"
) ->
    st.fenwick_events

c("SHOT", 
  "GOAL",
  "MISS",
  "BLOCK"
) ->
    st.corsi_events

c("3v3",
  "5v5", 
  "4v4", 
  "5v4", 
  "4v5", 
  "5v3", 
  "3v5", 
  "4v3", 
  "3v4", 
  "5vE", 
  "Ev5", 
  "4vE", 
  "Ev4", 
  "3vE", 
  "Ev3"
) %>%
    as.factor() ->
    st.strength_states

## Meta Functions
# Combo Code
st.combo_code <- function(p1, p2, p3) {
    
    ## Description
    # combo_code() returns the unique code produced from a list of up to three players
    
    sorted <- sort(c(p1, p2, p3))
    
    p1_abs <- sorted[1]
    p2_abs <- sorted[2]
    p3_abs <- sorted[3]
    
    code <- paste(p1_abs,
                  p2_abs,
                  p3_abs,
                  sep = "-"
    )
    
    return(code)
    
}

# Game Score
st.game_score <- function(x) {
    
    ## Description
    # game_score() returns the game score obtained from a given vector of statistics
    # The vector x is expected to contain the necessary stats in proper order
    
    return(sum(st.game_score_weights*x))
    
}

# Distance from Net
st.distance_from_net <- function(x, y) {
    
    ## Description
    # distance_from_net() returns the distance from the nearest net in feet of a location corresponding \
    # to a given set of coordinates
    
    return(sqrt((89 - abs(nabs(x)))^2 + nabs(y)^2))
    
}

# Angle from Centre
st.angle_from_centre <- function(x, y) {
    
    ## Description
    # angle_from_centre() returns the angle from the central line perpendicular to the goal line in \
    # degrees of a location corresponsing to a given set of coordinates
    
    return(abs(atan(nabs(y)/(89 - abs(nabs(x))))*(180/pi)))
    
}

# Which Zone
st.which_zone <- function(x) {
    
    ## Description
    # which_zone() returns the absolute zone of a location corresponding to a given x-coordinate
    
    factor_level <- as.factor(1*(x <= -25) +
                                  2*(abs(nabs(x)) < 25) +
                                  3*(x >= 25)
    )
    
    levels(factor_level) <- c("L",
                              "N",
                              "R"
    )
    
    return(as.character(factor_level))
    
}

st.which_circle <- function(x, y) {
    
    ## Description
    # which_circle() returns the faceoff circle number nearest to a location corresponding to a given \
    # set of coordinates
    
    circle <- 1*(nabs(x) <= -25 & nabs(y) > 0) +
        2*(nabs(x) <= -25 & nabs(y) < 0) +
        3*(nabs(x) < 0 & nabs(x) > 25 & nabs(y) > 0) +
        4*(nabs(x) < 0 & nabs(x) > 25 & nabs(y) < 0) +
        5*(abs(nabs(x)) < 5 & abs(nabs(y)) < 5) +
        6*(nabs(x) > 0 & nabs(x) < 25 & nabs(y) > 0) +
        7*(nabs(x) > 0 & nabs(x) < 25 & nabs(y) < 0) +
        8*(nabs(x) >= 25 & nabs(y) > 0) +
        9*(nabs(x) >= 25 & nabs(y) < 0)
    
    return(circle)
    
}


## General Functions
# Enhance PBP
st.pbp_enhance <- function(pbp) {
    
    ## Description
    # pbp_enhance() performs some preliminary operations on a given PBP data frame object and returns \
    # the enhanced version
    
    pbp %>%
        mutate_each(funs(nabs), coords_x, coords_y, game_period, game_seconds) %>%
        data.frame() -> 
        pbp
    
    pbp %>%
        mutate(event_distance = st.distance_from_net(coords_x, coords_y),
               event_angle = st.angle_from_centre(coords_x, coords_y),
               event_rinkside = st.which_zone(coords_x),
               event_circle = st.which_circle(coords_x, coords_y)
        ) %>%
        data.frame() ->
        enhanced_pbp
    
    return(enhanced_pbp)
    
}

# Summarize Team Stats
st.sum_team <- function(x, venue) {
    
    ## Description
    # sum_team() summarizes all team counting stats from a PBP data frame object
    # x is expected to be a grouped data frame with home_team or away_team as a grouping variable for \
    # venue = "home" and venue = "away" respectively
    
    venue_ <- tolower(as.character(venue))
    
    if(venue_ == "home") {
        
        x %>%
            rename(team = home_team) %>%
            summarise(venue = "Home",
                      GP = length(unique(game_id)),
                      TOI = sum(event_length)/60,
                      CF = sum({event_type %in% st.fenwick_events & event_team == team} |
                      {event_type == "BLOCKED_SHOT" & event_team == away_team}
                      ),
                      CA = sum({event_type %in% st.fenwick_events & event_team == away_team} |
                      {event_type == "BLOCKED_SHOT" & event_team == team}
                      ),
                      FF = sum(event_type %in% st.fenwick_events & event_team == team),
                      FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
                      SF = sum(event_type %in% st.shot_events & event_team == team),
                      SA = sum(event_type %in% st.shot_events & event_team == away_team),
                      GF = sum(event_type == "GOAL" & event_team == team),
                      GA = sum(event_type == "GOAL" & event_team == away_team),
                      xGF = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == team))),
                      xGA = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == away_team))),
                      ACF = sum(na.omit(adj_home_corsi*(event_type %in% st.corsi_events & event_team == team))),
                      ACA = sum(na.omit(adj_away_corsi*(event_type %in% st.corsi_events & event_team == away_team))),
                      AFF = sum(na.omit(adj_home_fenwick*(event_type %in% st.fenwick_events & event_team == team))),
                      AFA = sum(na.omit(adj_away_fenwick*(event_type %in% st.fenwick_events & event_team == away_team))),
                      ASF = sum(na.omit(adj_home_shot*(event_type %in% st.shot_events & event_team == team))),
                      ASA = sum(na.omit(adj_away_shot*(event_type %in% st.shot_events & event_team == away_team))),
                      AGF = sum(na.omit(adj_home_goal*(event_type == "GOAL" & event_team == team))),
                      AGA = sum(na.omit(adj_away_goal*(event_type == "GOAL" & event_team == away_team))),
                      AxGF = sum(na.omit(adj_home_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == team))),
                      AxGA = sum(na.omit(adj_away_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == away_team))),
                      OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != home_rinkside),
                      DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == home_rinkside),
                      NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
                      FOW = sum(event_type == "FACEOFF" & event_team == team),
                      FOL = sum(event_type == "FACEOFF" & event_team == away_team),
                      PENT2 = sum(1*(event_type == "PENALTY" & event_team == team) +
                                      1*(event_type == "PENALTY" & event_team == team & grepl("double minor", tolower(event_detail)) == TRUE) -
                                      1*(event_type == "PENALTY" & event_team == team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      ),
                      PENT5 = sum(event_type == "PENALTY" & event_team == team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                      PENTS = sum(event_type == "PENALTY" & event_team == team & grepl("ps \\-", tolower(event_detail)) == TRUE),
                      PEND2 = sum(1*(event_type == "PENALTY" & event_team == away_team) +
                                      1*(event_type == "PENALTY" & event_team == away_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                                      1*(event_type == "PENALTY" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      ),
                      PEND5 = sum(event_type == "PENALTY" & event_team == away_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                      PENDS = sum(event_type == "PENALTY" & event_team == away_team & grepl("ps \\-", tolower(event_detail)) == TRUE),
                      
                      GVA = sum(event_type == "GIVEAWAY" & event_team == team),
                      TKA = sum(event_type == "TAKEAWAY" & event_team == team),
                      HF = sum(event_type == "HIT" & event_team == team),
                      HA = sum(event_type == "HIT" & event_team == away_team)
            ) %>%
            data.frame() %>%
            return()
        
    } else if(venue_ == "away") {
        
        x %>%
            rename(team = away_team) %>%
            summarise(venue = "Away",
                      GP = length(unique(game_id)),
                      TOI = sum(event_length)/60,
                      CF = sum({event_type %in% st.fenwick_events & event_team == team} |
                      {event_type == "BLOCKED_SHOT" & event_team == home_team}
                      ),
                      CA = sum({event_type %in% st.fenwick_events & event_team == home_team} |
                      {event_type == "BLOCKED_SHOT" & event_team == team}
                      ),
                      FF = sum(event_type %in% st.fenwick_events & event_team == team),
                      FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
                      SF = sum(event_type %in% st.shot_events & event_team == team),
                      SA = sum(event_type %in% st.shot_events & event_team == home_team),
                      GF = sum(event_type == "GOAL" & event_team == team),
                      GA = sum(event_type == "GOAL" & event_team == home_team),
                      xGF = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == team))),
                      xGA = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == home_team))),
                      ACF = sum(na.omit(adj_away_corsi*(event_type %in% st.corsi_events & event_team == team))),
                      ACA = sum(na.omit(adj_home_corsi*(event_type %in% st.corsi_events & event_team == home_team))),
                      AFF = sum(na.omit(adj_away_fenwick*(event_type %in% st.fenwick_events & event_team == team))),
                      AFA = sum(na.omit(adj_home_fenwick*(event_type %in% st.fenwick_events & event_team == home_team))),
                      ASF = sum(na.omit(adj_away_shot*(event_type %in% st.shot_events & event_team == team))),
                      ASA = sum(na.omit(adj_home_shot*(event_type %in% st.shot_events & event_team == home_team))),
                      AGF = sum(na.omit(adj_away_goal*(event_type == "GOAL" & event_team == team))),
                      AGA = sum(na.omit(adj_home_goal*(event_type == "GOAL" & event_team == home_team))),
                      AxGF = sum(na.omit(adj_away_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == team))),
                      AxGA = sum(na.omit(adj_home_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == home_team))),
                      OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != home_rinkside),
                      DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == home_rinkside),
                      NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
                      FOW = sum(event_type == "FACEOFF" & event_team == team),
                      FOL = sum(event_type == "FACEOFF" & event_team == home_team),
                      PENT2 = sum(1*(event_type == "PENALTY" & event_team == team) +
                                      1*(event_type == "PENALTY" & event_team == team & grepl("double minor", tolower(event_detail)) == TRUE) -
                                      1*(event_type == "PENALTY" & event_team == team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      ),
                      PENT5 = sum(event_type == "PENALTY" & event_team == team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                      PENTS = sum(event_type == "PENALTY" & event_team == team & grepl("ps \\-", tolower(event_detail)) == TRUE),
                      PEND2 = sum(1*(event_type == "PENALTY" & event_team == home_team) +
                                      1*(event_type == "PENALTY" & event_team == home_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                                      1*(event_type == "PENALTY" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      ),
                      PEND5 = sum(event_type == "PENALTY" & event_team == home_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                      PENDS = sum(event_type == "PENALTY" & event_team == home_team & grepl("ps \\-", tolower(event_detail)) == TRUE),
                      GVA = sum(event_type == "GIVEAWAY" & event_team == team),
                      TKA = sum(event_type == "TAKEAWAY" & event_team == team),
                      HF = sum(event_type == "HIT" & event_team == team),
                      HA = sum(event_type == "HIT" & event_team == home_team)
            ) %>%
            data.frame() %>%
            return()
        
    }
    
}

# Summarize Skater Stats
st.sum_skater <- function(x, venue) {
    
    ## Description
    # sum_skater() summarizes all skater counting stats from a PBP data frame object
    # x is expected to be a grouped data frame with home_on_x or away_on_x as a grouping variable \
    # for venue = "home" and venue = "away" respectively
    # A rename() argument must be passed before sum_skater() to convert home/away_on_x to player
    
    venue_ <- tolower(as.character(venue))
    
    if(venue_ == "home") {
        
        x %>%
            summarise(venue = "Home",
                      team = first(home_team),
                      GP = length(unique(game_id)),
                      TOI = sum(event_length)/60,
                      CF = sum({event_type %in% st.fenwick_events & event_team == home_team} |
                      {event_type == "BLOCKED_SHOT" & event_team == away_team}
                      ),
                      CA = sum({event_type %in% st.fenwick_events & event_team == away_team} |
                      {event_type == "BLOCKED_SHOT" & event_team == home_team}
                      ),
                      FF = sum(event_type %in% st.fenwick_events & event_team == home_team),
                      FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
                      SF = sum(event_type %in% st.shot_events & event_team == home_team),
                      SA = sum(event_type %in% st.shot_events & event_team == away_team),
                      GF = sum(event_type == "GOAL" & event_team == home_team),
                      GA = sum(event_type == "GOAL" & event_team == away_team),
                      xGF = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == home_team))),
                      xGA = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == away_team))),
                      ACF = sum(na.omit(adj_home_corsi*(event_type %in% st.corsi_events & event_team == home_team))),
                      ACA = sum(na.omit(adj_away_corsi*(event_type %in% st.corsi_events & event_team == away_team))),
                      AFF = sum(na.omit(adj_home_fenwick*(event_type %in% st.fenwick_events & event_team == home_team))),
                      AFA = sum(na.omit(adj_away_fenwick*(event_type %in% st.fenwick_events & event_team == away_team))),
                      ASF = sum(na.omit(adj_home_shot*(event_type %in% st.shot_events & event_team == home_team))),
                      ASA = sum(na.omit(adj_away_shot*(event_type %in% st.shot_events & event_team == away_team))),
                      AGF = sum(na.omit(adj_home_goal*(event_type == "GOAL" & event_team == home_team))),
                      AGA = sum(na.omit(adj_away_goal*(event_type == "GOAL" & event_team == away_team))),
                      AxGF = sum(na.omit(adj_home_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == home_team))),
                      AxGA = sum(na.omit(adj_away_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == away_team))),
                      OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != home_rinkside),
                      DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == home_rinkside),
                      NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
                      PENT2 = sum(1*(event_type == "PENALTY" & event_team == home_team) +
                                      1*(event_type == "PENALTY" & event_team == home_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                                      1*(event_type == "PENALTY" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      ),
                      PENT5 = sum(event_type == "PENALTY" & event_team == home_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                      PEND2 = sum(1*(event_type == "PENALTY" & event_team == away_team) +
                                      1*(event_type == "PENALTY" & event_team == away_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                                      1*(event_type == "PENALTY" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      ),
                      PEND5 = sum(event_type == "PENALTY" & event_team == away_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                      
                      iCF = sum({event_type %in% st.fenwick_events & event_player_1 == player} |
                      {event_type == "BLOCKED_SHOT" & event_player_2 == player}
                      ),
                      iFF = sum(event_type %in% st.fenwick_events & event_player_1 == player),
                      iSF = sum(event_type %in% st.shot_events & event_player_1 == player),
                      ixGF = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_player_1 == player))),
                      G = sum(event_type == "GOAL" & event_player_1 == player),
                      A1 = sum(na.omit(event_type == "GOAL" & event_player_2 == player)),
                      A2 = sum(na.omit(event_type == "GOAL" & event_player_3 == player)),
                      iGVA = sum(event_type == "GIVEAWAY" & event_player_1 == player),
                      iTKA = sum(event_type == "TAKEAWAY" & event_player_1 == player),
                      iHF = sum(event_type == "HIT" & event_player_1 == player),
                      iHA = sum(event_type == "HIT" & event_player_2 == player),
                      iBLK = sum(event_type == "BLOCKED_SHOT" & event_player_1 == player),
                      iFOW = sum(event_type == "FACEOFF" & event_player_1 == player),
                      iFOL = sum(event_type == "FACEOFF" & event_player_2 == player),
                      iPENT2 = sum(na.omit(1*(event_type == "PENALTY" & event_player_1 == player) +
                                               1*(event_type == "PENALTY" & event_player_1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                               1*(event_type == "PENALTY" & event_player_1 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      )
                      ),
                      iPENT5 = sum(na.omit(event_type == "PENALTY" & event_player_1 == player & grepl("fighting|major", tolower(event_detail)) == TRUE)),
                      iPEND2 = sum(na.omit(1*(event_type == "PENALTY" & event_player_2 == player) +
                                               1*(event_type == "PENALTY" & event_player_2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                               1*(event_type == "PENALTY" & event_player_2 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      )
                      ),
                      iPEND5 = sum(na.omit(event_type == "PENALTY" & event_player_2 == player & grepl("fighting|major", tolower(event_detail)) == TRUE))
            ) %>%
            data.frame() %>%
            return()
        
    } else if(venue_ == "away") {
        
        x %>%
            summarise(venue = "Away",
                      team = first(away_team),
                      GP = length(unique(game_id)),
                      TOI = sum(event_length)/60,
                      CF = sum({event_type %in% st.fenwick_events & event_team == away_team} |
                      {event_type == "BLOCKED_SHOT" & event_team == home_team}
                      ),
                      CA = sum({event_type %in% st.fenwick_events & event_team == home_team} |
                      {event_type == "BLOCKED_SHOT" & event_team == away_team}
                      ),
                      FF = sum(event_type %in% st.fenwick_events & event_team == away_team),
                      FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
                      SF = sum(event_type %in% st.shot_events & event_team == away_team),
                      SA = sum(event_type %in% st.shot_events & event_team == home_team),
                      GF = sum(event_type == "GOAL" & event_team == away_team),
                      GA = sum(event_type == "GOAL" & event_team == home_team),
                      xGF = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == away_team))),
                      xGA = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == home_team))),
                      ACF = sum(na.omit(adj_away_corsi*(event_type %in% st.corsi_events & event_team == away_team))),
                      ACA = sum(na.omit(adj_home_corsi*(event_type %in% st.corsi_events & event_team == home_team))),
                      AFF = sum(na.omit(adj_away_fenwick*(event_type %in% st.fenwick_events & event_team == away_team))),
                      AFA = sum(na.omit(adj_home_fenwick*(event_type %in% st.fenwick_events & event_team == home_team))),
                      ASF = sum(na.omit(adj_away_shot*(event_type %in% st.shot_events & event_team == away_team))),
                      ASA = sum(na.omit(adj_home_shot*(event_type %in% st.shot_events & event_team == home_team))),
                      AGF = sum(na.omit(adj_away_goal*(event_type == "GOAL" & event_team == away_team))),
                      AGA = sum(na.omit(adj_home_goal*(event_type == "GOAL" & event_team == home_team))),
                      AxGF = sum(na.omit(adj_away_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == away_team))),
                      AxGA = sum(na.omit(adj_home_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == home_team))),
                      OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != away_rinkside),
                      DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == away_rinkside),
                      NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
                      PENT2 = sum(1*(event_type == "PENALTY" & event_team == away_team) +
                                      1*(event_type == "PENALTY" & event_team == away_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                                      1*(event_type == "PENALTY" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      ),
                      PENT5 = sum(event_type == "PENALTY" & event_team == away_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                      PEND2 = sum(1*(event_type == "PENALTY" & event_team == home_team) +
                                      1*(event_type == "PENALTY" & event_team == home_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                                      1*(event_type == "PENALTY" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      ),
                      PEND5 = sum(event_type == "PENALTY" & event_team == home_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                      
                      iCF = sum({event_type %in% st.fenwick_events & event_player_1 == player} |
                      {event_type == "BLOCKED_SHOT" & event_player_2 == player}
                      ),
                      iFF = sum(event_type %in% st.fenwick_events & event_player_1 == player),
                      iSF = sum(event_type %in% st.shot_events & event_player_1 == player),
                      ixGF = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_player_1 == player))),
                      G = sum(event_type == "GOAL" & event_player_1 == player),
                      A1 = sum(na.omit(event_type == "GOAL" & event_player_2 == player)),
                      A2 = sum(na.omit(event_type == "GOAL" & event_player_3 == player)),
                      iGVA = sum(event_type == "GIVEAWAY" & event_player_1 == player),
                      iTKA = sum(event_type == "TAKEAWAY" & event_player_1 == player),
                      iHF = sum(event_type == "HIT" & event_player_1 == player),
                      iHA = sum(event_type == "HIT" & event_player_2 == player),
                      iBLK = sum(event_type == "BLOCKED_SHOT" & event_player_1 == player),
                      iFOW = sum(event_type == "FACEOFF" & event_player_1 == player),
                      iFOL = sum(event_type == "FACEOFF" & event_player_2 == player),
                      iPENT2 = sum(na.omit(1*(event_type == "PENALTY" & event_player_1 == player) +
                                               1*(event_type == "PENALTY" & event_player_1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                               1*(event_type == "PENALTY" & event_player_1 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      )
                      ),
                      iPENT5 = sum(na.omit(event_type == "PENALTY" & event_player_1 == player & grepl("fighting|major", tolower(event_detail)) == TRUE)),
                      iPEND2 = sum(na.omit(1*(event_type == "PENALTY" & event_player_2 == player) +
                                               1*(event_type == "PENALTY" & event_player_2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                               1*(event_type == "PENALTY" & event_player_2 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      )
                      ),
                      iPEND5 = sum(na.omit(event_type == "PENALTY" & event_player_2 == player & grepl("fighting|major", tolower(event_detail)) == TRUE))
            ) %>%
            data.frame() %>%
            return()
        
    }
    
}

# Summarize Goalie Stats
st.sum_goalie <- function(x, venue) {
    
    ## Description
    # sum_goalie() summarizes all goalie counting stats from a PBP data frame object
    # x is expected to be a grouped data frame with home_goalie or away_goalie as a grouping variable for \
    # venue = "home" and venue = "away" respectively
    
    venue_ <- tolower(as.character(venue))
    
    if(venue_ == "home") {
        
        x %>%
            rename(player = home_goalie) %>%
            summarise(venue = "Home",
                      team = first(home_team),
                      GP = length(unique(game_id)),
                      TOI = sum(nabs(event_length))/60,
                      CA = sum(event_type %in% st.corsi_events & event_team == away_team),
                      FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
                      SA = sum(event_type %in% st.shot_events & event_team == away_team),
                      GA = sum(event_type == "GOAL" & event_team == away_team),
                      xGA = sum(na.omit((prob_goal/(prob_goal + prob_save))*(event_type %in% st.shot_events & event_team == away_team)))
            ) %>%
            data.frame() %>%
            return()
        
    } else if(venue_ == "away") {
        
        x %>%
            rename(player = away_goalie) %>%
            summarise(venue = "Away",
                      team = first(away_team),
                      GP = length(unique(game_id)),
                      TOI = sum(nabs(event_length))/60,
                      CA = sum(event_type %in% st.corsi_events & event_team == home_team),
                      FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
                      SA = sum(event_type %in% st.shot_events & event_team == home_team),
                      GA = sum(event_type == "GOAL" & event_team == home_team),
                      xGA = sum(na.omit((prob_goal/(prob_goal + prob_save))*(event_type %in% st.shot_events & event_team == home_team)))
            ) %>%
            data.frame() %>%
            return()
        
    }
    
}

# Summarize Team Stats (Old PBP Format)
st.old_sum_team <- function(x, venue) {
    
    ## Description
    # old_sum_team() summarizes all team counting stats from a Corsica 1.0 PBP data frame object
    # x is expected to be a grouped data frame with home_team or away_team as a grouping variable for \
    # venue = "home" and venue = "away" respectively
    
    venue_ <- tolower(as.character(venue))
    
    if(venue_ == "home") {
        
        x %>%
            rename(team = home_team) %>%
            summarise(venue = "Home",
                      GP = length(unique(game_id)),
                      TOI = sum(nabs(Event.Length))/60,
                      CF = sum(event_type %in% st.corsi_events & event_team == team),
                      CA = sum(event_type %in% st.corsi_events & event_team == away_team),
                      FF = sum(event_type %in% st.fenwick_events & event_team == team),
                      FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
                      SF = sum(event_type %in% st.shot_events & event_team == team),
                      SA = sum(event_type %in% st.shot_events & event_team == away_team),
                      GF = sum(event_type == "GOAL" & event_team == team),
                      GA = sum(event_type == "GOAL" & event_team == away_team),
                      xGF = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == team))),
                      xGA = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == away_team))),
                      ACF = sum(na.omit(adj_home_corsi*(event_type %in% st.corsi_events & event_team == team))),
                      ACA = sum(na.omit(adj_away_corsi*(event_type %in% st.corsi_events & event_team == away_team))),
                      AFF = sum(na.omit(adj_home_fenwick*(event_type %in% st.fenwick_events & event_team == team))),
                      AFA = sum(na.omit(adj_away_fenwick*(event_type %in% st.fenwick_events & event_team == away_team))),
                      ASF = sum(na.omit(adj_home_shot*(event_type %in% st.shot_events & event_team == team))),
                      ASA = sum(na.omit(adj_away_shot*(event_type %in% st.shot_events & event_team == away_team))),
                      AGF = sum(na.omit(adj_home_goal*(event_type == "GOAL" & event_team == team))),
                      AGA = sum(na.omit(adj_away_goal*(event_type == "GOAL" & event_team == away_team))),
                      AxGF = sum(na.omit(adj_home_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == team))),
                      AxGA = sum(na.omit(adj_away_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == away_team))),
                      OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != home_rinkside),
                      DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == home_rinkside),
                      NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
                      FOW = sum(event_type == "FACEOFF" & event_team == team),
                      FOL = sum(event_type == "FACEOFF" & event_team == away_team),
                      PENT2 = sum(1*(event_type == "PENL" & event_team == team) +
                                      1*(event_type == "PENL" & event_team == team & grepl("double minor", tolower(event_description)) == TRUE) -
                                      1*(event_type == "PENL" & event_team == team & grepl("ps \\-|match|fighting|major", tolower(event_description)) == TRUE)
                      ),
                      PENT5 = sum(event_type == "PENL" & event_team == team & grepl("fighting|major", tolower(event_description)) == TRUE),
                      PENTS = sum(event_type == "PENL" & event_team == team & grepl("ps \\-", tolower(event_description)) == TRUE),
                      PEND2 = sum(1*(event_type == "PENL" & event_team == away_team) +
                                      1*(event_type == "PENL" & event_team == away_team & grepl("double minor", tolower(event_description)) == TRUE) -
                                      1*(event_type == "PENL" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_description)) == TRUE)
                      ),
                      PEND5 = sum(event_type == "PENL" & event_team == away_team & grepl("fighting|major", tolower(event_description)) == TRUE),
                      PENDS = sum(event_type == "PENL" & event_team == away_team & grepl("ps \\-", tolower(event_description)) == TRUE),
                      
                      GVA = sum(event_type == "GIVEAWAY" & event_team == team),
                      TKA = sum(event_type == "TAKEAWAY" & event_team == team),
                      HF = sum(event_type == "HIT" & event_team == team),
                      HA = sum(event_type == "HIT" & event_team == away_team)
            ) %>%
            data.frame() %>%
            return()
        
    } else if(venue_ == "away") {
        
        x %>%
            rename(team = away_team) %>%
            summarise(venue = "Away",
                      GP = length(unique(game_id)),
                      TOI = sum(nabs(Event.Length))/60,
                      CF = sum(event_type %in% st.corsi_events & event_team == team),
                      CA = sum(event_type %in% st.corsi_events & event_team == home_team),
                      FF = sum(event_type %in% st.fenwick_events & event_team == team),
                      FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
                      SF = sum(event_type %in% st.shot_events & event_team == team),
                      SA = sum(event_type %in% st.shot_events & event_team == home_team),
                      GF = sum(event_type == "GOAL" & event_team == team),
                      GA = sum(event_type == "GOAL" & event_team == home_team),
                      xGF = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == team))),
                      xGA = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == home_team))),
                      ACF = sum(na.omit(adj_away_corsi*(event_type %in% st.corsi_events & event_team == team))),
                      ACA = sum(na.omit(adj_home_corsi*(event_type %in% st.corsi_events & event_team == home_team))),
                      AFF = sum(na.omit(adj_away_fenwick*(event_type %in% st.fenwick_events & event_team == team))),
                      AFA = sum(na.omit(adj_home_fenwick*(event_type %in% st.fenwick_events & event_team == home_team))),
                      ASF = sum(na.omit(adj_away_shot*(event_type %in% st.shot_events & event_team == team))),
                      ASA = sum(na.omit(adj_home_shot*(event_type %in% st.shot_events & event_team == home_team))),
                      AGF = sum(na.omit(adj_away_goal*(event_type == "GOAL" & event_team == team))),
                      AGA = sum(na.omit(adj_home_goal*(event_type == "GOAL" & event_team == home_team))),
                      AxGF = sum(na.omit(adj_away_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == team))),
                      AxGA = sum(na.omit(adj_home_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == home_team))),
                      OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != home_rinkside),
                      DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == home_rinkside),
                      NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
                      FOW = sum(event_type == "FACEOFF" & event_team == team),
                      FOL = sum(event_type == "FACEOFF" & event_team == home_team),
                      PENT2 = sum(1*(event_type == "PENL" & event_team == team) +
                                      1*(event_type == "PENL" & event_team == team & grepl("double minor", tolower(event_description)) == TRUE) -
                                      1*(event_type == "PENL" & event_team == team & grepl("ps \\-|match|fighting|major", tolower(event_description)) == TRUE)
                      ),
                      PENT5 = sum(event_type == "PENL" & event_team == team & grepl("fighting|major", tolower(event_description)) == TRUE),
                      PENTS = sum(event_type == "PENL" & event_team == team & grepl("ps \\-", tolower(event_description)) == TRUE),
                      PEND2 = sum(1*(event_type == "PENL" & event_team == home_team) +
                                      1*(event_type == "PENL" & event_team == home_team & grepl("double minor", tolower(event_description)) == TRUE) -
                                      1*(event_type == "PENL" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_description)) == TRUE)
                      ),
                      PEND5 = sum(event_type == "PENL" & event_team == home_team & grepl("fighting|major", tolower(event_description)) == TRUE),
                      PENDS = sum(event_type == "PENL" & event_team == home_team & grepl("ps \\-", tolower(event_description)) == TRUE),
                      GVA = sum(event_type == "GIVEAWAY" & event_team == team),
                      TKA = sum(event_type == "TAKEAWAY" & event_team == team),
                      HF = sum(event_type == "HIT" & event_team == team),
                      HA = sum(event_type == "HIT" & event_team == home_team)
            ) %>%
            data.frame() %>%
            return()
        
    }
    
}

# Summarize Skater Stats (Old PBP Format)
st.old_sum_skater <- function(x, venue) {
    
    ## Description
    # old_sum_skater() summarizes all skater counting stats from a Corsica 1.0 PBP data frame object
    # x is expected to be a grouped data frame with home_on_x or away_on_x as a grouping variable \
    # for venue = "home" and venue = "away" respectively
    # A rename() argument must be passed before sum_skater() to convert home/away_on_x to player
    
    venue_ <- tolower(as.character(venue))
    
    if(venue_ == "home") {
        
        x %>%
            summarise(venue = "Home",
                      team = first(home_team),
                      GP = length(unique(game_id)),
                      TOI = sum(nabs(Event.Length))/60,
                      CF = sum(event_type %in% st.corsi_events & event_team == home_team),
                      CA = sum(event_type %in% st.corsi_events & event_team == away_team),
                      FF = sum(event_type %in% st.fenwick_events & event_team == home_team),
                      FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
                      SF = sum(event_type %in% st.shot_events & event_team == home_team),
                      SA = sum(event_type %in% st.shot_events & event_team == away_team),
                      GF = sum(event_type == "GOAL" & event_team == home_team),
                      GA = sum(event_type == "GOAL" & event_team == away_team),
                      xGF = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == home_team))),
                      xGA = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == away_team))),
                      ACF = sum(na.omit(adj_home_corsi*(event_type %in% st.corsi_events & event_team == home_team))),
                      ACA = sum(na.omit(adj_away_corsi*(event_type %in% st.corsi_events & event_team == away_team))),
                      AFF = sum(na.omit(adj_home_fenwick*(event_type %in% st.fenwick_events & event_team == home_team))),
                      AFA = sum(na.omit(adj_away_fenwick*(event_type %in% st.fenwick_events & event_team == away_team))),
                      ASF = sum(na.omit(adj_home_shot*(event_type %in% st.shot_events & event_team == home_team))),
                      ASA = sum(na.omit(adj_away_shot*(event_type %in% st.shot_events & event_team == away_team))),
                      AGF = sum(na.omit(adj_home_goal*(event_type == "GOAL" & event_team == home_team))),
                      AGA = sum(na.omit(adj_away_goal*(event_type == "GOAL" & event_team == away_team))),
                      AxGF = sum(na.omit(adj_home_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == home_team))),
                      AxGA = sum(na.omit(adj_away_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == away_team))),
                      OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != home_rinkside),
                      DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == home_rinkside),
                      NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
                      PENT2 = sum(1*(event_type == "PENL" & event_team == home_team) +
                                      1*(event_type == "PENL" & event_team == home_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                                      1*(event_type == "PENL" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      ),
                      PENT5 = sum(event_type == "PENL" & event_team == home_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                      PEND2 = sum(1*(event_type == "PENL" & event_team == away_team) +
                                      1*(event_type == "PENL" & event_team == away_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                                      1*(event_type == "PENL" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      ),
                      PEND5 = sum(event_type == "PENL" & event_team == away_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                      
                      iCF = sum(event_type %in% st.corsi_events & p1 == player),
                      iFF = sum(event_type %in% st.fenwick_events & p1 == player),
                      iSF = sum(event_type %in% st.shot_events & p1 == player),
                      ixGF = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & p1 == player))),
                      G = sum(event_type == "GOAL" & p1 == player),
                      A1 = sum(na.omit(event_type == "GOAL" & p2 == player)),
                      A2 = sum(na.omit(event_type == "GOAL" & p3 == player)),
                      iGVA = sum(event_type == "GIVEAWAY" & p1 == player),
                      iTKA = sum(event_type == "TAKEAWAY" & p1 == player),
                      iHF = sum(event_type == "HIT" & p1 == player),
                      iHA = sum(event_type == "HIT" & p2 == player),
                      iBLK = sum(event_type == "BLOCKED_SHOT" & p2 == player),
                      iFOW = sum(event_type == "FACEOFF" & p1 == player),
                      iFOL = sum(event_type == "FACEOFF" & p2 == player),
                      iPENT2 = sum(na.omit(1*(event_type == "PENL" & p1 == player) +
                                               1*(event_type == "PENL" & p1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                               1*(event_type == "PENL" & p1 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      )
                      ),
                      iPENT5 = sum(na.omit(event_type == "PENL" & p1 == player & grepl("fighting|major", tolower(event_detail)) == TRUE)),
                      iPEND2 = sum(na.omit(1*(event_type == "PENL" & p2 == player) +
                                               1*(event_type == "PENL" & p2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                               1*(event_type == "PENL" & p2 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      )
                      ),
                      iPEND5 = sum(na.omit(event_type == "PENL" & p2 == player & grepl("fighting|major", tolower(event_detail)) == TRUE))
            ) %>%
            data.frame() %>%
            return()
        
    } else if(venue_ == "away") {
        
        x %>%
            summarise(venue = "Away",
                      team = first(away_team),
                      GP = length(unique(game_id)),
                      TOI = sum(nabs(Event.Length))/60,
                      CF = sum(event_type %in% st.corsi_events & event_team == away_team),
                      CA = sum(event_type %in% st.corsi_events & event_team == home_team),
                      FF = sum(event_type %in% st.fenwick_events & event_team == away_team),
                      FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
                      SF = sum(event_type %in% st.shot_events & event_team == away_team),
                      SA = sum(event_type %in% st.shot_events & event_team == home_team),
                      GF = sum(event_type == "GOAL" & event_team == away_team),
                      GA = sum(event_type == "GOAL" & event_team == home_team),
                      xGF = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == away_team))),
                      xGA = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & event_team == home_team))),
                      ACF = sum(na.omit(adj_away_corsi*(event_type %in% st.corsi_events & event_team == away_team))),
                      ACA = sum(na.omit(adj_home_corsi*(event_type %in% st.corsi_events & event_team == home_team))),
                      AFF = sum(na.omit(adj_away_fenwick*(event_type %in% st.fenwick_events & event_team == away_team))),
                      AFA = sum(na.omit(adj_home_fenwick*(event_type %in% st.fenwick_events & event_team == home_team))),
                      ASF = sum(na.omit(adj_away_shot*(event_type %in% st.shot_events & event_team == away_team))),
                      ASA = sum(na.omit(adj_home_shot*(event_type %in% st.shot_events & event_team == home_team))),
                      AGF = sum(na.omit(adj_away_goal*(event_type == "GOAL" & event_team == away_team))),
                      AGA = sum(na.omit(adj_home_goal*(event_type == "GOAL" & event_team == home_team))),
                      AxGF = sum(na.omit(adj_away_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == away_team))),
                      AxGA = sum(na.omit(adj_home_goal*prob_goal*(event_type %in% st.fenwick_events & event_team == home_team))),
                      OZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside != away_rinkside),
                      DZS = sum(event_type == "FACEOFF" & event_rinkside %in% c("L", "R") & event_rinkside == away_rinkside),
                      NZS = sum(event_type == "FACEOFF" & event_rinkside == "N"),
                      PENT2 = sum(1*(event_type == "PENL" & event_team == away_team) +
                                      1*(event_type == "PENL" & event_team == away_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                                      1*(event_type == "PENL" & event_team == away_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      ),
                      PENT5 = sum(event_type == "PENL" & event_team == away_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                      PEND2 = sum(1*(event_type == "PENL" & event_team == home_team) +
                                      1*(event_type == "PENL" & event_team == home_team & grepl("double minor", tolower(event_detail)) == TRUE) -
                                      1*(event_type == "PENL" & event_team == home_team & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      ),
                      PEND5 = sum(event_type == "PENL" & event_team == home_team & grepl("fighting|major", tolower(event_detail)) == TRUE),
                      
                      iCF = sum(event_type %in% st.corsi_events & p1 == player),
                      iFF = sum(event_type %in% st.fenwick_events & p1 == player),
                      iSF = sum(event_type %in% st.shot_events & p1 == player),
                      ixGF = sum(na.omit(prob_goal*(event_type %in% st.fenwick_events & p1 == player))),
                      G = sum(event_type == "GOAL" & p1 == player),
                      A1 = sum(na.omit(event_type == "GOAL" & p2 == player)),
                      A2 = sum(na.omit(event_type == "GOAL" & p3 == player)),
                      iGVA = sum(event_type == "GIVEAWAY" & p1 == player),
                      iTKA = sum(event_type == "TAKEAWAY" & p1 == player),
                      iHF = sum(event_type == "HIT" & p1 == player),
                      iHA = sum(event_type == "HIT" & p2 == player),
                      iBLK = sum(event_type == "BLOCKED_SHOT" & p2 == player),
                      iFOW = sum(event_type == "FACEOFF" & p1 == player),
                      iFOL = sum(event_type == "FACEOFF" & p2 == player),
                      iPENT2 = sum(na.omit(1*(event_type == "PENL" & p1 == player) +
                                               1*(event_type == "PENL" & p1 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                               1*(event_type == "PENL" & p1 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      )
                      ),
                      iPENT5 = sum(na.omit(event_type == "PENL" & p1 == player & grepl("fighting|major", tolower(event_detail)) == TRUE)),
                      iPEND2 = sum(na.omit(1*(event_type == "PENL" & p2 == player) +
                                               1*(event_type == "PENL" & p2 == player & grepl("double minor", tolower(event_detail)) == TRUE) -
                                               1*(event_type == "PENL" & p2 == player & grepl("ps \\-|match|fighting|major", tolower(event_detail)) == TRUE)
                      )
                      ),
                      iPEND5 = sum(na.omit(event_type == "PENL" & p2 == player & grepl("fighting|major", tolower(event_detail)) == TRUE))
            ) %>%
            data.frame() %>%
            return()
        
    }
    
}

# Summarize Goalie Stats (Old PBP Format)
st.old_sum_goalie <- function(x, venue) {
    
    ## Description
    # old_sum_goalie() summarizes all goalie counting stats from a Corsica 1.0 PBP data frame object
    # x is expected to be a grouped data frame with home_goalie or away_goalie as a grouping variable for \
    # venue = "home" and venue = "away" respectively
    
    venue_ <- tolower(as.character(venue))
    
    if(venue_ == "home") {
        
        x %>%
            rename(player = home_goalie) %>%
            summarise(venue = "Home",
                      team = first(home_team),
                      GP = length(unique(game_id)),
                      TOI = sum(nabs(Event.Length))/60,
                      CA = sum(event_type %in% st.corsi_events & event_team == away_team),
                      FA = sum(event_type %in% st.fenwick_events & event_team == away_team),
                      SA = sum(event_type %in% st.shot_events & event_team == away_team),
                      GA = sum(event_type == "GOAL" & event_team == away_team),
                      xGA = sum(na.omit((prob_goal/(prob_goal + prob_save))*(event_type %in% st.shot_events & event_team == away_team)))
            ) %>%
            data.frame() %>%
            return()
        
    } else if(venue_ == "away") {
        
        x %>%
            rename(player = away_goalie) %>%
            summarise(venue = "Away",
                      team = first(away_team),
                      GP = length(unique(game_id)),
                      TOI = sum(nabs(Event.Length))/60,
                      CA = sum(event_type %in% st.corsi_events & event_team == home_team),
                      FA = sum(event_type %in% st.fenwick_events & event_team == home_team),
                      SA = sum(event_type %in% st.shot_events & event_team == home_team),
                      GA = sum(event_type == "GOAL" & event_team == home_team),
                      xGA = sum(na.omit((prob_goal/(prob_goal + prob_save))*(event_type %in% st.shot_events & event_team == home_team)))
            ) %>%
            data.frame() %>%
            return()
        
    }
    
}



###############################################################################################################
###############################################################################################################



### USER FUNCTIONS ###
# Last edit: Manny (2017-05-06)


## Description
# User Functions are meta functions and methods for more efficient code writing
# Dependencies: dplyr, doMC


## Dependencies
#require(dplyr); require(doMC)


## General Functions
# Numeric Absolute
nabs <- function(x) {
    
    ## Description
    # nabs() returns x after first converting it to class numeric via character
    # Its primary use is converting objects of class factor to numeric
    # It also provides a more concise wrapper for standard numeric conversion
    
    return(as.numeric(as.character(x)))
    
}

# Logarithmic Loss
log_loss <- function(act, pred, allow_inf = FALSE) {
    
    ## Description
    # log_loss() returns the logarithmic loss obtained from a given prediction and known result
    # The allow_inf parameter controls whether infinite loss is allowed (default is FALSE)
    # Setting allow_inf to FALSE will cause large but finite penalties at the extremes
    
    eps = as.numeric(!allow_inf)*1e-15
    
    pred = matrix(sapply(pred, function(x) max(eps, x)),
                  nrow = nrow(pred)
    )      
    pred = matrix(sapply(pred, function(x) min(1 - eps, x)), 
                  nrow = nrow(pred)
    )
    
    ll = sum(act*log(pred) + (1 - act)*log(1 - pred))
    ll = -ll/(nrow(act))    
    
    return(ll)
    
}

# Moving 
moving <- function(x, n = 5) {
    
    ## Description
    # moving() returns a vector of averages obtained from the n elements of x preceding and including the element \
    # at each respective index
    
    if(length(x) < n) {
        
        v <- NA
        
    } else {
        
        stats::filter(x,
                      rep(1/n, n),
                      sides = 1
        ) -> 
            v
        
    }
    
    return(as.numeric(v))
    
}

# Brier Score
brier <- function(act, pred) {
    
    ## Description
    # brier() returns the Brier score obtained from a given prediction and known result
    
    bri <- sum((act - pred)^2)/length(act)
    
    return(bri)
    
}

# NA if NULL
na_if_null <- function(x) {
    
    ## Description
    # na_if_null() returns an object's value if it is not NULL and NA otherwise
    
    return(ifelse(is.null(x) == TRUE,
                  NA,
                  x
    )
    )
    
}

# Do Call Apply
dcapply <- function(x, fun, combine, cores, ...) {
    
    ## Description
    # dcapply() uses do.call() to merge the products of an applied function according to specifications
    # The function will be applied in parallel if cores >= 1
    
    if(cores > 1) {
        
        registerDoMC(cores)
        
        chunks <- split(x, cut(1:length(x), cores))
        
        foreach(i = 1:cores, .combine = c) %dopar% {
            
            chunks[[i]] %>%
                lapply(fun, ...) 
            
        } -> list
        
        combined <- do.call(combine, list)
        
    } else {
        
        
        list <- lapply(x, fun, ...)
        
        combined <- do.call(combine, list)
        
    }
    
    return(combined)
    
}

# NA as String
na_as_string <- function(x) {
    
    ## Description
    # na_as_string() returns a character vector with NA values replaced as "NA"
    
    x <- as.character(x)
    
    x[which(is.na(x) == TRUE)] <- "NA"
    
    return(x)
    
}

# NA as String
na_as_zero <- function(x) {
    
    ## Description
    # na_as_zero() returns a numeric vector with NA values replaced as 0
    
    x <- nabs(x)
    
    x[which(is.na(x) == TRUE)] <- 0
    
    return(x)
    
}

# F Table to Data Frame
ftable2df <- function(mydata) {
    
    ## Description
    # ftable2df() returns a data.frame from an ftable object
    
    ifelse(class(mydata) == "ftable", 
           mydata <- mydata, 
           mydata <- ftable(mydata)
    )
    
    dfrows <- rev(expand.grid(rev(attr(mydata, "row.vars"))))
    
    dfcols <- as.data.frame.matrix(mydata)
    
    do.call(paste, 
            c(rev(expand.grid(rev(attr(mydata, "col.vars")))), 
              sep = "_"
            )
    ) -> names(dfcols)
    
    cbind(dfrows, dfcols)
    
}


game_number <- as.integer(game_number)

pbp_list <- ds.compile_games(games = as.character(game_number),
                             season = "20172018",
                             pause = 2,
                             try_tolerance = 5,
                             agents = ds.user_agents)

pbp_df <- pbp_list[[1]]
################################################################################
#All code below here including xGmodel written by Matthew Barlowe @matt_barlowe#
#on twitter#####################################################################
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)
library(readr)
library(grid)
library(gridExtra)
library(stringr)
#loads xGmodel
xGModel <-load('~/ProgrammingStuff/R/Rprojects/xGmodel/xGmodelver2.rda')
home_team <- unique(pbp_df$home_team)[1]
away_team <- unique(pbp_df$away_team)[1]

#vectors used to subset the pbp data depending on event types
corsi <- c('SHOT', 'MISS', 'BLOCK', 'GOAL')
fenwick <- c('SHOT', 'MISS', 'GOAL')

#game strength states vectors used to determine shooter strength state
even_strength <- c('5v5', '4v4', '3v3', 'EvE', '5v0')
home_advantage <- c('5v4', '5v3', '4v3')
away_advantage <- c('4v5', '3v5', '3v4')
home_empty_net <- c('Ev5', 'Ev4', 'Ev3')
away_empty_net <- c('5vE', '4vE', '3vE')

#function to create dummy variables to determine if 
#event was committed by the home team
is_home <- function(dataframe){
    dataframe$is_home <- ifelse(dataframe$event_team == 
                                    dataframe$home_team, 1 , 0)
    return(dataframe)
}


#removes shootout info from data frame and creates is_home dummy variable
pbp_df <- pbp_df[pbp_df$game_period < 5,]
pbp_df <- is_home(pbp_df)

#calculating the time difference between each event in the game by seconds
#this will be used later to calculate whether shot is rebound or on the rush
pbp_df <- pbp_df %>% mutate(time_diff = game_seconds - lag(game_seconds))

#creating running corsi totals for each team 
pbp_df$home_corsi <- ifelse(pbp_df$is_home == 1 & pbp_df$event_type %in% corsi, 1, 0)
pbp_df$away_corsi <- ifelse(pbp_df$is_home == 0 & pbp_df$event_type %in% corsi, 1, 0)
pbp_df <- pbp_df %>% mutate(home_corsi_total = cumsum(home_corsi))
pbp_df <- pbp_df %>% mutate(away_corsi_total = cumsum(away_corsi))

#creates the is_rebound dummy variable for each even and turns any NAs to zeros
#just in case so the model doesn't throw an error 
pbp_df$is_rebound <- ifelse(pbp_df$time_diff < 3 & 
                                        pbp_df$event_type %in% corsi &
                                        pbp_df$event_team == 
                                        lag(pbp_df$event_team),
                                    1, 0)
pbp_df$is_rebound[is.na(pbp_df$is_rebound)] <- 0

#creates is_rush variable just like is_rebound
pbp_df$is_rush <- ifelse(pbp_df$time_diff < 4 &
                                     lag(abs(pbp_df$coords_x)) < 25 &
                                     pbp_df$event_type %in% corsi,
                                 1, 0)
pbp_df$is_rush[is.na(pbp_df$is_rush)] <- 0

#creating shot_angle, distance, and shot angle for shots taken below each goal 
#line in the second calculation which requires a different trignometric strategy
#distance is calculated from the goal 
pbp_df$shot_angle <- (asin(abs(pbp_df$coords_y)/sqrt((87.95 
                                        - abs(pbp_df$coords_x))^2
                                        + pbp_df$coords_y^2))*180)/ 3.14

pbp_df$shot_angle <- ifelse(pbp_df$coords_x > 88 | pbp_df$coords_x < -88, 90 + 
                        (180-(90 + pbp_df$shot_angle)), pbp_df$shot_angle)

pbp_df$distance <- sqrt((87.95 - abs(pbp_df$coords_x))^2 + pbp_df$coords_y^2)





#if statements to determine shooter strength state based on vectors declared
#at beginning of file
pbp_df$shooter_strength <- ifelse(pbp_df$game_strength_state %in% even_strength, 
                                              'EV', NA)
pbp_df$shooter_strength <- ifelse(pbp_df$game_strength_state %in% home_advantage
                    & pbp_df$is_home == 1, 'PP', pbp_df$shooter_strength)
pbp_df$shooter_strength <- ifelse(pbp_df$game_strength_state %in% away_advantage
                    & pbp_df$is_home == 1, 'SH', pbp_df$shooter_strength)
pbp_df$shooter_strength <- ifelse(pbp_df$game_strength_state %in% home_advantage
                    & pbp_df$is_home == 0, 'SH', pbp_df$shooter_strength)
pbp_df$shooter_strength <- ifelse(pbp_df$game_strength_state %in% away_advantage
                    & pbp_df$is_home == 0, 'PP', pbp_df$shooter_strength)
pbp_df$shooter_strength <- ifelse(pbp_df$game_strength_state %in% home_empty_net
                    & pbp_df$is_home == 1, 'PP', pbp_df$shooter_strength)
pbp_df$shooter_strength <- ifelse(pbp_df$game_strength_state %in% home_empty_net
                    & pbp_df$is_home == 0, 'EN', pbp_df$shooter_strength)
pbp_df$shooter_strength <- ifelse(pbp_df$game_strength_state %in% away_empty_net
                    & pbp_df$is_home == 1, 'EN', pbp_df$shooter_strength)
pbp_df$shooter_strength <- ifelse(pbp_df$game_strength_state %in% away_empty_net
                    & pbp_df$is_home == 0, 'PP', pbp_df$shooter_strength)
pbp_df$shooter_strength <- ifelse(pbp_df$game_strength_state %in% home_empty_net
                    & pbp_df$is_home == 1, 'PP', pbp_df$shooter_strength)
pbp_df$shooter_strength <- ifelse(pbp_df$game_strength_state %in% c('Ev0', '0vE'), 
                                              'PS', pbp_df$shooter_strength)

#filters pbp dataframe to fenwick events and predict the xG of each event
fenwick_pbp<- filter(pbp_df, event_type %in% c("SHOT", "MISS", "GOAL"))
fenwick_pbp$xG <-predict(xGmodel, fenwick_pbp, type = 'response')

#merge the fenwick PbP xG values back with the main PbP dataframe and replacing
#any NAs with zeros
pbp_df <- merge(pbp_df, fenwick_pbp[, c('event_index', 'xG')], 
                by = 'event_index', all.x = TRUE)

pbp_df <- pbp_df %>% replace_na(list(xG= 0))


#creates new columns that seperates xG into home and away values to calculate
#a running sum
pbp_df$home_xG <- ifelse(pbp_df$is_home == 1 & 
                             pbp_df$event_type %in% fenwick, pbp_df$xG, 0)
pbp_df$away_xG <- ifelse(pbp_df$is_home == 0 & 
                             pbp_df$event_type %in% fenwick, pbp_df$xG, 0)

#creating 5v5 xg values
pbp_df$home_5v5_xG <- ifelse(pbp_df$is_home == 1 & pbp_df$home_skaters == 5 &
                             pbp_df$away_skaters == 5 & pbp_df$event_type %in% 
                             fenwick, pbp_df$xG, 0)

pbp_df$away_5v5_xG <- ifelse(pbp_df$is_home == 0 & pbp_df$home_skaters == 5 &
                                 pbp_df$away_skaters == 5 & pbp_df$event_type %in% 
                                 fenwick, pbp_df$xG, 0)

#calculates the running sum for the step graphs for all situations and 5v5
pbp_df <- mutate(pbp_df, run_home_xg = cumsum(home_xG))
pbp_df <- mutate(pbp_df, run_away_xg = cumsum(away_xG))
pbp_df <- mutate(pbp_df, run_home_5v5_xg = cumsum(home_5v5_xG))
pbp_df <- mutate(pbp_df, run_away_5v5_xg = cumsum(away_5v5_xG))

#turns running sums of home and away xG values into long datat format inorder
#to step plot them
xg_graph_df <- gather(pbp_df, 'run_home_xg', 'run_away_xg', 'run_home_5v5_xg',
                      'run_away_5v5_xg',
                      key = 'team', value = 'running_xg')


#assigns colors to each team for graphing purposes
team_colors <- c('black', 'darkred', 'gold', 'royalblue4', 'red3', 'firebrick1',
                 'black', 'navy', 'steelblue', 'red', 'darkgreen', 'navyblue', 
                 'orange', 'mediumblue', 'slategrey', 'limegreen', 'darkgreen',
                 'darkorange1', 'yellow2', 'mediumblue', 'steelblue4', 'red2',
                 'lightseagreen', 'darkorange1', 'dodgerblue4', 'gold', 'gold',
                 'dodgerblue3', 'dodgerblue3', 'navyblue', 'red1')
names(team_colors) <- c('ANA', 'ARI','BOS', 'BUF', 'CGY', 'CAR', 'CHI', 'CBJ', 
                        'COL', 'DET', 'DAL', 'FLA', 'EDM', 'MTL', 'L.A', 'N.J',
                        'MIN', 'NYI', 'NSH', 'NYR', 'STL', 'OTT', 'S.J', 'PHI',
                        'VAN', 'PIT', 'VGK', 'T.B', 'TOR', 'WPG', 'WSH')

#creates xG leaders for Each team
fenwick_pbp <- filter(pbp_df, event_type %in% c("SHOT", "MISS", "GOAL"))
groupd_player_xg <- fenwick_pbp %>% group_by(event_player_1, event_team) %>%
    dplyr::summarise(xg = sum(xG))

#Sums xG by team
xg_sums <- fenwick_pbp %>% group_by(event_team) %>% 
    dplyr::summarise( xG = sum(xG))

#creates title graph from team names and graphs running xG throughout the game
xg_graph_title <- paste(away_team, '@', home_team, 
                        'Expected Goals', Sys.Date()-1)
xg_5v5_graph_title <- paste(away_team, '@', home_team, 
                            '5v5 Expected Goals', Sys.Date()-1)
final_xg_score <- paste(away_team, format(xg_sums$xG[1], digits = 3), 
                        home_team, format(xg_sums$xG[2], digits = 3))
xg_locations_title <- paste(away_team, '@', home_team, 'xG Locations', 
                            Sys.Date()-1)

xG_plot_all_sits  <- ggplot(aes(x = game_seconds/60, y = running_xg), 
            data = subset(xg_graph_df, xg_graph_df$team %in% c('run_home_xg',
                    'run_away_xg'))) +
            geom_step(aes(color = team)) +
            geom_point(aes(x = game_seconds/60, y = run_home_xg), 
            data = subset(pbp_df, pbp_df$event_type == "GOAL" & 
                pbp_df$event_team ==
                pbp_df$home_team), shape = 13) +
            geom_point(aes(x = game_seconds/60, y = run_away_xg), 
                data = subset(pbp_df, pbp_df$event_type == 'GOAL' & 
                pbp_df$event_team == pbp_df$away_team), shape = 13) +
            geom_vline(xintercept = c(20, 40, 60, 65)) +
            scale_color_manual(labels = c(away_team, home_team), 
                values = c("red", "blue")) +
            xlab("Minutes") + ylab("Expected Goals") +
            labs(title = xg_graph_title, subtitle = final_xg_score, 
                 caption = 'by @Matt_Barlowe')

#5v5 Running xG graph
xG_plot_5v5  <- ggplot(aes(x = game_seconds/60, y = running_xg), 
                    data = subset(xg_graph_df, 
                    xg_graph_df$team %in% c('run_home_5v5_xg',
                                            'run_away_5v5_xg'))) +
                    geom_step(aes(color = team)) +
                    geom_point(aes(x = game_seconds/60, y = run_home_5v5_xg), 
                        data = subset(pbp_df, pbp_df$event_type == "GOAL" & 
                        pbp_df$event_team == pbp_df$home_team & 
                        pbp_df$home_skaters == 5 & pbp_df$away_skaters == 5), 
                        shape = 13) +
                    geom_point(aes(x = game_seconds/60, y = run_away_5v5_xg), 
                        data = subset(pbp_df, pbp_df$event_type == 'GOAL' & 
                        pbp_df$event_team == pbp_df$away_team & 
                        pbp_df$home_skaters == 5 & 
                        pbp_df$away_skaters == 5), shape = 13) +
                    geom_vline(xintercept = c(20, 40, 60, 65)) +
                    scale_color_manual(labels = c(away_team, home_team), 
                       values = c("red", "blue")) +
                    xlab("Minutes") + ylab("Expected Goals") +
                    labs(title = xg_5v5_graph_title, subtitle = final_xg_score,
                         caption = 'by @Matt_Barlowe')


#head(arrange(groupd_player_xg, event_team, desc(xg)))
#head(arrange(groupd_player_xg, desc(event_team), desc(xg)))

#Creates tables of each team xG values and saves them to plots
away_xG_table <- arrange(subset(groupd_player_xg, 
                        groupd_player_xg$event_team == away_team), desc(xg))

home_xG_table <- arrange(subset(groupd_player_xg, 
                        groupd_player_xg$event_team == home_team), desc(xg))

away_table <- grid.arrange(tableGrob(away_xG_table))
home_table <- grid.arrange(tableGrob(home_xG_table))



#mirrors the y locations for the home and away teams on the offsides from where
#they will be graphed so the correct Ice Locations will be shown on the xG 
#location graphs
xG_location_graph <- fenwick_pbp
xG_location_graph$coords_y <- ifelse(xG_location_graph$is_home == 1 & 
                            xG_location_graph$coords_x > 0,
                            -xG_location_graph$coords_y, 
                            xG_location_graph$coords_y)
xG_location_graph$coords_y <- ifelse(xG_location_graph$is_home == 0 & 
                            xG_location_graph$coords_x < 0,
                            -xG_location_graph$coords_y, 
                            xG_location_graph$coords_y)


#plots xG locations and values for each team
xg_locations_plot <- ggplot() +
    geom_point(aes(x = -abs(coords_x), y = coords_y, size = xG), 
        data = subset(fenwick_pbp, fenwick_pbp$event_team == home_team & 
        fenwick_pbp$event_type %in% c('SHOT', 'MISS')), 
        color = team_colors[home_team], shape = 0) +
    geom_point(aes(x = -abs(coords_x), y = coords_y, size = xG,
                   color = xG_location_graph$event_team), data = 
        subset(fenwick_pbp, fenwick_pbp$event_team == home_team & 
        fenwick_pbp$event_type %in% c('GOAL')), color = team_colors[home_team], 
        shape = 15) +
    geom_point(aes(x = abs(coords_x), y = coords_y, size = xG), 
        data = subset(fenwick_pbp, fenwick_pbp$event_team == away_team & 
        fenwick_pbp$event_type %in% c('SHOT', 'MISS')), 
        color = team_colors[away_team], shape = 0) +
    geom_point(aes(x = abs(coords_x), y = coords_y, size = xG), data = 
        subset(fenwick_pbp, fenwick_pbp$event_team == away_team & 
        fenwick_pbp$event_type %in% c('GOAL')), color = team_colors[away_team],
        shape = 15) +
    labs(title = xg_locations_title, subtitle = final_xg_score, 
         caption = 'by @Matt_Barlowe') + 
    geom_segment(aes(x = -87.95, y = -39, xend = -87.95, yend = 39), color = 'red') +
    geom_segment(aes(x = 87.95, y = -39, xend = 87.95, yend = 39), color = 'red') +
    geom_segment(aes(x = 0, y = -42, xend = 0, yend = 42), color = 'red') +
    geom_segment(aes(x = -26, y = -42, xend = -26, yend = 42), color = 'blue') +
    geom_segment(aes(x = 26, y = -42, xend = 26, yend = 42), color = 'blue') +
    geom_segment(aes(x = -75, y = 42, xend = 75, yend = 42)) +
    geom_segment(aes(x = -75, y = -42, xend = 75, yend = -42)) +
    geom_curve(aes(x = -75, y = -42, xend = -99, yend = -25), curvature = -0.4) +
    geom_curve(aes(x = -75, y = 42, xend = -99, yend = 25), curvature = 0.4) +
    geom_segment(aes(x = -99, y = -25, xend = -99, yend = 25)) +
    geom_curve(aes(x = 75, y = -42, xend = 99, yend = -25), curvature = 0.4) +
    geom_curve(aes(x = 75, y = 42, xend = 99, yend = 25), curvature = -0.4) +
    geom_segment(aes(x = 99, y = -25, xend = 99, yend = 25)) +
    geom_segment(aes(x = -75, y =42, xend = 75, yend = 42)) +
    geom_segment(aes(x = -75, y = -42, xend = 75, yend = -42)) +
    geom_circle(aes(x0 = 0, y0 = 0, r = 15), color = 'blue') +
    geom_circle(aes(x0 = 65.95, y0 = 22, r = 15), color = 'red') +
    geom_circle(aes(x0 = 65.95, y0 = -22, r = 15), color = 'red') +
    geom_circle(aes(x0 = -65.95, y0 = 22, r = 15), color = 'red') +
    geom_circle(aes(x0 = -65.95, y0 = -22, r = 15), color = 'red') +
    geom_segment(aes(x = -91.61, y = -3, xend = -91.61, yend = 3), color = 'red') +
    geom_segment(aes(x = 91.61, y = -3, xend = 91.61, yend = 3), color = 'red') +
    geom_segment(aes(x = 91.61, y = -3, xend = 87.95, yend = -3), color = 'red') +
    geom_segment(aes(x = 91.61, y = 3, xend = 87.95, yend = 3), color = 'red') +
    geom_segment(aes(x = -91.61, y = -3, xend = -87.95, yend = -3), color = 'red') +
    geom_segment(aes(x = -91.61, y = 3, xend = -87.95, yend = 3), color = 'red') +
    geom_point(aes(x = c(65.95, 65.95, -65.95, -65.95), y = c(22, -22, 22, -22)), 
               color = 'red', size = 2) +
    geom_point(aes(x = 0, y= 0), color = 'blue', size = 2) +
    theme_void() +
    geom_point(aes(x = c(-21, -21, 21, 21), y = c(22, -22, 22, -22)), 
               color = 'red', size = 2) +
    scale_y_continuous(limits = c(-42, 42))
    
#saves all the plots to png files and folder labeled with game number
dir.create(paste0('~/HockeyStuff/xGGameBreakdowns/2018/', str_trim(game_number)))
setwd(paste0('~/HockeyStuff/xGGameBreakdowns/2018/', str_trim(game_number)))
ggsave('RunningxG.png', plot = xG_plot_all_sits, height = 5)
ggsave('RunningxG5v5.png', plot = xG_plot_5v5, height = 5)
ggsave('xGlocations.png', plot = xg_locations_plot, height = 4)
ggsave('xGHome.png', home_table)
ggsave('xGAway.png',  away_table)
write_delim(pbp_df, game_number, delim = '|')
    