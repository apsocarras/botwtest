library(tuber)
library(tidyverse)
library(lubridate)
library(purrr)
library(stringr)
library(httr)
library(readxl)
library(tidyjson)
library(roperators)


### API LOG-IN info ### 
#Web Client ID: 750567258427-2msp8ndrkiiumuv5i40ni76cai7129dd.apps.googleusercontent.com
#Web Client Secret: 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
#Desktop Client ID: 750567258427-tf07hsleu2ov7sp6fcp538cg7itkg7ii.apps.googleusercontent.com
#Desktop Client Secret: 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

client_id <- "750567258427-tf07hsleu2ov7sp6fcp538cg7itkg7ii.apps.googleusercontent.com"
client_secret <- "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"
  
yt_oauth(
  app_id = client_id, 
  app_secret = client_secret,
  token = '')

#### USING TUBER + REDDIT SHEET to create botw_all.csv -- info on episodes ####

# botw_playlist_id <- str_split(
#   string = "https://www.youtube.com/playlist?list=PLJ_TJFLc25JR3VZ7Xe-cmt4k3bMKBZ5Tm", 
#   pattern = "=",
#   n = 2, simplify = TRUE)[,2]
# 
# botw1_raw <- get_playlist_items(filter = 
#            c(playlist_id = botw_playlist_id),
#            part = "snippet",
#            max_results = 100,
#            page_token = "EAAaBlBUOkNESQ",
#            simplify = TRUE)                 # There was an issue trying to get all 150 playlist items in one call
# 
# botw2_raw <- get_playlist_items(filter = 
#           c(playlist_id = botw_playlist_id),
#           part = "snippet",
#           max_results = 50,
#           simplify = FALSE)                 # Why did this have to be FALSE? I remember there was a reason but forget what
# 
# 
# botw1_tib <- botw1_raw %>% select(date = snippet.publishedAt, 
#     title = snippet.title, 
#     description = snippet.description, 
#     video_id = snippet.resourceId.videoId) %>%
#     rowid_to_column("pl_order") %>%
#     mutate(pl_order = rev(as.integer(pl_order))) %>% 
#     arrange(pl_order)
# 
# botw2_list <- list()
#   
# for (j in c(1:50)) {
#   info <- botw2_raw[["items"]][[j]]$snippet[c("publishedAt", "title", "description", "resourceId")]
#   botw2_list[[j]] <- info
# }
# 
# 
# botw2_tib <- as_tibble(botw2_list, .name_repair = "universal") %>%
#   rename_with(~ gsub("...", "",.x)) %>% 
#   mutate(categories = c("date","title","description","resource")) %>%
#   select(categories,1:50) %>% 
#   pivot_longer(names_to = "pl_order", cols = c(2:51)) %>%
#   mutate(pl_order = rev(as.integer(pl_order)) + 70) %>% 
#   pivot_wider(names_from = categories) %>% 
#   unnest(cols = 2:5) %>% 
#   filter(!grepl("youtube#", resource)) %>%
#   mutate(video_id = as.character(resource), .keep = c("unused"), 
#          title = replace(title, title == "Deleted video", 
#             "Best of the Worst: Diamond Cobra vs the White Fox")) %>% 
#   arrange(pl_order)
# 
# 
# botw_tib <- full_join(botw1_tib,botw2_tib) %>%
#   mutate(ep_num = 0, subseries = "", holiday = "")
#      
# non_episodes <- botw_tib %>% 
#   filter(!grepl("Best of the Worst(:| Episode| Spotlight)", title))
# 
# episodes <- botw_tib %>% 
#   filter(!title %in% non_episodes$title) %>% 
#   mutate(ep_num = row_number())
# 
# botw <- full_join(non_episodes, episodes) %>% 
#   mutate(ep_num = ifelse(ep_num == 0, NA,ep_num)) %>%
#          arrange(pl_order) %>%
#   select(pl_order,ep_num, date:holiday)
# 
# #for adjusting subseries
# spotlight_other <- c(27,28,57,60,83,93,96)
# main_other <- c(1,18,74,87)
# random_other <- c(37,48,62,74,91,108,109)
# 
# botw <- botw %>% 
#   mutate (subseries = case_when(
#         is.na(ep_num) ~ "extras",
#         grepl(",", title) ~ "main",
#         ep_num == 64 ~ "bl_spine, spotlight",
#         grepl("[Ww]heel",description) | grepl("[Ww]heel", title) ~ "wheel",
#         grepl("[Ss]pine", description) | grepl("[Ss]pine", title) ~ "bl_spine",
#         grepl("([Pp]linketto | ball)", description) | grepl("[Pp]linketto", title) ~ "plinketto",
#         grepl("[Ss]potlight", description) | grepl("[Ss]potlight", title) ~ "spotlight",
#         ep_num %in% spotlight_other ~ "spotlight",
#         ep_num %in% main_other ~ "main",
#         ep_num %in% random_other ~ "random_other", 
#         ep_num == 97 ~ "bl_spine",
#         ep_num == 106 ~ "main"))
# 
# 
# # ERROR: SWHS Two-Part episode was counted as two separate episodes. 
#  botw <- botw %>% 
#      mutate(ep_num = case_when(ep_num < 28 ~ ep_num, 
#                           ep_num >= 28 ~ ep_num-1))
#  
#  # other columns 
#  
# botw <- botw %>% 
#    mutate(holiday = case_when(grepl("(Christmas| Holiday |-mas)", title) ~ "christmas",
#                               grepl("Halloween", description) & subseries != "wheel" ~ "halloween", 
#                               grepl("Hollywood Cop", title) ~ "tums festival", 
#                               is.na(holiday) ~ "none"), 
#           date = as_datetime(date))
# 
# fanPath <- "~/R/BestoftheWorst/data/BoTW Spreadsheet V2 .xlsx"
# 
# get_duration <- function(time) { # remove junk ymd data from length column 
#   time %>% 
#     str_split(" ") %>%
#     map_chr(2) %>% 
#     lubridate::hms() 
#   } # we'll need this for the next two sheets
# 
# 
# fansheet_1 <- Filter(function(x)!all(is.na(x)), 
#                      read_excel(fanPath, sheet = 1, col_names = TRUE))[,1:23]
# for (i in c(8:15)) {
#   fansheet_1[[i]] <- case_when(grepl("✓", fansheet_1[[i]]) ~ names(fansheet_1)[[i]], 
#                                grepl("X", fansheet_1[[i]]) ~ "", 
#                                TRUE ~ fansheet_1[[i]]) } # TRUE is the else condition
# 
# fansheet_2 <- Filter(function(x)!all(is.na(x)), 
#                      read_excel(fanPath, sheet = 2, col_names = TRUE))
#   for (i in c(3:8)) {
#       fansheet_2[[i]] <- case_when(grepl("✓", fansheet_2[[i]]) ~ names(fansheet_2)[[i]], 
#                                grepl("X", fansheet_2[[i]]) ~ "", 
#                                TRUE ~ fansheet_2[[i]]) } 
# 
# main_eps <- fansheet_1 %>% 
#   rename_all(tolower) %>% 
#   rename(theme = `theme / gimmick`, best = `best of the worst`, 
#          worst = `worst of the worst`, destruction_method = `method of destruction`,
#          date = `date released`) %>% 
#   unite("films", 2:5, sep = ", ", remove = TRUE, na.rm = TRUE) %>% 
#   unite("panelists", mike:guests, sep = " ", remove = TRUE, na.rm = TRUE) %>% 
#   mutate(panelists = str_squish(str_remove_all(panelists, "from Canada|&")),
#          duration = get_duration(length),
#          unanimous = case_when(grepl("✓", `unanimous win`) ~ "yes", 
#                                grepl("X", `unanimous win`) ~ "no", 
#                                TRUE ~ `unanimous win`)) %>%
#   select(episode, date, duration, films:best, unanimous, worst:destruction_method, 
#          -`youtube links`, -`# gimmik`)
#     
# spotlight_eps <- fansheet_2 %>% 
#   rename_all(tolower) %>%
#   unite("panelists", mike:guests, sep = " ") %>% 
#   mutate(theme = "spotlight",
#          panelists = str_squish(str_remove_all(panelists, "from Canada")), 
#          duration = get_duration(length), 
#          date = `date released`,
#          films = film) %>% 
#  select(episode, date, duration, films, theme, panelists, editor)
# 
# all_eps <- full_join(main_eps, spotlight_eps) %>% 
#   mutate(date = as_datetime(date)) %>%
#   rename(ep_num = episode) %>% 
#   arrange(ep_num) 
# 
# botw_all <- full_join(botw, all_eps %>% select(-date), by = c("ep_num")) %>% 
#   mutate(link = paste("https://www.youtube.com/watch?v=", video_id, sep = "")) 
# YOUTUBE LINKS EASILY GENERABLE FROM VIDEO_ID COLUMN!! See above 
# youtube_links <- hyperlinks %>%
#   filter(grepl("youtube", token)) %>%
#   mutate(ep_num = as.numeric(str_remove(character, "Episode "),
#                   .keep = c("unused")),
#          link = token) %>%
#   select(ep_num, link) %>%
#   arrange(ep_num)
# 
# missing_links <- tibble(ep_num = c(76, 92, 93, 105:108),
#                         link = c("https://www.youtube.com/watch?v=6hHyn29O81k",
#                                  "Video was removed due to copyright",
#                                  "https://www.youtube.com/watch?v=-F0jtrV3RxI",
#                                  "https://www.youtube.com/watch?v=R5xa7r6oIBQ",
#                                  "https://www.youtube.com/watch?v=5jPtkjxU5jg",
#                                  "https://www.youtube.com/watch?v=FBEYOlXNAC8",
#                                  "https://www.youtube.com/watch?v=HgNKJhT74jU"))
# 
# youtube_links <- full_join(missing_links, youtube_links) %>% arrange(ep_num)
# 
# botw_all <- left_join(botw_all, youtube_links, by = c("ep_num")) %>%
botw_all <- as_tibble(read_csv("data/botw_all.csv")) %>% 
  mutate(date = mdy_hm(date))

#### TRANSCRIPTS ####
# See data/botw_transcripts.Rmd w/ Python YTApi -> json -> data/allcaptions2.csv ###
# tidy_captions <- read_json("data/captions.json")
# 
# tidycap_tbl <- tidy_captions %>%
#   gather_array %>% 
#   spread_values(text = jstring("text"),
#                 start = jstring("start"),
#                 duration = jstring("duration"),
#                 ep_num = jstring("ep_num")) %>% 
#   as_tibble %>% 
#   select(ep_num, text, start, duration)
# tidycap_tbl <- tidycap_tbl %>% mutate(ep_num = as.integer(ep_num))
#
# tidy_restr_caps <- read_json("data/restr_caps.json")
# tidyrcap_tbl <- tidy_restr_caps %>%
#   gather_array %>%
#   spread_values(text = jstring("text"),
#                 start = jstring("start"),
#                 duration = jstring("duration"),
#                 ep_num = jstring("ep_num")) %>%
#   as_tibble %>%
#   select(ep_num, text, start, duration)
# tidyrcap_tbl <- tidyrcap_tbl %>% mutate(ep_num = as.integer(ep_num), 
#                                         start = as.numeric(start), 
#                                         duration = as.numeric(duration))
# 
# tidycap <- as_tibble(read_csv("data/captions.csv")) 
# 
# write_excel_csv(full_join(tidycap,tidyrcap_tbl), "data/allcaptions.csv")
# allcap <- as_tibble(read_csv("data/allcaptions.csv")) 
# allcap <- allcap %>% filter(!grepl("transcribe", text))
# 
allcap <- as_tibble(read_csv("data/allcaptions2.csv"))



#### DATE CORRECTIONS (botw_all2.xlsx) #### 
# Previously was date added to playlist, NOT upload date. Just use tuber again
# date_check <- character(121)
# for (i in seq_along(botw_all$pl_order)) {
# 
#   out <- tuber::get_video_details(botw_all$video_id[[i]],
#                                           part = "snippet")$items[[1]]$snippet$publishedAt
#   if (!is.null(out)) {
#     date_check[[i]] <- out
#   }
# 
# }
# dc_df <- as_tibble(ymd_hms(date_check))
# colnames(dc_df) <- c("date")
# dc_df <- dc_df %>% mutate(pl_order = row_number()) %>% select(pl_order, date)
# 
# dc_df$date[[47]] <- botw_all$date[[47]]
# dc_df$date[[103]] <- botw_all$date[[103]]
# dc_df$date[[104]] <- botw_all$date[[104]]
# 
# botw_all$date <- dc_df$date
# 
# writexl::write_xlsx(botw_all, "data/botw_all2.xlsx")


#### Final version: botw_all3.xlsx ####

botw_all2 %>% count(ep_num) %>% filter(n == 2) 
# 1.) Ep 27 is a literal two-parter where the first half is basically unrelated to the movie 
# Ep 63 is a single episode/upload but halfway through it morphs into a spotlight episode for Partners 
# Going forward it may be easier to list these as separate episodes: 27 & 27.5, 63 & 63.5
botw_all2 <- readxl::read_xlsx("data/botw_all2.xlsx")
botw_all3 <- botw_all2
botw_all3$ep_num[[33]] <- botw_all3$ep_num[[33]] + .5 
botw_all3$ep_num[[72]] <- botw_all3$ep_num[[72]] + .5

botw_all3$duration[[32]] <- "42M 54S"
botw_all3$duration[[33]] <- "43M 40S"
botw_all3$duration[[71]] <- "31M 45S"
botw_all3$duration[[72]] <- "38M 0S" # NOTE: this is different from the "start_end" value in botw_otherdata2, 
# which measures the start/end of discussion on the movie itself. This "duration" 
# value measures up to the end of the episode itself.  


# 2.) As mentioned when analyzing botw_all2 in botw_analviz.rmd, the date for the second episode 
# is incorrect. Failed to write this edit into the excel file. 

botw_all3[2,3] <- as_datetime("2013-02-01 00:00:00")
# writexl::write_xlsx(botw_all3,"data/botw_all3.xlsx")


#### Final Version: botw_otherdata2.xlsx #####
# 1.) As previously noted, need to edit two-parter ep_nums
botw_otherdata <- readxl::read_xlsx("data/botw_otherdata.xlsx")
botw_otherdata[botw_otherdata$movie == "Partners","ep_num"] <- 63.5
botw_otherdata2 <- rbind(botw_otherdata, botw_otherdata[botw_otherdata$ep_num == 27.0,]) # duplicate ep 27 row to make into 27.5
botw_otherdata2[360,"ep_num"] <- 27.5
botw_otherdata2[botw_otherdata2$ep_num == 27.0,"start_end"] <- "00:00 - 42:54"
botw_otherdata2 <- arrange(botw_otherdata2, ep_num) 

#  2.) Missing start_end for episodes 53, 56, 59

botw_otherdata2$start_end[botw_otherdata2$ep_num == 53] <- "00:00 - 29:43"
botw_otherdata2$start_end[botw_otherdata2$ep_num == 56] <- "00:00 - 38:52"
botw_otherdata2$start_end[botw_otherdata2$ep_num == 59] <- "00:00 - 39:20"

# 3.) Correct Diamond Cobra ep_num to 92, not 93 

botw_otherdata2$ep_num[grepl("Diamond",botw_otherdata2$movie)] <- 92

# 4.) Add start_end for ep. 82 

botw_otherdata2[botw_otherdata2$ep_num == 82, "start_end"] <- "00:00 - 33:15"


# writexl::write_xlsx(botw_otherdata2, "data/botw_otherdata2.xlsx")


#### Other Data On Episodes ####

botw_otherdata2 <- readxl::read_xlsx("data/botw_otherdata2.xlsx") 
botw_all3 <- readxl::read_xlsx("data/botw_all3.xlsx")

disc_segment <- botw_otherdata2 %>%
  filter(grepl("-", start_end)) %>% # removes movies/videos which weren't discussed
  separate(start_end, into = c("segment1", "segment2"), sep = ";", extra = "merge") %>%
  separate(segment1, into = c("startseg1", "endseg1"), sep = " - ") %>% 
  separate(segment2, into = c("startseg2", "endseg2"), sep = " - ") %>%
  mutate(across(startseg1:endseg2, ~if_else(str_count(.,":") < 2, paste0("00:", .),.))) %>%
  mutate(across(startseg1:endseg2, ~gsub(" ","",.))) %>% 
  # left_join(botw_all2[,c("ep_num","date")]) %>% -- make disc_intervals own df
  select(ep_num, movie, startseg1:endseg2) 

disc_interval <- disc_segment %>% 
  left_join(botw_all3[,c("ep_num","date")]) %>%
  filter(ep_num < 109) %>% # episode 109 missing from botw_all2.xlsx -- haven't set up sheet to automatically update yet
  mutate(int1 = as.interval(hms(endseg1) - hms(startseg1), start = date + hms(startseg1)), 
         int2 = as.interval(hms(endseg2) - hms(startseg2), start = date + hms(startseg2)), 
         disc_length = if_else(is.na(int2), int_length(int1), int_length(int1) + int_length(int2))) %>% 
  select(ep_num, movie, int1:disc_length)

disc_interval <- disc_interval %>%
  add_count(ep_num) %>% 
  group_by(ep_num) %>%
  mutate(movie_num = row_number())


#### Adding Time Data to Transcripts ####

# Plan: Get "start" column from every line of the transcripts, add to "date" column from botw_all3 (by ep_num)
# If this value falls into one of int1, int2 in disc_interval, assign "movie" column to the line 

allcaps2 <- readr::read_csv("data/allcaptions2.csv")

# First, though, we need to edit episodes 27 and 63 as we did earlier

ep_27 <- allcaps2 %>% filter(ep_num == 27)
ep_63 <- allcaps2 %>% filter(ep_num == 63)

ep_27[lag(ep_27$start) > ep_27$start,] # start of part 27.5 -- row 936

ep_27.5 <- ep_27[936:1775,] %>% mutate(ep_num = 27.5)
ep_27 <- ep_27[1:935,]
ep_63.5 <- ep_63[612:2738,] %>% mutate(ep_num = 63.5)
ep_63 <- ep_63[1:611,]

allcaps3 <- allcaps2 %>% 
  filter(!ep_num %in% c(27,63)) %>% 
  rbind(ep_27, ep_27.5, ep_63, ep_63.5) %>% 
  arrange(ep_num) 

# write_csv(allcaps3,"data/allcaptions3.csv")
# allcaps3 <- read_csv("data/allcaptions3.csv")

### Adding time/movie codes to episode 1
ep1_caps <- allcaps3 %>% filter(ep_num == 1)
ep_dates <- botw_all3 %>% select(ep_num, date) 

ep1_disc <- disc_interval %>% filter(ep_num == 1)


ep1_timecaps <- ep1_caps %>% 
  left_join(ep_dates, by = c("ep_num")) %>%
  # left_join(disc_interval) %>% -- computationally expensive from repeating rows?
  mutate(start = date + start, 
         segment = case_when(start < int_start(ep1_disc$int1[[1]]) ~ "intro",  
                           start %within% ep1_disc$int1[[1]] ~ ep1_disc$movie[[1]],
                           start %within% ep1_disc$int1[[2]] ~ ep1_disc$movie[[2]],
                           start %within% ep1_disc$int1[[3]] ~ ep1_disc$movie[[3]],
                           start > int_end(ep1_disc$int1[[3]]) ~ "conclusion"))

## Generalize: fewer/more than 3 movies in an episode; missing captions; two intervals given; iterate over all episodes

testcaps <- allcaps3 %>% 
  filter(ep_num %in% c(1,20,46,56,104)) %>% # 3 movie standard; two episodes with missing captions; spotlight; wheel >3
  left_join(ep_dates, by = c("ep_num")) %>% 
  mutate(start = date + start) %>% 
  select(ep_num:start)
 
#### Slow left_join method ####

testcaps %>%
  left_join(disc_interval) %>%
  mutate(segment = case_when(text == "Transcript Unavailable" ~ "(blank)",
                           (start < int_start(int1) & movie_num == 1) ~ "intro",
                           (start > int_end(int1) & movie_num == n) ~ "conclusion",
                           start %within% int1 ~ movie)) %>% 
  filter(!is.na(segment)) %>% 
  select(ep_num, text, start, int1,segment) %>% View()
  
## Okay...time to try with whole thing
## First need to adjust "date" for episode 63.5 -- right now it is the same as the date for episode 63, which will result in
## duplicating lines that occur in "both" 63 and 63.5

ep_dates <- select(botw_all3,ep_num,date) %>% drop_na()
ep_dates$date[ep_dates$ep_num == 63.5] %+=% ms(botw_all3$duration[!is.na(botw_all3$ep_num) & botw_all3$ep_num == 63])

finalcap <- allcaps3 %>% 
  left_join(ep_dates, by = c("ep_num")) %>%
  left_join(select(disc_interval,-c(int2,disc_length))) %>%
  mutate(start = date + start,
  segment = case_when(text == "Transcript Unavailable" ~ ".",
                           (start < int_start(int1) & movie_num == 1) ~ "intro",
                           (start > int_end(int1) & movie_num == n) ~ "conclusion",
                           start %within% int1 ~ movie)) %>%
  filter(!is.na(segment)) %>%
  select(ep_num, text, start, segment) 
  
  # write_csv(finalcap, "data/captions_final.csv")

#### OLD Mistake####
## hmmm why is finalcap bigger than allcaptions.csv? repeated rows?

# discrep <- anti_join(allcaps3[,1:2], finalcap[,1:2])
# 
# audio_desc <- finalcap[grep("\\[[aA-zZ]+\\]", finalcap$text),] ## want to find repeated rows which aren't just audio descriptions
# 
# sus_repeats <- finalcap %>% 
#   filter(!text %in% unique(audio_desc$text)) %>% 
#   group_by(text) %>% 
#   add_count() %>% 
#   filter(n > 1) %>% 
#   ungroup() %>%
#   distinct(ep_num, text, .keep_all = TRUE) %>% 
#   arrange(desc(stringr::str_length(text))) 
# 
# sus_repeats %>%  
#   distinct(ep_num,text, .keep_all = TRUE)  %>% View()
# 
# sus_repeats %>% filter(n == 2)
# 
# finalcap %>% filter(ep_num %in% c(63,63.5,93)) ## Culprits
# finalcap %>% distinct(ep_num, segment)
# # For some reason, episode 63 got copied twice into 63.5, and 93 got messed up from Diamond Cobra 
# # taking the place of "intro" segment and then duplicating lines of the episode
#       # Diamond Cobra was mislabeled as Ep 93 in botw_otherdata
#       # Ep 63.5 has same initial "start" value as 63 since 
# allcaps3 %>% filter(ep_num %in% c(63,63.5,93)) # Not present here 
# 


#### Now finalcap is *smaller* than allcaps3 ####

# caprownums <- allcaps3[,1:2] %>% mutate(row = row_number())
# 
# capswithna <- allcaps3 %>%
#   left_join(ep_dates, by = c("ep_num")) %>%
#   left_join(select(disc_interval,-c(int2,disc_length))) %>%
#   mutate(start = date + start,
#          segment = case_when(text == "Transcript Unavailable" ~ ".",
#                              (start < int_start(int1) & movie_num == 1) ~ "intro",
#                              (start > int_end(int1) & movie_num == n) ~ "conclusion",
#                              start %within% int1 ~ movie)) %>%
#   # filter(!is.na(segment)) %>%
#   select(ep_num, text, start, movie, int1, segment)

missing_ep <- anti_join(allcaps3[,"ep_num"],finalcap[,"ep_num"]) # disc_segment, disc_interval missing ep 82, due to "start" column in botw_otherdata2    


# missing_lines <- anti_join(caprownums,finalcap[,1:2]) # 
missing_lines <- anti_join(allcaps3[,1:2],finalcap[,1:2]) # These two are now the same with ep 82 added -- they are "being lost" "filter(!is.na(segment))"

inner_join(missing_lines, capswithna)

missing_lines <- inner_join(missing_lines[,1:2], capswithna) %>%
  mutate(gap = abs(round(int_start(int1) - start))) %>%
  group_by(start) %>%
  mutate(mingap = min(gap))

min_missing <- missing_lines %>% filter(gap == mingap) %>% mutate(segment = movie) %>% select(ep_num:start, segment)

# What if we made "gap intervals" and included them in the case_when for finalcap










#### Loop method (Surrender for now)#### 

# We want to add a counter column to disc_interval which will say which order movie a given movie is in an episode
# This will help us when creating the segment column 
# 
# disc_interval <- disc_interval %>%
#   group_by(ep_num) %>%
#   mutate(movie_num = row_number())
# 
# for (i in seq_along(testcaps$start)) {
#   
#   curr_ep <- testcaps$ep_num[[i]]
#   
#   movie_orders <- disc_interval$movie_num[disc_interval$ep_num == curr_ep]
#   
#   if (testcaps$text[[i]] == "Transcript Unavailable") {
#     
#     next
#     
#   } else {
#   
#       for (movnum in movie_orders) {
#         
#         movie1 <- disc_interval[disc_interval$ep_num == curr_ep,][1,]
#         curr_movie <- disc_interval[disc_interval$ep_num == curr_ep,][movnum,]
#         
#         print(curr_movie)
#         # 
#         #   if (testcaps$start[[i]] < int_start(movie1$int1)) {
#         #   
#         #     testcaps$segment[[i]] <- "intro"
#         #   
#         # } else if (testcaps$start[[i]] %within% curr_movie$int1) {
#         #   
#         #   testcaps$segment[[i]] <- curr_movie$movie
#         #   
#         # } else {
#         #   testcaps$segment[[i]] <- "conclusion"
#         # 
#         #} 
#       }
#   }
# } 
# 




















#### IMDB DATA ####
# fanPath <- "~/R/BestoftheWorst/data/BoTW Spreadsheet V2 .xlsx"
imdb_links<- tidyxl::xlsx_cells(fanPath, sheets = c(1,2)) %>% # see YouTube Links section for fanPath
  filter(!is.na(formula) & !is.na(character)) %>%
  mutate(formula = map(formula, tidyxl::xlex)) %>% 
  unnest(formula) %>% 
  filter(grepl("imdb", token)) %>% 
  select(character, token) 






