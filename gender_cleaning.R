library(tidyselect)
library(stringr)
library(tidyr)
library(gender)
library(dplyr)
library(ggplot2)
library(statar)

### part 1: cleaning the data

# create standardized win column

debate$win <- ifelse(grepl("aff|pro", debate$Win, ignore.case=TRUE), "aff", "neg")

#remove unneeded columns

debate = select(debate, -"Unnamed: 0", -"Unnamed: 1", -"Unnamed: 6", -"Votes", -"...1",
                        -"Win", -"Unnamed: 4", -"Pro", -"Con", -"PRO", -"CON",
                        -"Pro  Points & Ranks", -"Con  Points & Ranks")

# fix year, division, round
debate$year <- strtoi(word(debate$year, 1))

debate$division <- ifelse(grepl("Varsity", debate$event_name) == TRUE |
                          grepl("STATE", debate$tournament) == TRUE | 
                          grepl("DISTRICT", debate$tournament) == TRUE, "V", NA)
                            
debate$division <- ifelse(grepl("Junior Varsity", debate$event_name) == TRUE |
                          grepl("JV", debate$event_name) == TRUE, "JV", debate$division)

debate$division <- ifelse(grepl("Novice", debate$event_name) == TRUE | 
                          grepl("NOVICE", debate$tournament) == TRUE, "N", debate$division)

debate = select(debate, -"event_name")

debate$round <- ifelse(grepl("\\d", debate$round) == TRUE, "prelim", "elim")

# standardize tournament names

debate$tournament <- ifelse(grepl("CHALLENGE", debate$tournament) == TRUE,
                                   "harrisburg", debate$tournament)
debate$tournament <- ifelse(grepl("NOVICE", debate$tournament) == TRUE,
                                   "novice", debate$tournament)
debate$tournament <- ifelse(grepl("STATE", debate$tournament) == TRUE,
                                   "state", debate$tournament)
debate$tournament <- ifelse(grepl("RUSHMORE DISTRICT", debate$tournament) == TRUE,
                                   "rushmore", debate$tournament)
debate$tournament <- ifelse(grepl("NORTHERN", debate$tournament) == TRUE,
                                   "northern", debate$tournament)
debate$tournament <- ifelse(grepl("GOLDEN", debate$tournament) == TRUE,
                                   "gec", debate$tournament)
debate$tournament <- ifelse(grepl("STATE", debate$tournament) == TRUE,
                                   "state", debate$tournament)
debate$tournament <- ifelse(grepl("BELL", debate$tournament) == TRUE,
                                   "bell", debate$tournament)
debate$tournament <- ifelse(grepl("FIESTA", debate$tournament) == TRUE,
                                   "fiesta", debate$tournament)
debate$tournament <- ifelse(grepl("MCGOVERN", debate$tournament) == TRUE,
                                   "mcgov", debate$tournament)
debate$tournament <- ifelse(grepl("LEWIS", debate$tournament) == TRUE,
                                   "yankton", debate$tournament)
debate$tournament <- ifelse(grepl("SWEETSTAKES", debate$tournament) == TRUE,
                                   "sweet", debate$tournament)
debate$tournament <- ifelse(grepl("WARRIOR INVITE", debate$tournament) == TRUE,
                                   "warrior", debate$tournament)
debate$tournament <- ifelse(grepl("CFC", debate$tournament) == TRUE |
                                     grepl("CONFERENCE", debate$tournament) == TRUE,
                                   "cfc", debate$tournament)
debate$tournament <- ifelse(grepl("CAVALIER", debate$tournament) == TRUE,
                                   "jeff", debate$tournament)
debate$tournament <- ifelse(grepl("SILVER", debate$tournament) == TRUE,
                            "silver", debate$tournament)

# fix speaker points 
  # merge all aff points columns into one
debate$aff_pts <- paste(debate$"Aff  Points", debate$"Aff  Points & Ranks", 
                        debate$"PRO  Points", debate$"Pro  Points")

  # remove nonnumeric
debate$aff_pts <- gsub("[^0-9.-]", " ", debate$aff_pts)
debate$aff_pts <- gsub("\\s+", " ", str_trim(debate$aff_pts))

  # count number of numbers, including decimal
debate$pts_ct <- str_count(debate$aff_pts, "\\d+\\.?\\d*")

  # separate values, split by first three and last three; this is for elims speaks
x <- strsplit(debate$aff_pts, " ") 
y <- lapply(x, head, n = 3)
z <- lapply(x, tail, n = 3)
avg_pts_1 <- sapply(y, function(y) mean(as.numeric(y[y != '0.0' & y != '0'])))
avg_pts_2 <- sapply(z, function(z) mean(as.numeric(z[z != '0.0' & z != '0'])))

      ##FIX RANKS !!!
      ##aff/neg pts and ranks remove below 5

  # set aff speaks
debate$aff_1_pts <- ifelse(debate$pts_ct == 1, debate$aff_pts, NA)
debate$aff_1_pts <- ifelse(debate$pts_ct == 2, gsub( " .*$", "", debate$aff_pts), debate$aff_1_pts)
debate$aff_1_pts <- ifelse(debate$pts_ct >= 3, avg_pts_1, debate$aff_1_pts)

debate$aff_2_pts <- ifelse(debate$pts_ct == 2, gsub("^\\S+\\s+", "", debate$aff_pts), NA)
debate$aff_2_pts <- ifelse(debate$pts_ct == 6, avg_pts_2, debate$aff_2_pts)

  # do the same thing for neg speaks
debate$neg_pts <- paste(debate$"Neg  Points", debate$"Neg  Points & Ranks", 
                        debate$"CON  Points", debate$"Con  Points")

debate$neg_pts <- gsub("[^0-9.-]", " ", debate$neg_pts)
debate$neg_pts <- gsub("\\s+", " ", str_trim(debate$neg_pts))

debate$pts_ct <- str_count(debate$neg_pts, "\\d+\\.?\\d*")

x <- strsplit(debate$neg_pts, " ") 
y <- lapply(x, head, n = 3)
z <- lapply(x, tail, n = 3)
avg_pts_1 <- sapply(y, function(y) mean(as.numeric(y[y != '0.0' & y != '0'])))
avg_pts_2 <- sapply(z, function(z) mean(as.numeric(z[z != '0.0' & z != '0'])))

debate$neg_1_pts <- ifelse(debate$pts_ct == 1, debate$neg_pts, NA)
debate$neg_1_pts <- ifelse(debate$pts_ct == 2, gsub( " .*$", "", debate$neg_pts), debate$neg_1_pts)
debate$neg_1_pts <- ifelse(debate$pts_ct >= 3, avg_pts_1, debate$neg_1_pts)

debate$neg_2_pts <- ifelse(debate$pts_ct == 2, gsub("^\\S+\\s+", "", debate$neg_pts), NA)
debate$neg_2_pts <- ifelse(debate$pts_ct == 6, avg_pts_2, debate$neg_2_pts)

# make numeric, fix negative values, remove rank values

debate$aff_1_pts <- abs(as.numeric(debate$aff_1_pts))
debate$aff_1_pts[debate$aff_1_pts < 5] <- NA

debate$aff_2_pts <- abs(as.numeric(debate$aff_2_pts))
debate$aff_2_pts[debate$aff_2_pts < 5] <- NA

debate$neg_1_pts <- abs(as.numeric(debate$neg_1_pts))
debate$neg_1_pts[debate$neg_1_pts < 5] <- NA

debate$neg_2_pts <- abs(as.numeric(debate$neg_2_pts))
debate$neg_2_pts[debate$neg_2_pts < 5] <- NA

# remove unneeded points columns now
debate = select(debate, -"Pro  Points", -"Con  Points", -"Aff  Points", -"Neg  Points",
                -"Aff  Points & Ranks", -"Neg  Points & Ranks", -"PRO  Points", -"CON  Points")

# separate PF teams

debate <- debate %>% 
  separate("Aff", c('aff_1', 'aff_2'), sep=" & ", extra = "drop", fill="right")

debate <- debate %>%
  separate("Neg", c('neg_1', 'neg_2'), sep=" & ", extra = "drop", fill="right")

# get first names of debaters

debate$aff_1_first <- word(debate$aff_1, 1)
debate$aff_2_first <- word(debate$aff_2, 1)
debate$neg_1_first <- word(debate$neg_1, 1)
debate$neg_2_first <- word(debate$neg_2, 1)
debate$judge_first <- word(debate$Judge, 1)

# predict gender of debaters

debate_gender <- gender(names = c(debate$aff_1_first, 
                                  debate$aff_2_first, 
                                  debate$neg_1_first, 
                                  debate$neg_2_first,
                                  debate$judge_first), 
                        years = c(1996, 2008), 
                        method = "ssa")

debate <- 
  distinct(merge(debate, 
                 debate_gender[ , c("name", "gender")], 
                 by.x = "aff_1_first", 
                 by.y = "name",
                 all.x = TRUE)) %>%
  rename(aff_1_gender = gender)

debate <- 
  distinct(merge(debate, 
                 debate_gender[ , c("name", "gender")], 
                 by.x = "aff_2_first", 
                 by.y = "name",
                 all.x = TRUE)) %>%
  rename(aff_2_gender = gender)

debate <- 
  distinct(merge(debate, 
                 debate_gender[ , c("name", "gender")], 
                 by.x = "neg_1_first", 
                 by.y = "name",
                 all.x = TRUE)) %>%
  rename(neg_1_gender = gender)

debate <- 
  distinct(merge(debate, 
                 debate_gender[ , c("name", "gender")], 
                 by.x = "neg_2_first", 
                 by.y = "name",
                 all.x = TRUE)) %>%
  rename(neg_2_gender = gender)

debate <- 
  distinct(merge(debate, 
                 debate_gender[ , c("name", "gender")], 
                 by.x = "judge_first", 
                 by.y = "name",
                 all.x = TRUE)) %>%
  rename(judge_gender = gender)

# turn gender columns into indicator variables

debate$aff_1_gender <- ifelse(debate$aff_1_gender == "female", 1, 0)
debate$aff_2_gender <- ifelse(debate$aff_2_gender == "female", 1, 0)
debate$neg_1_gender <- ifelse(debate$neg_1_gender == "female", 1, 0)
debate$neg_2_gender <- ifelse(debate$neg_2_gender == "female", 1, 0)
debate$judge_gender <- ifelse(debate$judge_gender == "female", 1, 0)

# add indicators for gender composition of PF and LD teams

  # for PF aff teams
debate$pf_aff_ff <- ifelse(debate$aff_1_gender + debate$aff_2_gender == 2, 1, 0) 
debate$pf_aff_ff <- ifelse(debate$event == "PF", debate$pf_aff_ff, NA)
                    
debate$pf_aff_mf <- ifelse(debate$aff_1_gender + debate$aff_2_gender == 1, 1, 0)
debate$pf_aff_mf <- ifelse(debate$event == "PF", debate$pf_aff_mf, NA)         

debate$pf_aff_mm <- ifelse(debate$aff_1_gender + debate$aff_2_gender == 0, 1, 0)
debate$pf_aff_mm <- ifelse(debate$event == "PF", debate$pf_aff_mm, NA)

  # for PF neg teams
debate$pf_neg_ff <- ifelse(debate$neg_1_gender + debate$neg_2_gender == 2, 1, 0)
debate$pf_neg_ff <- ifelse(debate$event == "PF", debate$pf_neg_ff, NA)

debate$pf_neg_mf <- ifelse(debate$neg_1_gender + debate$neg_2_gender == 1, 1, 0)
debate$pf_neg_mf <- ifelse(debate$event == "PF", debate$pf_neg_mf, NA)

debate$pf_neg_mm <- ifelse(debate$neg_1_gender + debate$neg_2_gender == 0, 1, 0)
debate$pf_neg_mm <- ifelse(debate$event == "PF", debate$pf_neg_mm, NA)

  # for LD aff teams
debate$ld_aff_f <- ifelse(debate$aff_1_gender == 1, 1, 0)
debate$ld_aff_f <- ifelse(debate$event == "LD", debate$ld_aff_f, NA)

  # for LD neg teams
debate$ld_neg_f <- ifelse(debate$neg_1_gender == 1, 1, 0)
debate$ld_neg_f <- ifelse(debate$event == "LD", debate$ld_neg_f, NA)

write.csv(debate, "gender_team.csv")

### for individual debaters
# PF stuff--for individual debaters
pf <- subset(debate, event == "PF")
x1 <- pf[c("event", "tournament", "aff_1_gender", "aff_1_pts", "division", "win", "judge_gender", "round", "year")] 
x1 <- rename(x1, "gender" = "aff_1_gender",
             "pts" = "aff_1_pts")
x1$win <- ifelse(x1$win == "aff", 1, 0)

x2 <- pf[c("event", "tournament", "aff_2_gender", "aff_2_pts", "division", "win", "judge_gender", "round", "year")]
x2 <- rename(x2, "gender" = "aff_2_gender",
             "pts" = "aff_2_pts")
x2$win <- ifelse(x2$win == "aff", 1, 0)

x3 <- pf[c("event", "tournament", "neg_1_gender", "neg_1_pts", "division", "win", "judge_gender", "round", "year")]
x3 <- rename(x3, "gender" = "neg_1_gender",
             "pts" = "neg_1_pts")
x3$win <- ifelse(x3$win == "neg", 1, 0)

x4 <- pf[c("event", "tournament", "neg_2_gender", "neg_2_pts", "division", "win", "judge_gender", "round", "year")]
x4 <- rename(x4, "gender" = "neg_2_gender",
             "pts" = "neg_2_pts")
x4$win <- ifelse(x4$win == "neg", 1, 0)

pf_gender <- rbind(x1, x2, x3, x4)

ld <- subset(debate, event == "LD")
x1 <- ld[c("event", "tournament", "aff_1_gender", "aff_1_pts", "division", "win", "judge_gender", "round", "year")] 
x1 <- rename(x1, "gender" = "aff_1_gender",
             "pts" = "aff_1_pts")
x1$win <- ifelse(x1$win == "aff", 1, 0)

x2 <- ld[c("event", "tournament", "neg_1_gender", "neg_1_pts", "division", "win", "judge_gender", "round", "year")]
x2 <- rename(x2, "gender" = "neg_1_gender",
             "pts" = "neg_1_pts")
x2$win <- ifelse(x2$win == "neg", 1, 0)

ld_gender <- rbind(x1, x2)

gender_ind <- rbind(pf_gender, ld_gender)

write.csv(gender_ind, "gender_ind.csv")


