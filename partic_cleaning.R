library(stringr)
library(ggplot2)
library(statar)
library(dplyr)

# fix year and division
debate_partic$year <- strtoi(word(debate_partic$year, 1))

debate_partic$division <- ifelse(grepl("Varsity", debate_partic$event_name) == TRUE |
                            grepl("STATE", debate_partic$tournament) == TRUE | 
                            grepl("DISTRICT", debate_partic$tournament) == TRUE, "V", NA)

debate_partic$division <- ifelse(grepl("Junior Varsity", debate_partic$event_name) == TRUE |
                            grepl("JV", debate_partic$event_name) == TRUE, "JV", debate_partic$division)

debate_partic$division <- ifelse(grepl("Novice", debate_partic$event_name) == TRUE | 
                            grepl("NOVICE", debate_partic$tournament) == TRUE, "N", debate_partic$division)

# standardize tournament names

debate_partic$tournament <- ifelse(grepl("CHALLENGE", debate_partic$tournament) == TRUE,
                                   "harrisburg", debate_partic$tournament)
debate_partic$tournament <- ifelse(grepl("NOVICE", debate_partic$tournament) == TRUE,
                                   "novice", debate_partic$tournament)
debate_partic$tournament <- ifelse(grepl("STATE", debate_partic$tournament) == TRUE,
                                   "state", debate_partic$tournament)
debate_partic$tournament <- ifelse(grepl("RUSHMORE DISTRICT", debate_partic$tournament) == TRUE,
                                   "rushmore", debate_partic$tournament)
debate_partic$tournament <- ifelse(grepl("NORTHERN", debate_partic$tournament) == TRUE,
                                   "northern", debate_partic$tournament)
debate_partic$tournament <- ifelse(grepl("GOLDEN", debate_partic$tournament) == TRUE,
                                   "gec", debate_partic$tournament)
debate_partic$tournament <- ifelse(grepl("STATE", debate_partic$tournament) == TRUE,
                                   "state", debate_partic$tournament)
debate_partic$tournament <- ifelse(grepl("BELL", debate_partic$tournament) == TRUE,
                                   "bell", debate_partic$tournament)
debate_partic$tournament <- ifelse(grepl("FIESTA", debate_partic$tournament) == TRUE,
                                   "fiesta", debate_partic$tournament)
debate_partic$tournament <- ifelse(grepl("MCGOVERN", debate_partic$tournament) == TRUE,
                                   "mcgov", debate_partic$tournament)
debate_partic$tournament <- ifelse(grepl("LEWIS", debate_partic$tournament) == TRUE,
                                   "yankton", debate_partic$tournament)
debate_partic$tournament <- ifelse(grepl("SWEETSTAKES", debate_partic$tournament) == TRUE,
                                   "sweet", debate_partic$tournament)
debate_partic$tournament <- ifelse(grepl("WARRIOR INVITE", debate_partic$tournament) == TRUE,
                                   "warrior", debate_partic$tournament)
debate_partic$tournament <- ifelse(grepl("CFC", debate_partic$tournament) == TRUE |
                                   grepl("CONFERENCE", debate_partic$tournament) == TRUE,
                                   "cfc", debate_partic$tournament)
debate_partic$tournament <- ifelse(grepl("CAVALIER", debate_partic$tournament) == TRUE,
                                   "jeff", debate_partic$tournament)
debate_partic$tournament <- ifelse(grepl("SILVER", debate_partic$tournament) == TRUE,
                                   "silver", debate_partic$tournament)

# save as csv 

write.csv(debate_partic,"partic.csv")
        

