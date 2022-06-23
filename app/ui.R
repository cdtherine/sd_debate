library(shiny)
library(ggplot2)
library(statar)
library(shinyWidgets)
library(broom)
library(dplyr)
library(bslib)

tournaments <- tibble(
  "State" = "state",
  "Rushmore Quals" = "rushmore",
  "Northern Quals" = "northern",
  "Aberdeen Golden Eagle Cup" = "gec",
  "Brookings Bell" = "bell",
  "Watertown Speech Fiesta" = "fiesta",
  "George Mcgovern" = "mcgov",
  "Yankton Lewis and Clark" = "yankton",
  "Lincoln Silver Bowl" = "silver",
  "Roosevelt Sweetstakes" = "sweet",
  "Harrisburg Rushmore Challenge" = "harrisburg",
  "Washington Warrior" = "warrior",
  "CFCs" = "cfc",
  "Jefferson Cavalier" = "jeff",
  "Novice Tournaments" = "novice"
)

years <- tibble(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)
  
navbarPage("The State of SD Debate",
           
  theme = bs_theme(bootswatch = "flatly"),
  
  tabPanel("Participation Rates",
    fluidRow(
    sidebarPanel(
      h4("Participation Rates"),
      
      pickerInput("tournament", "Tournaments:", 
                  tournaments %>% select(-"CFCs", -"Novice Tournaments"),
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE,
                  selected = (tournaments %>% select(-"CFCs", -"Novice Tournaments"))),
      
      pickerInput("event", "Events:", 
                  c("Lincoln Douglas" = "LD",
                    "Public Forum" = "PF"),
                  multiple = TRUE,
                  selected = c("LD", "PF")),
      
      pickerInput("division", "Divisions:", 
                  c("Varsity" = "V",
                    "Junior Varsity" = "JV",
                    "Novice" = "N"),
                  multiple = TRUE,
                  selected = "V"),
      
      pickerInput("year", "Years:", 
                  years,
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE,
                  selected = years)
      
    ),
    
    mainPanel(
      
      textOutput("pstat"),
      plotOutput("partic")
      
    )
    )
  ),
  
  tabPanel("Gender Equity", 
    
    fluidRow(
    sidebarPanel(
      
      h4("Gender Equity"),
      
      prettyRadioButtons("gselect", "Statistic",
        choices = c("Speaker Points" = "pts", 
                    "Win Rate" = "win",
                    "Participation Rate" = "part"),
        selected = "pts"),
             
      pickerInput("gtournament", "Tournaments:", 
                  tournaments %>% select(-"State", -"Rushmore Quals", -"Northern Quals"),
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE,
                  selected = tournaments),
      
      pickerInput("gevent", "Events:", 
                  c("Lincoln Douglas" = "LD",
                    "Public Forum" = "PF"),
                  multiple = TRUE,
                  selected = c("LD", "PF")),
      
      pickerInput("gdivision", "Divisions:", 
                  c("Varsity" = "V",
                    "Junior Varsity" = "JV",
                    "Novice" = "N"),
                  multiple = TRUE,
                  selected = c("V")),
      
      pickerInput("gyear", "Years:", 
                  years,
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE,
                  selected = years),
       
      pickerInput("gjudge", "Judge Gender:", 
                  c("Male" = 0, "Female" = 1),
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE,
                  selected = c(0, 1))
      
    ),
     
    mainPanel(
      
      textOutput("gstat"), 
      plotOutput("gender"),
      plotOutput("by_year")
  
    )
    )
  ),
  
  tabPanel("Methods",
    h4("Methods"),
           
    h5("Participation Rates"),
    span("I used Python to scrape all available participant lists from "),
    a(href="https://www.tabroom.com/index/results/", 
      "Tabroom"),
    span(" for tournaments in South Dakota. The graph shows the average number of participants
         per event, only including Public Forum and Lincoln Douglas. Participants refers to 
         teams, so in PF, one participant means two people. The dataset includes 344 events 
         from 100 tournaments. Some things to keep in mind:"),
    tags$ul(
      tags$li("Some data is omitted due to how tournaments posted results on Tabroom. I  
            included tournaments that posted prelim records."), 
      tags$li("Tournaments typically used Joy of Tournaments before 2015, and some tournaments 
            did not migrate to Tabroom until later years."), 
      tags$li("For overall participant data, I excluded novice tournaments and CFCs.")
    ),
    
    h5("Gender Equity"),
    span("Similarly, I scraped all available round results from every round in South Dakota. 
         I then used Luke Mullen's "),
    a(href="https://github.com/lmullen/gender", 
      "genderdata package"),
    span(" to predict the gender of each debater and judge by first name, excluding results 
         without values. The resulting dataset has 28,988 observations. Again, some things
         to keep in mind:"),
    tags$ul(
      tags$li("Obviously, predicting gender is not entirely accurate, but this is the best 
              approximation."), 
      tags$li("Novice tournaments and CFCs are included now, since I'm analyzing 
              round-by-round data."), 
      tags$li("The state tournament, national qualifying tournaments, and most elimination 
              rounds are excluded because judges don't assign speaker points in them.")
    ),
    
    h5("Data"),
    span("All the code I wrote and datasets I compiled are here."),
    
    h5("Contact"),
    span("Catherine Liu, catherineliu@college.harvard.edu")
    
  ),
  
  tabPanel("Results",
    h4("Results"),
    
    h5("Participation Rates"),
    span("Overall, average participation rates in varsity events are declining with 
         statistical significance (p = 0.00145). Not including 2020 and 2021, which were partially 
         online due to COVID, average participation rates are still declining, but with less 
         significance (p = 0.07677). There's no evidence that participation rates are changing 
         for novice and junior varsity events, so the downward trend could be related to 
         retention moreso than initial recruitment."),
    
    h5("Gender Equity"),
    span("The South Dakota circuit seems fairly equitable by gender. Interestingly,
         women tend to slightly outspeak and win more rounds than men. However, there are still
         some statistics to take note of:"),
    tags$ul(
      tags$li("In almost every year, there was a higher proportion of women 
              in the novice division than in the varsity division. This might suggest 
              that the retention rate for women is lower than that of men. This also might explain
              why women slightly outperform men: those selecting to stay in the activity 
              could also be those who perform better."), 
      tags$li("Male judges award male debaters wins more often than female judges do, but
              they give men lower speaker points on average (and vice versa for female judges)."), 
      tags$li("During the online year, there were significantly more women competing than men 
              in the varsity division by a gap of 14.96%?!")
    )
    
  )
)
