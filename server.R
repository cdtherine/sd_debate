library(shiny)
library(ggplot2)
library(statar)
library(shinyWidgets)
library(broom)
library(dplyr)
library(scales)

# cleaning data
debate_partic <- read.csv("partic.csv")
gender_ind <- read.csv("gender_ind.csv")
gender_ind <- na.omit(gender_ind) 
gender_ind$g <- gender_ind$gender
gender_ind$gender <- ifelse(gender_ind$g == 1, "F", "M")
gender_ind$w <- gender_ind$win
gender_ind$win <- ifelse(gender_ind$w == 1, "F", "M")

function(input, output) {
  
  # graph for participation rates over time
  output$partic <- renderPlot({
 
    validate(
      need(input$tournament, "Please select a tournament."),
      need(input$event, "Please select an event."),
      need(input$division, "Please select a division."),
      need(input$year, "Please select a year.")
    )
    
    p <- debate_partic %>% 
      group_by(tournament, year, event, division) %>% 
      summarise(participants = sum(participants)) %>%
      filter(tournament %in% input$tournament) %>%
      filter(event %in% input$event) %>%
      filter(division %in% input$division) %>%
      filter(year %in% input$year) 
    
    ggplot(p, aes(x = year, y = participants)) + 
      stat_smooth(method = "lm", se = FALSE, aes(color = "red"), show.legend = FALSE) +  
      stat_binmean(geom = "point") + 
      theme_minimal()
  })
  
  # text statistics for participation rates
  output$pstat <- renderText({
    if(is.null(input$tournament) | is.null(input$event) | 
       is.null(input$division) | is.null(input$year)) return(NULL)
    
    p <- debate_partic %>% 
      group_by(tournament, year, event, division) %>% 
      summarise(participants = sum(participants)) %>%
      filter(tournament %in% input$tournament) %>%
      filter(event %in% input$event) %>%
      filter(division %in% input$division) %>%
      filter(year %in% input$year) 
    
    # regression of participation by year
    reg <- lm(data = p, participants ~ year) %>%
      tidy()
    
    # get slope
    slope <- reg %>%
      slice(2) %>%
      pull("estimate") %>%
      round(3)
    
    # get p-value of slope
    p_value <- reg %>%
      slice(2) %>%
      pull("p.value") %>%
      round(5)
    
    paste0("The average number of participants in an event is changing at a rate of ", slope, 
    " participants per year with p = ", p_value, ".")
    
  })
  
  # bar plot for gender statistics 
  output$gender <- renderPlot({
    
    validate(
      need(input$gtournament, "Please select a tournament."),
      need(input$gevent, "Please select an event."),
      need(input$gdivision, "Please select a division."),
      need(input$gyear, "Please select a year."), 
      need(input$gjudge, "Please select judge gender.")
    )
    
    p <- gender_ind %>% 
      filter(tournament %in% input$gtournament) %>%
      filter(event %in% input$gevent) %>%
      filter(division %in% input$gdivision) %>%
      filter(year %in% input$gyear) %>%
      filter(judge_gender %in% input$gjudge)
    
    # show graph by radio input
    if (input$gselect == "pts"){
      ggplot(p, aes(x=pts, y=..density.., fill=gender)) + 
        geom_histogram(alpha=0.2, position="identity", binwidth=1) +
        geom_density(alpha=0.4, adjust=2) +
        theme_minimal()
    }
    
    else if (input$gselect == "win"){
      ggplot(p, aes(x=win, fill=win)) +
        geom_bar(aes(y=..count../sum(..count..)), width=0.5) +
        scale_y_continuous(labels=percent_format()) + 
        theme_minimal()
    }
    
    else{
      ggplot(p, aes(x=gender, fill=gender)) +
        geom_bar(aes(y=..count../sum(..count..)), width=0.5) +
        scale_y_continuous(labels=percent_format()) + 
        theme_minimal()
    }
    
  })
  
  # line plot for gender statistics by year
  output$by_year <- renderPlot(height = 200, {
    
    p <- gender_ind %>% 
      filter(tournament %in% input$gtournament) %>%
      filter(event %in% input$gevent) %>%
      filter(division %in% input$gdivision) %>%
      filter(year %in% input$gyear) %>%
      filter(judge_gender %in% input$gjudge)
    
    # show graph by radio input
    if (input$gselect == "win"){
      ggplot(p, aes(x = year, y = w)) + 
        stat_binmean(geom = "line") + 
        theme_minimal()
    }
    
    else if (input$gselect == "part"){
      ggplot(p, aes(x = year, y = g)) + 
        stat_binmean(geom = "line") + 
        theme_minimal()
    }

  })
  
  # text statistics for gender
  output$gstat <- renderText({
    if(is.null(input$gtournament) | is.null(input$gevent) | 
       is.null(input$gdivision) | is.null(input$gyear) |is.null(input$gjudge)) return(NULL)
    
    p <- gender_ind %>% 
      filter(tournament %in% input$gtournament) %>%
      filter(event %in% input$gevent) %>%
      filter(division %in% input$gdivision) %>%
      filter(year %in% input$gyear) %>%
      filter(judge_gender %in% input$gjudge)
    
    # speaker pt avg for women
    f_mean <- mean(data.matrix(p[p$g == 1, "pts"])) %>%
      round(2)
    
    # speaker pt avg for men
    m_mean <- mean(data.matrix(p[p$g == 0, "pts"])) %>%
      round(2)
    
    # stats for speaker pts
    reg <- lm(data = p, pts ~ g) %>%
      tidy()
    
    p_value <- reg %>%
      slice(2) %>%
      pull("p.value") %>%
      round(5)
    
    # stats for wins
    win_mean <- mean(p$w) %>%
      round(4) * 100
    
    reg_win <- lm(data = p, w ~ g) %>%
      tidy()
    
    p_value_win <- reg_win %>%
      slice(2) %>%
      pull("p.value") %>%
      round(5)
    
    # stats for gendered participation rate
    g_mean <- mean(p$g) %>%
      round(4) * 100
    
    p_value_g <- prop.test(x = sum(p$g), n = nrow(p), p = 0.5, 
                           correct = FALSE)
    
    # show text by radio input
    if (input$gselect == "pts") {
      paste0("Men receive ", m_mean, " speaker points on average, and women receive ",
             f_mean, " speaker points on average. This makes a difference of ", 
             (f_mean - m_mean) %>% round(2),
             " speaker points with p = ", p_value, ".")
    }
    
    else if (input$gselect == "win"){
      paste0("Men win ", (100 - win_mean), "% of rounds, and women win ", 
             win_mean, "% of rounds. This makes a difference of ", 
             (win_mean - (100 - win_mean)) %>% round(2),
             "% with p = ", p_value_win, ".")
    }
    
    else{
      paste0("Men make up ", (100 - g_mean), "% of debaters, and women make up ",
             g_mean, "% of debaters. This makes a difference of ", 
             (g_mean - (100 - g_mean)) %>% round(2),
             "% with p = ", p_value_g$p.value %>% round(5), ".")
    }
    
  })
}