library(shinydashboard)
library(pitchRx)
library(data.table)
library(plyr)
library(fmsb)
library(shiny)
library(ggplot2)

season = fread("2016_Modified.csv")
season = as.data.frame(season)

season$inside = ifelse(season$batterHand == 'R' & season$px < 0, 'inside',
                       ifelse(season$batterHand == 'R' & season$px >= 0, 'outside',
                              ifelse(season$batterHand == 'L' & season$px < 0,'outside',
                                     ifelse(season$batterHand == 'L' & season$px >= 0,'inside','error'))))
season$ahead = ifelse(season$strikes > season$balls, 'ahead',
                          ifelse(season$strikes < season$balls, 'behind', 'even'))

season$ahead_prev = season$ahead[c(length(season$ahead),1:(length(season$ahead)-1))]
season$pitchResult = ifelse(season$pitchResult %in% c('B','BID','HBP'), 'B','S')

season$prev = season$pitchResult[c(length(season$pitchResult),1:(length(season$pitchResult)-1))]
season$prev_type = season$pitchType[c(length(season$pitchResult),1:(length(season$pitchResult)-1))]

season$count = paste(season$ahead, 'and', season$pitchResult)
season$count_prev = paste(season$ahead_prev, 'and', season$prev)

season_alt = season[season$pitchType != 'UN',c('pitchType','spinRate','releaseVelocity',
                                               'probCalledStrike','pz')]
season_alt = season_alt[season_alt$pitchType != 'PO',]
season_alt = season_alt[season_alt$pitchType != 'IN',]
season_alt = season_alt[season_alt$pitchType != 'AB',]
season_alt = season_alt[season_alt$pitchType != 'AS',]

season_alt = season_alt[!is.na(season_alt$spinRate),]
season_alt = season_alt[!is.na(season_alt$releaseVelocity),]
season_alt = season_alt[!is.na(season_alt$probCalledStrike),]

season$high = ifelse(season$pz > mean(season_alt$pz), 'high','low')
season$prev_high = season$high[c(length(season$pitchResult),1:(length(season$pitchResult)-1))]
season$prev_inside = season$inside[c(length(season$pitchResult),1:(length(season$pitchResult)-1))]

season$quadrant = paste(season$high, 'and', season$inside)
season$quadrant_prev = paste(season$prev_high, 'and', season$prev_inside)

season$risp = ifelse(season$manOnSecond == TRUE | season$manOnThird == TRUE,TRUE,FALSE)

season = season[season$pitchType != 'UN',]
season = season[season$pitchType != 'PO',]
season = season[season$pitchType != 'IN',]
season = season[season$pitchType != 'AB',]
season = season[season$pitchType != 'AS',]

season = na.omit(season)

pitchers1 = aggregate(season[,c('releaseVelocity','spinRate','probCalledStrike')],
                      by = list(season$pitcher, season$pitchType), FUN = mean)
pitchers2 = aggregate(season[,'probCalledStrike'], by = list(season$pitcher, season$pitchType), FUN = sd)

pitchers = merge(pitchers1, pitchers2, all = TRUE, by = c('Group.1', 'Group.2'))
colnames(pitchers)[6] = 'ProbCalledStrikeDev'

pitchers = na.omit(pitchers)

season$gameDate2 = strptime(season$gameDate, '%m/%d/%Y %H:%M')
season$week = strftime(season$gameDate2, '%W')

sequencing = season[season$balls != 0 | season$strikes != 0,]

ui <- dashboardPage(
  dashboardHeader(title = "Pitching Matchup"),
  dashboardSidebar(
    sidebarMenu(id = 'menu',
      menuItem("Pitch Execution", tabName = "execution", icon = icon("dashboard")),
      menuItem("Pitch Sequencing", icon = icon("th"), tabName = "sequencing"),
      menuItem('User Reference', icon = icon('cog'), tabName = 'reference')
    ),
    conditionalPanel(condition = "input.menu=='execution' | input.menu == 'sequencing'",
                     htmlOutput("selectpitchers"),
                     selectInput('bathand',
                                 'Batter Handedness:',
                                 c('Both','L','R')
                     ),
                     selectInput('RISP',
                                 'RISP:',
                                 c('Both',TRUE,FALSE)
                     ),
                     
                     selectInput('Outs',
                                 'Outs:',
                                 c('Any','0','1','2')),
                     
                     selectInput('strikes',
                                 'Strikes:',
                                 c('Any','0','1','2')),
                     
                     selectInput('balls',
                                 'Balls:',
                                 c('Any','0','1','2','3'))
    ),
    conditionalPanel(condition = "input.menu=='execution'",
                     htmlOutput("selectUI"))
    ),
  dashboardBody(
    conditionalPanel(condition = "input.menu=='execution'",
                     
                     fluidRow(
                       box(plotOutput("distPlot"), title = 'Pitch Selection'),
                       box(plotOutput("heatplot"), title = 'Pitch Location')
                     ),
                     fluidRow(
                       box(tableOutput('datatable'),
                           tableOutput('datatable2'),
                           title = 'Pitch Results and Average Velocities'
                          ),
                       box(plotOutput("spiderplot"), title = 'Pitch Advanced Metrics')
                     )
                     ),
    conditionalPanel(condition = 'input.menu == "sequencing"',
                     fluidRow(
                       box(plotOutput('pitch_type_sequencing'),
                           title = 'Pitch Type'),
                       box(plotOutput('pitch_quadrant_sequencing'),
                           title = 'Pitch Quadrant')),
                     fluidRow(
                       box(plotOutput("pitch_count_sequencing"),
                           title = 'Pitch Selection by Count'),
                       box(plotOutput('pitch_type_time'),
                           title = 'Pitch Selection by Week')
                     )
                     ),
    conditionalPanel(condition = 'input.menu == "reference"',
                     box(title = 'User Reference',
                         p("This project is my submission for the 2017 TruMedia Networks Hackathon."),
                         p('This dashboard is intended to assist a hitter in understand the approach of a pitcher in a variety of situations. The dashboard is built with a modified form of the 2016 data set in which I filtered out columns that I was not using.'),
                         p('The first tab contains the count of pitches that a pitcher throws, the location of those pitchers thrown, some stats on PAs ending in that pitch, and then information on velocty, spin rate and strike probability of pitches thrown, all framed in reference to a certain pitch by a certain pitcher. For example, when you select Wade Davis and his cutter, the bottom right chart tells you that Wade Davis throws his cutter with as high a spin rate as anyone else throws their cutter.'),
                         p('The second tab contains information on pitch usage based on the count of the PA, previous pitch type, and previous pitch location, as well as pitch usage over the course of the season.'),
                         p('Please note that if the data becomes overfiltered down to 0 rows, the graphs will fail to load'),
                         p('Thanks, I hope you enjoy my project.'),
                         p('Daniel Massop')))
    )
  )

server <- function(input, output) {
  
  adjust <- function() {
    
    data = season[season$pitcher == input$Pitcher,]
    
    if (input$Outs != "Any"){                
      data <- data[data$outs == input$Outs,] 
    }
    
    if (input$bathand != "Both"){                
      data <- data[data$batterHand == input$bathand,] 
    }
    
    if (input$RISP != "Both"){                
      data <- data[data$risp == input$RISP,] 
    }
    
    if (input$strikes != "Any"){                
      data <- data[data$strikes == input$strikes,] 
    }
    
    if (input$balls != "Any"){                
      data <- data[data$balls == input$balls,] 
    }
    
    data = data$pitchType
    
    count = count(data)
    
    return(height = count$freq)
  }
  
  adjust4 <- function() {
    
    data = season[season$pitcher == input$Pitcher,]
    
    if (input$Outs != "Any"){                
      data <- data[data$outs == input$Outs,] 
    }
    
    if (input$bathand != "Both"){                
      data <- data[data$batterHand == input$bathand,] 
    }
    
    if (input$RISP != "Both"){                
      data <- data[data$risp == input$RISP,] 
    }
    
    if (input$strikes != "Any"){                
      data <- data[data$strikes == input$strikes,] 
    }
    
    if (input$balls != "Any"){                
      data <- data[data$balls == input$balls,] 
    }
    
    data = data$pitchType
    
    count = count(data)
    
    return(names = count$x)
  }
  
  adjust2 <- function() {
    
    data = season[season$pitcher == input$Pitcher,]

    data = data[data$pitchType == input$pitch_type,]
        
    if (input$Outs != "Any"){                
      data <- data[data$outs == input$Outs,] 
    }
    
    if (input$bathand != "Both"){                
      data <- data[data$batterHand == input$bathand,] 
    }
    
    if (input$RISP != "Both"){                
      data <- data[data$risp == input$RISP,] 
    }
    
    if (input$strikes != "Any"){                
      data <- data[data$strikes == input$strikes,] 
    }
    
    if (input$balls != "Any"){                
      data <- data[data$balls == input$balls,] 
    }
    
    data = data[,c('px','pz','pitchType','batterHand')]
    colnames(data)[3] = 'pitch_type'
    colnames(data)[4] = 'stand'
    
    return(data)
  }
  
  output$selectpitchers <- renderUI({ 
    selectInput("Pitcher", "Pitcher:", c(unique(as.character(season$pitcher))))
  })
  
  output$selectUI <- renderUI({ 
    selectInput("pitch_type", "Pitch Type:", pitchers$Group.2[pitchers$Group.1 == input$Pitcher])
  })
  
  output$distPlot <- renderPlot({
    barplot(adjust(), names.arg = adjust4(), col = 'blue')
  })
  output$heatplot <- renderPlot({
        strikeFX(adjust2(), contour = TRUE)
  })
  adjust3 <- function() {
    
    data <- pitchers[pitchers$Group.1 == input$Pitcher,]
    data <- data[data$Group.2 == input$pitch_type,]
    data <- data[,c(3:6)]
    
    pitchers_range = pitchers[pitchers$Group.2 == input$pitch_type,]
    
    max = c(max(pitchers_range$releaseVelocity),max(pitchers_range$spinRate),max(pitchers_range$probCalledStrike),max(pitchers_range$ProbCalledStrikeDev))
    min = c(min(pitchers_range$releaseVelocity),min(pitchers_range$spinRate),min(pitchers_range$probCalledStrike),min(pitchers_range$ProbCalledStrikeDev))
    
    data = rbind(max,min,data)
    data = data[,c(4,1,3,2)]
  }
  
  output$spiderplot <- renderPlot({
    
    radarchart(adjust3(), axistype=1 , vlabels = c('Strike Called Probability Deviation','Release Velocity','Strike Called Probability','Spin Rate'),
               
               pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
               
               cglcol="grey", cglty=1, axislabcol="White", cglwd=0.8,
               
               vlcex=0.8 
    )
  })
  
  adjust5 <- function() {
    
    data = season[season$pitcher == input$Pitcher,]
    
    if (input$Outs != "Any"){                
      data <- data[data$outs == input$Outs,] 
    }
    
    if (input$bathand != "Both"){                
      data <- data[data$batterHand == input$bathand,] 
    }
    
    if (input$RISP != "Both"){                
      data <- data[data$risp == input$RISP,] 
    }
    
    if (input$strikes != "Any"){                
      data <- data[data$strikes == input$strikes,] 
    }
    
    if (input$balls != "Any"){                
      data <- data[data$balls == input$balls,] 
    }
    
    data <- data[data$pitchType == input$pitch_type,]
    data = data[data$paResult != "",]
    
    batted_balls = data[data$battedBallType != "",]
    
    data = count(data$paResult)
    
    batted_balls = count(batted_balls$battedBallType)
    
    table = NULL
    
    table$BB_Per = (data$freq[data$x == 'BB'] / sum(data$freq))
    table$K_Per = (data$freq[data$x == 'K'] / sum(data$freq))
    table$HR_Per = (data$freq[data$x == 'HR'] / sum(data$freq))
    table$GB_Per = batted_balls$freq[batted_balls$x == 'GB'] / sum(batted_balls$freq)
    table$FB_Per = batted_balls$freq[batted_balls$x == 'FB'] / sum(batted_balls$freq)
    table$LD_Per = batted_balls$freq[batted_balls$x == 'LD'] / sum(batted_balls$freq)
    table$PU_Per = batted_balls$freq[batted_balls$x == 'PU'] / sum(batted_balls$freq)

    for (i in c(1:length(table))){
      if (table[i] == 'numeric(0)') {table[i] = 0.00}
    }
    table = as.data.frame(table)
    colnames(table) = c('BB Per','K Per','HR Per',' GB Per','FB Per', 'LD Per','PU Per')
    return(table)
  }
  
  output$datatable <- renderTable({
    expr = adjust5()
  }, digits = 2)
  
  adjust6 <- function() {
    
    data = season[season$pitcher == input$Pitcher,]
    
    if (input$Outs != "Any"){                
      data <- data[data$outs == input$Outs,] 
    }
    
    if (input$bathand != "Both"){                
      data <- data[data$batterHand == input$bathand,] 
    }
    
    if (input$RISP != "Both"){                
      data <- data[data$risp == input$RISP,] 
    }
    
    if (input$strikes != "Any"){                
      data <- data[data$strikes == input$strikes,] 
    }
    
    if (input$balls != "Any"){                
      data <- data[data$balls == input$balls,] 
    }
    
    means = aggregate(data[,'releaseVelocity'],
                          by = list(data$pitcher, data$pitchType), FUN = mean)
    table = t(means[,c(2:3)])
    table = as.data.frame(table)
    row.names(table) = NULL
    colnames(table) = NULL
    
    return(table)
  }
  
  output$datatable2 <- renderTable({
    expr = adjust6()
  }, digits = 3)
  
  adjust7 <- function() {
    
    data = sequencing[sequencing$pitcher == input$Pitcher,]
    
    if (input$Outs != "Any"){                
      data <- data[data$outs == input$Outs,] 
    }
    
    if (input$bathand != "Both"){                
      data <- data[data$batterHand == input$bathand,] 
    }
    
    if (input$RISP != "Both"){                
      data <- data[data$risp == input$RISP,] 
    }
    
    if (input$strikes != "Any"){                
      data <- data[data$strikes == input$strikes,] 
    }
    
    if (input$balls != "Any"){                
      data <- data[data$balls == input$balls,] 
    }
    
    data <- count(data, c('prev_type','pitchType'))
    frequencies = aggregate(data$freq, by = list(data$prev_type), FUN = sum)
    
    data = merge(data, frequencies, by.x = 'prev_type',by.y = 'Group.1', all.x = TRUE)
    data$frequency = data$freq / data$x
    
    data = data[data$x > 5,]
    
    colnames(data)[2] = 'Next Pitch'
    
    ggplot(data, aes(factor(prev_type), frequency, fill = `Next Pitch`)) + 
      geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set2") + xlab('Previous Pitch') 
  }
  
  output$pitch_type_sequencing <- renderPlot({
    adjust7()
  })
  
  adjust9 <- function() {
    
    data = sequencing[sequencing$pitcher == input$Pitcher,]
    
    if (input$Outs != "Any"){                
      data <- data[data$outs == input$Outs,] 
    }
    
    if (input$bathand != "Both"){                
      data <- data[data$batterHand == input$bathand,] 
    }
    
    if (input$RISP != "Both"){                
      data <- data[data$risp == input$RISP,] 
    }
    
    if (input$strikes != "Any"){                
      data <- data[data$strikes == input$strikes,] 
    }
    
    if (input$balls != "Any"){                
      data <- data[data$balls == input$balls,] 
    }
    
    data <- count(data, c('quadrant_prev','quadrant'))
    frequencies = aggregate(data$freq, by = list(data$quadrant_prev), FUN = sum)
    
    data = merge(data, frequencies, by.x = 'quadrant_prev',by.y = 'Group.1', all.x = TRUE)
    data$frequency = data$freq / data$x
    
    colnames(data)[2] = 'Next Pitch'
    
    ggplot(data, aes(factor(quadrant_prev), frequency, fill = `Next Pitch`)) + 
      geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set3") + xlab('Previous Pitch') 
  }
  
  output$pitch_quadrant_sequencing <- renderPlot({
    adjust9()
  })
  
  adjust10 <- function() {
    
    data = sequencing[sequencing$pitcher == input$Pitcher,]
    
    if (input$Outs != "Any"){                
      data <- data[data$outs == input$Outs,] 
    }
    
    if (input$bathand != "Both"){                
      data <- data[data$batterHand == input$bathand,] 
    }
    
    if (input$RISP != "Both"){                
      data <- data[data$risp == input$RISP,] 
    }
    
    data <- count(data, c('count_prev','pitchResult'))
    frequencies = aggregate(data$freq, by = list(data$count_prev), FUN = sum)
    
    data = merge(data, frequencies, by.x = 'count_prev',by.y = 'Group.1', all.x = TRUE)
    data$frequency = data$freq / data$x
    
    colnames(data)[2] = 'Next Pitch'
    
    ggplot(data, aes(factor(count_prev), frequency, fill = `Next Pitch`)) + 
      geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set1") + xlab('Previous Pitch') 
  }
  
  output$pitch_count_sequencing <- renderPlot({
    adjust10()
  })
  
  adjust11 <- function() {
    
    data = season[season$pitcher == input$Pitcher,]
    
    if (input$Outs != "Any"){                
      data <- data[data$outs == input$Outs,] 
    }
    
    if (input$bathand != "Both"){                
      data <- data[data$batterHand == input$bathand,] 
    }
    
    if (input$RISP != "Both"){                
      data <- data[data$risp == input$RISP,] 
    }
    
    data <- count(data, c('week','pitchType'))
    frequencies = aggregate(data$freq, by = list(data$week), FUN = sum)
    
    data = merge(data, frequencies, by.x = 'week',by.y = 'Group.1', all.x = TRUE)
    data$frequency = data$freq / data$x
    colnames(data)[2] = 'Pitch Type'
    
    ggplot(data=data, aes(x=week, y=frequency, group = `Pitch Type`, colour = `Pitch Type`)) + geom_line(size = 1.1)

  }
  
  output$pitch_type_time <- renderPlot({
    adjust11()
  })
  
}

shinyApp(ui, server)
