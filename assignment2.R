library(shiny)
library(ggvis)
library(ggplot2)
library(reshape2)
library(plotly)

setwd('~/Documents/Data Viz/')
frt=read.csv('FRT.csv')
le=read.csv('LE.csv')
region_df <- read.csv('data_LE_country.csv',na.strings="")
pop <- read.csv('pop.csv')
pop_long <- melt(pop, id.vars=c("Country.Name", "Country.Code"),variable.name="Year",
                 value.name="pop")
frt_long <- melt(frt, id.vars=c("Country.Name", "Country.Code"),variable.name="Year",
                 value.name="Fert")
le_long <- melt(le, id.vars=c("Country.Name", "Country.Code"),variable.name="Year",
                value.name="LE")

df <- merge(frt_long,le_long)

df <- merge(df,region_df[,c("Country.Code","Region")])
df <- merge(df,pop_long)
df=na.omit(df)
df$Year <- gsub('X', '', df$Year)
df$Year <- as.numeric(df$Year)
ui = fluidPage(
  
  #  Title
  #titlePanel("Life Expectancy vs Fertility Rate"),
  
  # Sidebar with slider and controls for animation
  sidebarLayout(
    # sidebar with slider
    
    # Show the animated graph
    mainPanel(
      plotlyOutput(outputId="plot_le")
      
    ),
    sidebarPanel(
      # Slider with looping
      sliderInput("theYear", "Year", 1960, 2014, 1960, step = 1, ticks=FALSE,
                  animate=animationOptions(interval=400, loop = F,
                                           playButton = NULL, pauseButton = NULL)),
      # Slider with looping
      sliderInput("thePop", "Population", 10, 30, 20, step = 1,ticks=FALSE)
    )
  )
)

# server section
server = function(input, output) {
  
  # Reactive expression to create data frame and graph
  aniGraph <- reactive ({
    
    # subset the data frame into the portion that has the data for the
    # graph in the animation sequence
    
    # Save subset of 'df' as pe the input year and pass it to the plot
    dat_sub <- df[df$Year==input$theYear,]
    # create the graph
   ggplotly(ggplot(dat_sub, aes(x=LE, y=Fert,label=Country.Name)) +
      geom_point(aes(colour=Region,size=pop,alpha=0.7))+  guides(size = "none") +
      xlim(10,100)+ ylim(0,10) + 
      xlab("Life Expectancy") + ylab("Fertility Rate") +
      scale_size_area(max_size = input$thePop))
   #ggplotly()
  # gg$data[[1]]$text <- dat_sub$Country.Name'
  #$ print (gg$x$data[[1]])
   #print (gg$x$data[[1]])
   #gg$x$data[[2]]$text <-  dat_sub$Country.Name
   #gg$x$data[[3]]$text <-  dat_sub$Country.Name
   #gg$x$data[[4]]$text <-  dat_sub$Country.Name
   #gg$x$data[[5]]$text <-  dat_sub$Country.Name
   #gg$x$data[[6]]$text <-  dat_sub$Country.Name
   #gg$x$data[[7]]$text <-  dat_sub$Country.Name
 #  gg$data[[1]]$hoverinfo <- dat_sub$Country.Name
  # gg$data[[2]]$hoverinfo <- dat_sub$Country.Name
   
  # gg$config <- list(displayModeBar = F, showLink = F)
   
  # ggplotly()
    
  }) 
  

  # Show the graph
  output$plot_le <- renderPlotly({
    aniGraph()
  })
}
shinyApp(ui = ui, server = server)
