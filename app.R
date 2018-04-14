library(shiny)
library(dplyr)
library(plotly)

ui <- fluidPage(withMathJax(),
  tabsetPanel(
    tabPanel("Calculator",
             sidebarLayout(
               sidebarPanel( selectInput(inputId = "p",
                                         label = "Type:",
                                         c("SSR Single Rate-Up" = 0.007,
                                           "SSR Double Rate-Up" = 0.008,
                                           "Any SSR" = 0.01)),
                 img(src='Saintquartz.png', width = 25, height = 25, align = "left"),
                            numericInput(inputId = "Quartz",
                                         label = "Number of Quartz:",
                                         value = 3, min = 0, max = 5000, step = 1), 
                            img(src='Summon_ticket.png', width = 25, height = 25, align = "left"),
                            numericInput(inputId = "Tickets",
                                         label = "Number of Tickets:",
                                         value = 1, min = 0, max = 5000, step = 1),
                            #div(style="display: inline-float;vertical-align:right; width: 500px;",img(src='Jeanneaf.png', width = 150, height = 180, align = "right")),
                            textOutput("N"),
                            textOutput("Atleast1"),
                            tableOutput("binomial_frame_final")
               ),
               mainPanel(plotlyOutput("plot1"),
                         br(),
                         p("Double click the plot or press the home button on the top right to reset the plot."))
             )
    ),
    tabPanel("About",mainPanel(
      h1("Theory"),
      p("The probabilities were calculated using a binomial distribution as there are only two possible outcomes which are denoted,
        success (if we get a SSR) and failure (if we don't get a SSR). Note that for the case of only one roll, it simply becomes a bernoulli distribution!
        We let $N$ be the total number of rolls we can do while $p$ is the rate up depending on the type of situation."),
      "$$ X \\sim Bin(n,p)  $$",
      p("Thus the probability of getting exactly $x$ SSR(s) is given below:"),
      "$$ P(X = x) = \\binom{n}{x} p^x(1-p)^{n-x} $$" ,
      p("Substituting the numbers from $0$ to $N$ into $n$ will provide the probability density function which can then be used to estimate values seen in the table on the Calculator tab.
        A property of the binomial distribution is the trials (or rolls in this case) are", strong("independent"),"i.e. the outcome of one roll will not affect the outcome of the next roll!"),
      br(),
      p("Author: WTByte"),
      p("Built using",a(href="http://shiny.rstudio.com/", "Shiny"),"by",a(href="https://www.rstudio.com/", "RStudio"),"and",a(href="https://www.r-project.org/", "R"),"."),
      p("Last updated: 14/04/2018")
    )
  )
  )
)

#This is where the functionality of the webpage is from
server <- function(input,output) {

  p = reactive ({ as.numeric(input$p)})
  
  ptext = reactive ({
    if (p() == 0.007) {
      ptext = "Single Rate-Up SSR"
    } else if (p() == 0.008) {
      ptext = "Double Rate-Up SSR"
    } else {
      ptext = "SSR (Any)"
    }
  })
  
  N = reactive ({ 
    
    validate( 
      need(!is.na(input$Tickets) && !is.na(input$Quartz),"Error: NA, please insert number(s)!")
    )
    
    as.integer(floor(input$Quartz/3) + input$Tickets) })
  
  #Calculate how many attempts we get
  output$N = reactive ({paste("Total number of rolls:", as.integer(floor(input$Quartz/3) + input$Tickets) )})
  output$n = reactive ({ as.integer(floor(input$Quartz/3) + input$Tickets) })
  
  #Render the actual table
  binomial_frame = reactive ({
    binomial_frame = data.frame(binomial_data(),Probability = apply(binomial_data(),1,function(x) dbinom(x,N(), p() )),
                                Percent = apply(binomial_data(),1,function(x) dbinom(x,N(),p() ))*100)
  })
  
  #Calculate the relevant probabilities using binomial
  binomial_data = reactive ({
    if (N() == 1) {
      data.frame("Total_5_Stars" = 0:1)
    } else {
      data.frame("Total_5_Stars" = 0:N())
    }
     })
  
  output$binomial_frame = renderTable ({
    if (N() == 1) {
      binomial_frame = data.frame(binomial_data, Probability = c(1-p() ,p() ), Percent = c(1-p() ,p() )*100)  
    } else {
      binomial_frame = data.frame(binomial_data(),Probability = apply(binomial_data(),1,function(x) dbinom(x,N(),p() )),
                                  Percent = apply(binomial_data(),1,function(x) dbinom(x,N(),p() ))*100)
    }
  })
  
  
  #Minimum and Maximum cut off for plotting and table.
  cut_off_max = reactive ({ 
    if ((N() > 1) & (N() <= 683 )) {
      min(which(binomial_frame()$Percent < 0.05))
    } else if (N() == 1){ 
      2
    } else {
      max(which(binomial_frame()$Percent > 0.05))  
      }
    })
  
  cut_off_min = reactive ({ 
    if ((N() > 1) & (N() <= 683 )) {
      min(which(binomial_frame()$Percent > 0.05)) - 1.5
    } else if (N() == 1){ 
      min(which(binomial_frame()$Percent > 0.05)) - 1.5
    } else {
      min(which(binomial_frame()$Percent > 0.05)) - 0.5
    }
  })
  
  #This is the actual table that is shown on the web app!
  output$binomial_frame_final = renderTable ({
    if (N() == 1) {
      binomial_frame()[1:2,]
    } else {
      binomial_frame()[cut_off_min()+1.5:cut_off_max(),]  
    }
    })
  
  #Output plot we see on web app
  output$plot1 = renderPlotly({
    plot_ly(
      binomial_frame(),
      x = ~Total_5_Stars,
      y = ~Percent,
      type = "bar"
    ) %>% 
      layout(title = paste("Estimated Percentage of getting X", ptext(), "in", N(),"roll(s)"),
             yaxis = list(autorange = TRUE, title = "Percent Chance (%)"),
             xaxis = list(range = c(cut_off_min(),cut_off_max()+0.5), title = paste("Total Number of", ptext(),", P(X = x)")),
             margin = list(t = 50))
  })
  
  #Calculate at least one SSR depending on type.
  output$Atleast1 =  reactive ({ paste("Chance of at least 1 ",ptext(),": ", signif(100 - binomial_frame()$Percent[1],5),"%", sep = "" ) })
}

shinyApp(ui = ui, server = server)