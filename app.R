source("helpers2.R")
library(shinydashboard)
library(tidyverse)

ui<-dashboardPage( skin = "purple",
                   dashboardHeader(title = "USA Excess Mortality", titleWidth = 320),
                                   
                   dashboardSidebar(width = 320,
                                   sidebarMenu(
                                     br(),
                                     menuItem("About these analyses", tabName = "about"),
                                     menuItem("Forecast Method", tabName = "fore"),
                                     menuItem("Anomaly Method", tabName = "anom"),
                                     menuItem("R Packages Used", tabName = "references"),
                                      
                                     br(),
                                      HTML(paste(strong("Created by:"), "Timothy Wiemken, PhD", "Samson Niemotka, BS", "Christopher Prener, PhD", sep="<br/>")),
                                      p(a("Email Us", target="_blank", href="mailto:timothy.wiemken@pfizer.com")), 
                                     # br(),
                                     #  img(src="goodslu.png", align="center", height=50, width=150),
                                     # p(),
                                     #  img(src="sipc2.png", align="center", height=150, width=150),
                                     p(),
                                     "Version 1.1, December 16, 2020"
                                    )),
                   
                   dashboardBody(
                     tags$head(tags$style(HTML('
                          .main-header .logo {
                          font-family: "Georgia", Times, "Times New Roman", serif;
                          font-weight: bold;
                          font-size: 20px;
                          }'
                       ))), ### close tags
                     
                     tabItems(
                       tabItem(tabName = "about",
                               tags$h2("About these analyses"),
                               "Two plots are included in this excess mortality analysis. Both plots utilize the same data - ", tags$a(href="https://wonder.cdc.gov/ucd-icd10.html", "historical, confirmed mortality"), " as well as", tags$a(href="https://data.cdc.gov/resource/r8kw-7aab.json?state=United States", "provisional mortality data"), "from the National Center for Health Statistics (NCHS). Provisional mortality is web-scraped on demand and will update with each refresh of this page.",
                               p(),
                               "The forecast method uses data through February 2020 (pre-COVID-19 mortality in the USA), followed by a forecast 36 months from that date. Then, mortality from March 2020 to present is overlayed on the forecast. Seasonal and trend decomposition using Loess and exponential smoothing methods were used for time-series decomposition and forecasting, though other options are made available.",
                               p(),
                               "The anomaly method uses the same data but combines confirmed and provisional data to-date while running an STL/GESD decomposition and anomaly detection algorithm. Twitter's breakout detection is also added to evaluate mean shifts.",
                               p(),
                               tags$h1(""),
                                "Although the CDC NCHS developed an ",
                                  tags$a(href="https://www.cdc.gov/nchs/nvss/vsrr/covid19/excess_deaths.htm",
                                      "excess mortality tool, "),
                               "our team has identified",
                                  tags$a(href="https://pubmed.ncbi.nlm.nih.gov/33079038/",
                                      "other methods which are potentially more useful for detection of abnormal mortality counts."),
                            p(),
                            p(),
                            p(),
                            tags$h3("More Web Applications"),
                            "Our team manages several web applications evaluating COVID-19 Data and Mortality Surveillance",
                                tags$ul(
                                  tags$a(href="https://slu-opengis.github.io/covid_daily_viz/", tags$li("COVID-19 in Missouri")),
                                  tags$a(href="https://surveillance.shinyapps.io/covidmort/", tags$li("COVID-19 Mortality Analyzer")),
                                  tags$a(href="https://surveillance.shinyapps.io/covidsyndromic/", tags$li("COVID-19 Syndromic Surveillance")),
                                  tags$a(href="https://surveillance.shinyapps.io/fluview/", tags$li("Influenza & Pneumonia Mortality Anomaly Detection")),
                                  tags$a(href="https://surveillance.shinyapps.io/ipstats/", tags$li("Infection Prevention Statistics, IPStats")),
                                ), #close bullet list
                            ), # close tab item
                      
                     tabItem(tabName = "fore",
                                radioButtons('forecastmethod', "Choose Forecast Method", choices=c("Exponential Smoothing" = "ets", "ARIMA" = "arima", "Random Walk Drift" = "rwdrift"), selected="ets", inline=T),
                                dygraphOutput("excessplot"),
                                HTML('<center>Drag the sliders left and right to change the view.</center>'),                   
                                #HTML('<center>Mouse-over data points to view raw data.</center>'),                    
                                HTML('<center><img src="legend.jpg", height = 200, width = 500></center>')
                            ), #close tab item
                      tabItem(tabName = "anom",
                              plotlyOutput("anomplot"),
                              
                              column(4, sliderInput('alphaslider', "Width of the normal range",min=0.01, max=0.45, value=0.05, step=0.05, width=200)),
                              column(4, sliderInput('anomslider', "Maximum proportion of anomalies",min=0.01, max=0.45, value=0.05, step=0.05, width=200)),
                              column(4, sliderInput('breakslider', "Minimum points in a breakout",min=3, max=30, value=7, step=1, width=200))
                              
                            ), # close tab item
                      tabItem(tabName = "references",
                             tags$ul(
                               tags$a(href="https://CRAN.R-project.org/package=dplyr", tags$li("Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2020). dplyr: A Grammar of Data Manipulation. R package version 1.0.2.")),
                               tags$a(href="https://CRAN.R-project.org/package=rvest", tags$li("Hadley Wickham (2020). rvest: Easily Harvest (Scrape) Web Pages. R package version 0.3.6.")),
                               tags$a(href="https://www.jstatsoft.org/article/view/v027i03", tags$li("Hyndman RJ, Khandakar Y (2008). “Automatic time series forecasting: the forecast package for R.” _Journal of Statistical Software_, *26*(3), 1-22.")),
                               tags$a(href="https://CRAN.R-project.org/package=dygraphs", tags$li("Dan Vanderkam, JJ Allaire, Jonathan Owen, Daniel Gromer and Benoit Thieurmel (2018). dygraphs: Interface to 'Dygraphs' Interactive Time Series Charting Library. R package version 1.1.1.6.")),
                               tags$a(href="https://CRAN.R-project.org/package=anomalize", tags$li("Matt Dancho and Davis Vaughan (2020). anomalize: Tidy Anomaly Detection. R package version 0.2.1.")),
                               tags$a(href="https://CRAN.R-project.org/package=scales", tags$li("Hadley Wickham and Dana Seidel (2020). scales: Scale Functions for Visualization. R package version 1.1.1.")),
                               tags$a(href="https://ggplot2-book.org", tags$li("H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.")),
                               tags$a(href="https://github.com/twitter/BreakoutDetection", tags$li("Nicholas A. James, Arun Kejariwal and David S. Matteson (2014). BreakoutDetection: Breakout Detection via Robust E-Statistics. R package version 1.0.1.")),
                               tags$a(href="https://CRAN.R-project.org/package=shiny", tags$li("Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2020). shiny: Web Application Framework for R. R package version 1.5.0.")),
                               tags$a(href="https://dev.socrata.com/connectors/rsocrata.html", tags$li("Hugh Devlin, Ph. D., Tom Schenk, Jr., Gene Leynes, Nick Lucius, John Malc, Mark Silverberg and Peter Schmeideskamp (2019). RSocrata: Download or Upload 'Socrata' Data Sets. R package version 1.7.10-6. https://CRAN.R-project.org/package=RSocrata")),
                               ) # close tags
                     ) # close tab item  
                     ) # close tabItems
                ) # close DB body
            ) # close DB page
                   


server <- function(input, output, session) {
  library(jpeg)
  output$excessplot <- renderDygraph({
    dyplot(meth=input$forecastmethod)
    })
  output$anomplot <- renderPlotly({
    anombreak.mort(alp=input$alphaslider, max_anoms=input$anomslider, n.break=input$breakslider)
    })
}


shinyApp(ui = ui, server = server)