library(FinCal)
library(ggplot2)
library(shiny)
library(reshape2)
#--------------------------------------------------------------------------------------------------
ui        = fluidPage(
               titlePanel("Anaerobic Digestion and Carbon Markets"),
               sidebarLayout(
                    sidebarPanel(
                         helpText("This tool calculates the potential internal rate of return from 
                                   installing a complete mix anaerobic digester on a swine farm 
                                   based on default investment and state-specific energy prices."),
                         sliderInput("pcarbon",
                                     "Carbon Price (USD per Metric Ton of CO2-Equivalent):",
                                     min=0,max=100,value=0),
                         sliderInput("costshare",
                                     "Cost Share in % of Investment Cost",
                                     min=0,max=25,value=0),
                         radioButtons("mms",h3("Current Manure Management System"),
                                      choices=c("Anaerobic Lagoon",
                                                "Deep Pit",
                                                "Deep Pit (less than 1 month)",
                                                "Liquid/Slurry")),
                         radioButtons("farmtype",h3("Farm Type"),
                                      choices=c("Breeding Swine","Market Swine")),
                         numericInput("head",h3("Swine Inventory"),value=5000),
                         radioButtons("codigestate",h3("Type of Co-Digestate"),
                                      choices=c("Corn Stover","Wheat Straw")),
                         selectInput("state",h3("Select State"),
                                     choices=c("Alabama","Arizona","Arkansas","California",
                                               "Colorado","COnnecticut","Delaware","Florida",
                                               "Georgia","Idaho","Illinois","Indiana","Iowa",
                                               "Kansas","Kentucky","Louisiana","Maine","Maryland",
                                               "Massachusetts","Michigan","Minnesota",
                                               "Mississippi","Missouri","Montana","Nebraska",
                                               "Nevada","New Hampshire","New Jersey","New Mexico",
                                               "New York","North Carolina","North Dakota","Ohio",
                                               "Oklahoma","Oregon","Pennsylvania","Rhode Island",
                                               "South Carolina","South Dakota","Tennessee",
                                               "Texas","Utah","Vermont","Virginia","Washington",
                                               "West Virginia","Wisconsin","Wyoming"),
                                     selected="Iowa")),
                    mainPanel(
                         h4("Greenhouse Gas Emissions and Revenue"),
                         "Based on your farm characteristics, you avoid on an annual basis",
                         textOutput("emissionreduction",inline=TRUE),
                         "Given the carbon price, this would result in an annual revenue of",
                         textOutput("revcarbon",inline=TRUE),
                         h4("Annual Profit"),
                         "The graph below shows the annual profit given energy prices in your",
                         "state over the period 2017-2021. The energy prices are differeniated",
                         "by average, minimum, and maximum values over the time period.",
                         plotOutput("annualprofit"),
                         h4("Internal Rate of Return"),
                         "The graph below shows the internal rate of return (IRR) given average",
                         "energy prices in your state over the period 2017-2021. The upper and",
                         "lower whiskers indicate the IRR in the case of minimum and maximum",
                         "energy prices observed over the time period.",
                         plotOutput("irr"))))
#--------------------------------------------------------------------------------------------------
server    = function(input,output){
               load("DSTData.RData",envir=.GlobalEnv)
               aduse = reactive({funirr(input$head,input$pcarbon,input$state,input$mms,
                                        input$farmtype,input$codigestate,input$costshare)})
               output$emissionreduction = renderText({
                    value               = aduse()
                    value               = paste(round(value[[2]],0),
                                                "Metric Tons of CO2-equivalent.")
               })
               output$revcarbon         = renderText({
                    value               = aduse()
                    value               = paste(round(value[[6]],0),"US Dollars.")
               })
               output$annualprofit      = renderPlot({
                    df                  = aduse()
                    ggplot(df[[4]],aes(x=use,y=profit/1000,fill=scenario))+theme_bw(base_size=15)+
                         ylab("Annual Profit in US Dollars")+
                         geom_bar(stat="identity",color="black",position=position_dodge())+
                         theme(legend.title=element_blank(),axis.title.x=element_blank())+
                         scale_fill_brewer(palette="Paired")
               })
               output$irr               = renderPlot({
                    df                  = aduse()
                    df                  = df[[1]]
                    df                  = dcast(df,use+lifetime~scenario,value.var="irr")
                    df$lifetime         = paste(df$lifetime,"years")
                    ggplot(df,aes(x=use,y=Average*100,fill=lifetime))+theme_bw(base_size=15)+
                         ylab("Internal Rate of Return")+
                         geom_bar(stat="identity",color="black",position=position_dodge())+
                         geom_errorbar(aes(ymin=Minimum*100,ymax=Maximum*100),width=0.5,
                                       position=position_dodge(.9))+
                         theme(legend.title=element_blank(),axis.title.x=element_blank())+
                         scale_fill_brewer(palette="Paired")
               })
          }
#--------------------------------------------------------------------------------------------------
shinyApp(ui=ui,server=server)