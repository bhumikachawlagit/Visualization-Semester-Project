#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/



y <- function(colname,datasetnew,chosenchars){
  df = data.frame()  
  colindex = which(names(datasetnew) == colname)
  for (i in 1:length(chosenchars))
  {
    df = rbind.data.frame(df,
                          datasetnew[datasetnew[,colindex] == chosenchars[i],])
  }
  df 
}

z <- function(colname1,colname2,datasetnew,chosenchars1,chosenchars2){
  df = data.frame()  
  colindex1 = which(names(datasetnew) == colname1)
  colindex2 = which(names(datasetnew) == colname2)
  for (i in 1:length(chosenchars1))
  {
    df = rbind.data.frame(df,
                          datasetnew[datasetnew[,colindex1] == chosenchars1[i],])
  }
  df2 = data.frame()
  for (i in 1:length(chosenchars2)) 
  {
    df2 = rbind.data.frame(df2,
                           df[df[,colindex2] == chosenchars2[i],])
  }
  df2
}


library(shiny)
library(shinythemes)
library(randomcoloR)
library(ggplot2)
library(dplyr)
library(readxl)
source('finalhelpers.R') # This script contains all the data and functions required
# for making the dashboard.

datasetnew$`When was the business launched?` = factor(datasetnew$`When was the business launched?`,
                                                      levels = c("Prior To Covid-19",
                                                                 "During Covid-19" ))

datasetnew$`Monthly Orders` = factor(datasetnew$`Monthly Orders`,
                                     levels = c("< 40" ,"Between 40 - 80","> 80"))


datasetnew$`What is the capital invested?` = factor(datasetnew$`What is the capital invested?`,
                                                    levels = c("less than 2500",
                                                               "2500 - 5000" ,
                                                               "5000 - 7500" ,
                                                               "7500 - 10000",
                                                               "more than 10000"))

# For uni-variate plots: 

x = names(datasetnew) 
barplots = c(x[2],x[4],x[6:9],x[11:28])   
histograms = c(x[1],x[3],x[10])
polarcharts = c(x[5])

#############################################
# For bivariate plots:

x = names(datasetnew)
xc = names(datasetnew)[-c(1,3,10)]
categorical = c(x[2],x[4:9],x[11:28])
continuous = c(x[1],x[3],x[10])
ranks = c(x[6:9],x[11],x[13:20],x[25])
covid = x[4]
capital = x[21]
orders = x[25]

# Define UI for application:

variable_choices = c('Age of owner','Gender of owner',
                     'Months of running the business',
                     'When was the business launched?',
                     'Product Category',
                     'Monthly Orders',
                     'Shipping',
                     "Target Audience",
                     'Mode of Delivery',
                     'Average Net Profit per month (in %)',
                     'Rank for Instagram as aid to business',
                     'Rank for Facebook as aid to business',
                     'Rank for YouTube as aid to business',
                     'Rank for WhatsApp as aid to business',
                     "Content interaction",
                     "Marketing strategy?",
                     'What is the capital invested?',
                     "Does the owner face budget constraints?",
                     'Does the owner face lack of clients?',
                     'Does the owner face lack of inventory?',
                     'Does the owner face shortage of labour?',
                     'Does the owner face family barrier?',
                     "Does the owner face time constraint?",
                     "Does the owner face place constraint?",
                     'Does the owner face lack of management?',
                     'Mode of payment',
                     'Is the business registered on other established websites?',
                     'Does the business have physical store?')

ui <- fluidPage(
  theme = shinytheme('cerulean'),
  
  
  # Application title
  titlePanel(title = p("Analysis of small business owners",
               style ='text-align: center'),
            windowTitle = "Analysis of small business owners"),

  navbarPage("",
             tabPanel(title = 'Introduction',
                      fluidRow(column(width = 12,
                                      
                                      p('Small businesses are corporations, partnerships, or sole proprietorships which have fewer employees and/or less annual revenue than a regular-sized business or corporation.
   Small businesses range from fifteen employees under the Australian',tags$i(tags$a(href = 'https://en.wikipedia.org/wiki/Fair_Work_Australia','Fair Work Act 2009,')),'fifty employees according to the definition used by the',tags$i(tags$a(href = 'https://en.wikipedia.org/wiki/European_Union','European Union,')),'and fewer than five hundred employees to qualify for
   many U.S.', tags$i(tags$a(href='https://en.wikipedia.org/wiki/Small_Business_Administration','Small Business Administration')),'programs.
  Small businesses can also be classified according to other methods, such as annual revenues, shipments, sales, assets, or by annual gross or net revenue or net profits.',style = 'font-size: 19px'),
                                      
                                      p('As for India, the Indian govenment uses the
  following definition for small businesses : In India, all the manufacturing and service enterprises having Investment of',span('not more than', strong('Rs 10 crore')), 'and Annual Turnover of',span('not more than', strong('Rs 50 crore')),' come under the category of small businesses.',
                                        br(),style = 'font-size: 19px'),
                                      
                                      div(img(src='picpng.png',height = 300,width = 400,
                                              style="display: block;margin-left: auto; margin-right: auto;")),
                                      br(),                        
                                      p('This dashboard aims at exploring the chosen dataset using univariate
  and bivariate plots. The univariate plots section can be used to understand
  the overall distribution of various variables in the dataset, while
  the bivariate plots section can be used to examine the interaction effect 
  of two variables for various small businesses.',style = 'font-size: 19px'),
                                      br(),
                                      br(),
                                      
                                      p(strong('Caution:'),'Most small businesses in our
sample are those which have been launched during the period of Covid-19,
hence there is bias in our sample. Thus, any insights drawn from our sample should not be generalised to the entire population of small
business owners in India.',style = 'font-size: 19px'
                                      )))),              
             
             tabPanel(title = 'Dataset Desciption',
                      fluidRow(column(12,
                                      p(
                                        'The data used for this project had been collected with the help of a
Google form link, without collecting any information that would lead to
respondent’s identification. The original collected dataset had 67 rows
and 30 columns, while the one used for visualizations in subsequent tabs has 64 rows and 28 columns
after pre-processing the original data. The following variables were present in 
    the final dataset used: ',style = 'font-size: 19px'
                                      ),
                                      p(tags$ul(tags$li('Age of the business owner : Continuous'),
                                                tags$li("Gender of owner : Categorical" ),
                                                tags$li("Product Category eg. Decor, Clothing, Home Kitchen : Categorical "),
                                                tags$li("When was the business launched? i.e. During Covid-19 or prior to it : Categorical   " ),
                                                tags$li("Number of months since the business has been running : Continuous"),
                                                tags$li("A ranking of Instagram, Facebook, Youtube and WhatsApp as a mode of preference for business by the owners : Categorical "),
                                                tags$li("Target audience : Categorical "),
                                                tags$li("Shipping range eg. Within state, pan india, international : Categorical "),
                                                tags$li("Average Net Profit per month (in %) : Continuous"),
                                                tags$li('Monthly orders : Categorical '),
                                                tags$li("Content interaction : Categorical "),
                                                tags$li("Marketing strategy eg. Inhouse, Outsourced : Categorical "),
                                                tags$li("Whether the business owner faces following constraints :",
                                                        tags$br("All the following variables were Categorical
                                                 i.e. 'Agree', 'Disagree', 'Neutral', 'Strongly Agree' & 'Strongly Disagree' for each one."),
                                                        tags$ul(tags$li('Budget constraints'),
                                                                tags$li("Lack of clients"),
                                                                tags$li("Lack of inventory"),
                                                                tags$li("Shortage of labour"),
                                                                tags$li("Family barrier"),
                                                                tags$li("Time Constraint"),
                                                                tags$li("Place Constraint"),
                                                                tags$li("Lack Of Management"))),
                                                tags$li("Capital Investment : Categorical "),
                                                tags$li("Mode of delivery : Categorical "),
                                                tags$li("Mode of payment : Categorical "),
                                                tags$li("Whether registered on other established websites : Categorical "),
                                                tags$li("Whether the business has a physical store : Categorical "),style = 'font-size: 19px'
                                      )                        
                                      
                                      )))),
             
             tabPanel(title = 'Univariate Analysis',
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = 'univar',
                                      label = 'Choose a variable to display',
                                      choices = variable_choices)),     
                        
                        mainPanel =  mainPanel(
                          plotOutput(outputId = 'uniplot',
                                     width = '900px',
                                     height = '400px')
                        ))),
             
             tabPanel(title = 'Bivariate Analysis',
                      fluidRow(column(width = 6,
                                      selectInput(inputId = 'var1',
                                                  label = 'Choose the 1st variable to display: ',
                                                  choices = variable_choices,
                                                  selected = 'Age of owner',
                                                  width = '500px'),            
                                      conditionalPanel(condition = "input.var1 == 
 'Gender of owner' || input.var1 ==                                         
 'When was the business launched?'|| input.var1 ==
 'Product Category'|| input.var1 ==                                        
 'Rank for Instagram as aid to business'|| input.var1 ==                   
 'Rank for Facebook as aid to business'|| input.var1 ==                   
 'Rank for YouTube as aid to business'|| input.var1 ==                     
 'Rank for WhatsApp as aid to business'||input.var1 ==                     
 'Content interaction'||input.var1 ==                                      
 'Marketing strategy?'||input.var1 ==                                      
 'Does the owner face budget constraints?'||input.var1 ==                  
 'Does the owner face lack of clients?'||input.var1 ==                     
 'Does the owner face lack of inventory?'||input.var1 ==                   
 'Does the owner face shortage of labour?'|| input.var1 ==                 
 'Does the owner face family barrier?'||input.var1 ==                      
 'Does the owner face time constraint?'|| input.var1 ==                    
 'Does the owner face place constraint?'||input.var1 ==                    
 'Does the owner face lack of management?'|| input.var1 ==                 
 'What is the capital invested?'|| input.var1 ==                           
 'Mode of payment'||input.var1 ==                                          
 'Is the business registered on other established websites?'||input.var1 ==
 'Does the business have physical store?'||input.var1 ==                   
 'Monthly Orders'||input.var1 ==                                           
 'Shipping'|| input.var1 ==                                               
 'Target Audience'||  input.var1 ==                                        
 'Mode of Delivery'", 
                                                       radioButtons(inputId = 'charselection1', label = NULL,
                                                                    choices = c('All'= 1,
                                                                                'Manual Select' =  2),
                                                                    selected = 1),
                                                       conditionalPanel(condition = "input.charselection1 == 2",
                                                                        uiOutput(outputId = 'custom1')))),
                               
                               column(width = 6,
                                      selectInput(inputId = 'var2',
                                                  label = 'Choose the 2nd variable to display: ',
                                                  choices = c(variable_choices),
                                                  selected = 'Gender of owner',
                                                  width = '500px'),
                                      
                                      conditionalPanel(condition = "input.var2 == 
 'Gender of owner' || input.var2 ==                                         
 'When was the business launched?'|| input.var2 ==
 'Product Category'|| input.var2 ==                                        
 'Rank for Instagram as aid to business'|| input.var2 ==                   
 'Rank for Facebook as aid to business'|| input.var2 ==                   
 'Rank for YouTube as aid to business'|| input.var2 ==                     
 'Rank for WhatsApp as aid to business'||input.var2 ==                     
 'Content interaction'||input.var2 ==                                      
 'Marketing strategy?'||input.var2 ==                                      
 'Does the owner face budget constraints?'||input.var2 ==                  
 'Does the owner face lack of clients?'||input.var2 ==                     
 'Does the owner face lack of inventory?'||input.var2 ==                   
 'Does the owner face shortage of labour?'|| input.var2 ==                 
 'Does the owner face family barrier?'||input.var2 ==                      
 'Does the owner face time constraint?'|| input.var2 ==                    
 'Does the owner face place constraint?'||input.var2 ==                    
 'Does the owner face lack of management?'|| input.var2 ==                 
 'What is the capital invested?'|| input.var2 ==                           
 'Mode of payment'||input.var2 ==                                          
 'Is the business registered on other established websites?'||input.var2 ==
 'Does the business have physical store?'||input.var2 ==                   
 'Monthly Orders'||input.var2 ==                                           
 'Shipping'|| input.var2 ==                                               
 'Target Audience'||  input.var2 ==                                        
 'Mode of Delivery'", 
                                                       radioButtons(inputId = 'charselection2', label = NULL,
                                                                    choices = c('All'= 3,
                                                                                'Manual Select' = 4),
                                                                    selected = 3),
                                                       conditionalPanel(condition = "input.charselection2 == 4",
                                                                        uiOutput(outputId = 'custom2'))
                                      ))),
                      
                      fluidRow(column(offset = 1 ,width = 8,plotOutput(outputId = 'biplot',
                                                           width = '950px',
                                                           height = '400px')))
             ),
             collapsible = TRUE))


############################################

# Define server logic:
server <- function(input, output,session) {
  
  unidat <- reactive({
    
    switch(input$univar, 
           'Age of owner' = datasetnew$`Age of owner`,
           'Gender of owner' = datasetnew$`Gender of owner`,
           'Months of running the business'= datasetnew$`Months of running the business`,
           'When was the business launched?' = datasetnew$`When was the business launched?`,
           'Product Category' = datasetnew$`Product Category`,
           'Monthly Orders'= datasetnew$`Monthly Orders`,
           'Shipping' = datasetnew$Shipping,
           'Target Audience' = datasetnew$`Target Audience`,
           'Mode of Delivery' = datasetnew$`Mode of Delivery`,
           'Average Net Profit per month (in %)' = datasetnew$`Average Net Profit per month (in %)`,
           'Rank for Instagram as aid to business' = datasetnew$`Rank for Instagram as aid to business`,
           'Rank for Facebook as aid to business' = datasetnew$`Rank for Facebook as aid to business`,
           'Rank for YouTube as aid to business' = datasetnew$`Rank for YouTube as aid to business`,
           'Rank for WhatsApp as aid to business' = datasetnew$`Rank for WhatsApp as aid to business`,
           "Content interaction" = datasetnew$`Content interaction`,
           "Marketing strategy?" = datasetnew$`Marketing strategy?`,
           'What is the capital invested?' = datasetnew$`What is the capital invested?`,
           "Does the owner face budget constraints?" = datasetnew$`Does the owner face budget constraints?`,
           'Does the owner face lack of clients?' = datasetnew$`Does the owner face lack of clients?`,
           'Does the owner face lack of inventory?' = datasetnew$`Does the owner face lack of inventory?`,
           'Does the owner face shortage of labour?' = datasetnew$`Does the owner face shortage of labour?`,
           'Does the owner face family barrier?' = datasetnew$`Does the owner face family barrier?`,
           "Does the owner face time constraint?" = datasetnew$`Does the owner face time constraint?`,
           "Does the owner face place constraint?" = datasetnew$`Does the owner face place constraint?`,
           'Does the owner face lack of management?'= datasetnew$`Does the owner face lack of management?`,
           'Mode of payment' = datasetnew$`Mode of payment`,
           'Is the business registered on other established websites?' = datasetnew$`Is the business registered on other established websites?`,
           'Does the business have physical store?' = datasetnew$`Does the business have physical store?`)
    
  })
  
  output$uniplot <- renderPlot(
    {
      if(input$univar %in% barplots)
      {if(input$univar == 'Shipping')
      {
        verticalbar(datasetnew,unidat(),rearrange = T) + ylab('Proportion')+
          xlab('Category') + ggtitle(input$univar)
      }else if(input$univar == 'When was the business launched?')
      {
        verticalbar(datasetnew,unidat()) + 
          xlim(c('Prior To Covid-19', 'During Covid-19')) + ylab('Proportion')+
          ggtitle(input$univar) + xlab('')
      }else if(input$univar == 'Monthly Orders')
      {
        verticalbar(datasetnew,unidat()) + 
          xlim(c('< 40', 'Between 40 - 80','> 80')) + ylab('Proportion')+
          ggtitle(input$univar) 
      }else if(input$univar == 'What is the capital invested?')
      {
        verticalbar(datasetnew,unidat()) +
          xlim(c('less than 2500','2500 - 5000','5000 - 7500',
                 '7500 - 10000','more than 10000')) + ylab('Proportion')+
          ggtitle(input$univar)
      }else if(input$univar == 'Target Audience'){
        ggplot(datasetnew,aes(unidat())) +
          geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))),
                   fill = sample.int(100,1),color = 'black') +
          theme_bw()+
          theme(panel.grid.major.x = element_blank()) +
          ggtitle(input$univar) + ylab('Proportion') + xlab('') +
          xlim(c("15 to 25 years","15 to 35 years","15 to 45 years",
                 "15 and above","< 25 years","25 to 35 years",
                 "25 to 45 years","25 and above",
                 "35 to 45 years","35 and above","< 35 years",
                 "< 45 years","All age groups"))
      }else if(input$univar == 'Mode of Delivery'){
        ggplot(datasetnew,aes(unidat())) +
          geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))),
                   fill= 	sample.int(100,1),color = 'black') +
          theme_bw()+
          theme(panel.grid.major.x = element_blank()) +
          ggtitle(input$univar) + ylab('Proportion') + xlab('')
      }
        else if(input$univar == 'Marketing strategy?')
        {
          verticalbar(datasetnew,unidat()) + 
            xlim(c('Inhouse Marketing','Outsourced Marketing',
                   'Combination Of Both')) + xlab('') + ylab('Proportion')
        }else
        {
          verticalbar(datasetnew,unidat(),rearrange = F) + xlab('Category')+
            ylab('Proportion') + ggtitle(input$univar)
        }
      }else if(input$univar %in% histograms)
      {
        histogram(datasetnew,unidat()) +
          ylab('Count') + xlab(input$univar) +
          ggtitle(paste('Distribution for',input$univar))
        
      }else if(input$univar %in% polarcharts)
      {
        polarchart(datasetnew,unidat()) + 
          scale_fill_discrete(name = 'Product Category')
      }
    })
  
  observe({
    updateSelectInput(session,inputId = 'var2',
                      choices = variable_choices[-which(variable_choices == input$var1)],
                      selected = input$var2)
  })
  
  observe({
    updateSelectInput(session,inputId = 'var1',
                      choices = variable_choices[-which(variable_choices == input$var2)],
                      selected = input$var1)
  })
  
  observeEvent(
    input$var1,{
      updateRadioButtons(session,inputId = 'charselection1',
                         selected = 1)
    })
  
  observeEvent(
    input$var2,{
      updateRadioButtons(session,inputId = 'charselection2',
                         selected = 3)
    })
  
  bidat1 <- reactive({
    
    switch(input$var1,
           'Age of owner' = datasetnew$`Age of owner`,
           'Gender of owner' = datasetnew$`Gender of owner`,
           'Months of running the business'= datasetnew$`Months of running the business`,
           'When was the business launched?' = factor(datasetnew$`When was the business launched?`,
                                                      levels = c("Prior To Covid-19",
                                                                 "During Covid-19" )),   
           'Product Category' = datasetnew$`Product Category`,
           'Monthly Orders'= factor(datasetnew$`Monthly Orders`,
                                    levels = c("< 40" ,"Between 40 - 80","> 80")),
           'Shipping' = datasetnew$Shipping,
           "Target Audience" = datasetnew$`Target Audience`,
           'Mode of Delivery' = datasetnew$`Mode of Delivery`,
           'Average Net Profit per month (in %)' = datasetnew$`Average Net Profit per month (in %)`,
           'Rank for Instagram as aid to business' = datasetnew$`Rank for Instagram as aid to business`,
           'Rank for Facebook as aid to business' = datasetnew$`Rank for Facebook as aid to business`,
           'Rank for YouTube as aid to business' = datasetnew$`Rank for YouTube as aid to business`,
           'Rank for WhatsApp as aid to business' = datasetnew$`Rank for WhatsApp as aid to business`,
           "Content interaction" = datasetnew$`Content interaction`,
           "Marketing strategy?" = datasetnew$`Marketing strategy?`,
           'What is the capital invested?' = factor(datasetnew$`What is the capital invested?`,
                                                    levels = c("less than 2500",
                                                               "2500 - 5000" ,
                                                               "5000 - 7500" ,
                                                               "7500 - 10000",
                                                               "more than 10000")),
           "Does the owner face budget constraints?" = datasetnew$`Does the owner face budget constraints?`,
           'Does the owner face lack of clients?' = datasetnew$`Does the owner face lack of clients?`,
           'Does the owner face lack of inventory?' = datasetnew$`Does the owner face lack of inventory?`,
           'Does the owner face shortage of labour?' = datasetnew$`Does the owner face shortage of labour?`,
           'Does the owner face family barrier?' = datasetnew$`Does the owner face family barrier?`,
           "Does the owner face time constraint?" = datasetnew$`Does the owner face time constraint?`,
           "Does the owner face place constraint?" = datasetnew$`Does the owner face place constraint?`,
           'Does the owner face lack of management?'= datasetnew$`Does the owner face lack of management?`,
           'Mode of payment' = datasetnew$`Mode of payment`,
           'Is the business registered on other established websites?' = datasetnew$`Is the business registered on other established websites?`,
           'Does the business have physical store?' = datasetnew$`Does the business have physical store?`)
    
  })
  
  bidat2 <- reactive({
    
    switch(input$var2,
           'Age of owner' = datasetnew$`Age of owner`,
           'Gender of owner' = datasetnew$`Gender of owner`,
           'Months of running the business'= datasetnew$`Months of running the business`,
           'When was the business launched?' = factor(datasetnew$`When was the business launched?`,
                                                      levels = c("Prior To Covid-19",
                                                                 "During Covid-19" )),
           'Product Category' = datasetnew$`Product Category`,
           'Monthly Orders'= factor(datasetnew$`Monthly Orders`,
                                    levels = c("< 40" ,"Between 40 - 80","> 80")),
           'Shipping' = datasetnew$Shipping,
           "Target Audience" = datasetnew$`Target Audience`,
           'Mode of Delivery' = datasetnew$`Mode of Delivery`,
           'Average Net Profit per month (in %)' = datasetnew$`Average Net Profit per month (in %)`,
           'Rank for Instagram as aid to business' = datasetnew$`Rank for Instagram as aid to business`,
           'Rank for Facebook as aid to business' = datasetnew$`Rank for Facebook as aid to business`,
           'Rank for YouTube as aid to business' = datasetnew$`Rank for YouTube as aid to business`,
           'Rank for WhatsApp as aid to business' = datasetnew$`Rank for WhatsApp as aid to business`,
           "Content interaction" = datasetnew$`Content interaction`,
           "Marketing strategy?" = datasetnew$`Marketing strategy?`,
           'What is the capital invested?' = factor(datasetnew$`What is the capital invested?`,
                                                    levels = c("less than 2500",
                                                               "2500 - 5000" ,
                                                               "5000 - 7500" ,
                                                               "7500 - 10000",
                                                               "more than 10000")),
           "Does the owner face budget constraints?" = datasetnew$`Does the owner face budget constraints?`,
           'Does the owner face lack of clients?' = datasetnew$`Does the owner face lack of clients?`,
           'Does the owner face lack of inventory?' = datasetnew$`Does the owner face lack of inventory?`,
           'Does the owner face shortage of labour?' = datasetnew$`Does the owner face shortage of labour?`,
           'Does the owner face family barrier?' = datasetnew$`Does the owner face family barrier?`,
           "Does the owner face time constraint?" = datasetnew$`Does the owner face time constraint?`,
           "Does the owner face place constraint?" = datasetnew$`Does the owner face place constraint?`,
           'Does the owner face lack of management?'= datasetnew$`Does the owner face lack of management?`,
           'Mode of payment' = datasetnew$`Mode of payment`,
           'Is the business registered on other established websites?' = datasetnew$`Is the business registered on other established websites?`,
           'Does the business have physical store?' = datasetnew$`Does the business have physical store?`)
    
  })
  
  output$custom1 <- renderUI({
    
    uniques1 = unique(bidat1())
    checkboxGroupInput(inputId = 'chosenchars1',
                       label = 'Choose the characteristics to include: ',
                       choices = c(uniques1),
                       selected = uniques1[1],inline = TRUE)    
  })
  
  output$custom2 <- renderUI({
    
    uniques2 = unique(bidat2())
    checkboxGroupInput(inputId = 'chosenchars2',
                       label = 'Choose the characteristics to include: ',
                       choices = c(uniques2),
                       selected = uniques2[1],inline = TRUE)    
  })
  
  output$biplot <-  renderPlot({
    
    if(input$var1 %in% continuous & input$var2 %in% continuous)
    {
      scatterplot(datasetnew,bidat1(),bidat2()) +
        xlab(input$var1) + ylab(input$var2) +
        ggtitle(paste(input$var1,'vs',input$var2))
    }
    else if(input$var1 %in% continuous && input$var2 %in% categorical)
    {  
      if(input$charselection2 == 3){
        catv = bidat2()
        contv = bidat1()
        temp = datasetnew
      }
      else{
        temp = y(colname = input$var2,datasetnew,
                 chosenchars = input$chosenchars2)
        bidat1c <- reactive({
          ind1 = which(names(temp) == input$var1)
          temp[,unname(ind1)]})
        bidat2c <- reactive({
          ind2 = which(names(temp) == input$var2)
          temp[,unname(ind2)] })
        catv = as.vector(unlist(bidat2c()))
        contv = as.vector(unlist(bidat1c()))
        temp = as.data.frame(temp)}
      
      if(input$var2 %in% ranks){
        
        boxplotfn(temp,catv,contv,rearrangeasc = F,
                  rearrangedsc = F) +
          xlab(input$var2) + ylab(input$var1) +
          ggtitle(paste(input$var1,'vs',input$var2))
      }
      else if(input$var2 == covid){
        boxplotfn(temp,catv,contv,rearrangeasc = F,
                  rearrangedsc = T) +
          xlab(input$var1) + ylab(input$var2) +
          ggtitle(paste(input$var2,'vs',input$var1))
      }
      else{
        boxplotfn(temp,catv,contv) +
          xlab(input$var2) + ylab(input$var1) +
          ggtitle(paste(input$var1,'vs',input$var2))
      }}
    else if(input$var2 %in% continuous & input$var1 %in% categorical)
    { 
      if(input$charselection1 == 1){
        catv = bidat1()
        contv = bidat2()
        temp = datasetnew
      }else{
        temp =  y(colname = input$var1,datasetnew,chosenchars = input$chosenchars1)
        
        bidat1c <- reactive({
          ind1 = which(names(temp) == input$var1)
          temp[,unname(ind1)]})
        bidat2c <- reactive({
          ind2 = which(names(temp) == input$var2)
          temp[,unname(ind2)] })
        catv = as.vector(unlist(bidat1c()))
        contv = as.vector(unlist(bidat2c()))
        temp = as.data.frame(temp) }
      if(input$var1 %in% ranks)
      {
        boxplotfn(temp,catv,contv,rearrangeasc = F,
                  rearrangedsc = F) +
          xlab(input$var1) + ylab(input$var2) +
          ggtitle(paste(input$var2,'vs',input$var1))
      }
      else if(input$var1 == covid){
        boxplotfn(temp,catv,contv,rearrangeasc = F,
                  rearrangedsc = T) +
          xlab(input$var1) + ylab(input$var2) +
          ggtitle(paste(input$var2,'vs',input$var1))
      }
      else{
        boxplotfn(temp,catv,contv) +
          xlab(input$var1) + ylab(input$var2) +
          ggtitle(paste(input$var2,'vs',input$var1))
      }}
    else if(input$var1 %in% categorical & input$var2 %in% categorical)
    {
      if(input$charselection1 == 1 & input$charselection2 == 3)
      {
        catv1 = bidat1()
        catv2 = bidat2()
        temp = datasetnew
      }else if(input$charselection1 == 1){
        temp = y(colname = input$var2,datasetnew,chosenchars = input$chosenchars2)
        bidat1c <- reactive({
          ind1 = which(names(temp) == input$var1)
          temp[,unname(ind1)] }) 
        bidat2c <- reactive({
          ind2 = which(names(temp) == input$var2)
          temp[,unname(ind2)]
        })
        catv1 = as.vector(unlist(bidat1c()))
        catv2 = as.vector(unlist(bidat2c()))
        temp = as.data.frame(temp)
      }else if(input$charselection2 == 3){
        temp = y(colname = input$var1,datasetnew,chosenchars = input$chosenchars1)
        bidat1c <- reactive({
          ind1 = which(names(temp) == input$var1)
          temp[,unname(ind1)] }) 
        bidat2c <- reactive({
          ind2 = which(names(temp) == input$var2)
          temp[,unname(ind2)]
        })
        catv1 = as.vector(unlist(bidat1c()))
        catv2 = as.vector(unlist(bidat2c()))
        temp = as.data.frame(temp)
      }else{      
        temp =  z(colname1 = input$var1,
                  colname2 = input$var2,
                  datasetnew,
                  chosenchars1 = input$chosenchars1,
                  chosenchars2 = input$chosenchars2)
        
        bidat1c <- reactive({
          ind1 = which(names(temp) == input$var1)
          temp[,unname(ind1)] }) 
        bidat2c <- reactive({
          ind2 = which(names(temp) == input$var2)
          temp[,unname(ind2)]
        })
        catv1 = as.vector(unlist(bidat1c()))
        catv2 = as.vector(unlist(bidat2c()))
        temp = as.data.frame(temp)
      }
      if(length(unique(catv1)) <= 8 | length(unique(catv2)) <= 8){
        if(length(unique(catv1)) > 8 & length(unique(catv2)) < 8)
        {
          stackedbarplotvertical(temp,catv1,catv2)  +
            labs(fill=input$var2)+
            xlab(input$var1)+ylab('Proportion')+
            ggtitle(paste(input$var1,'vs',input$var2)) 
          
        }else if(length(unique(catv1)) <= 8 & length(unique(catv2)) > 8)
        {
          stackedbarplotvertical(temp,catv2,catv1) +
            labs(fill=input$var1) +
            xlab(input$var2)+ylab('Proportion')+
            ggtitle(paste(input$var1,'vs',input$var2)) 
          
        }else{
          stackedbarplotvertical(temp,catv1,catv2) +
            labs(fill=input$var2)+
            xlab(input$var1)+ylab('Proportion')+
            ggtitle(paste(input$var1,'vs',input$var2)) 
          
        }
      }
    }
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
