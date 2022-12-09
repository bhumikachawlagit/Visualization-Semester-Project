#Importing, transforming data and defining functions:
library(readxl)
library(dplyr)
library(ggplot2)
dataset = read_excel("Running A Business.xlsx")


#Convert monthly orders into data types with different categories,
# after removing missing values:

missingval = which(dataset$`Monthly orders` == 'Highly variable')
datasetnew = dataset[-missingval,]

datasetnew = datasetnew %>%
  mutate(MonthlyOrders = case_when(
    as.numeric(`Monthly orders`) <= 40 ~ '< 40',
    as.numeric(`Monthly orders`) > 40 & as.numeric(`Monthly orders`) <= 80 ~ 'Between 40 - 80',
    as.numeric(`Monthly orders`) > 80 ~ '> 80'
  ))

#Convert shipping range into proper categorical column:


Shipping = datasetnew$`Shipping?`
datasetnew = datasetnew %>% mutate(Shippingrange = case_when(
  `Shipping?`== "City" ~ 'City',
  `Shipping?` == "Neighbourhood, City" ~ 'City',
  `Shipping` == "City, State" ~ 'Within State',
  `Shipping?` == "Neighbourhood, City, State" ~ 'Within State',
  `Shipping?` == "State" ~ 'Within State',
  `Shipping?` == "International" ~ "International", 
  `Shipping?` == "Neighbourhood, City, State, Pan India, International"  ~ "International",
  `Shipping?` == "Pan India, International" ~ "International",
  `Shipping?` == "Neighbourhood" ~ "Neighbourhood",
  `Shipping?` == "Neighbourhood, City, State, Pan India" ~ 'Pan India',
  `Shipping?` == "Pan India" ~ 'Pan India'
))


#Convert target audience into proper categorical column:

datasetnew = datasetnew %>%
  mutate(TargetAudience = case_when(
    `Target audience` == "15 - 25 years" ~ '15 to 25 years',
    `Target audience` == "less than 15 years, 15 - 25 years" ~ '< 25 years',
    `Target audience` == "15 - 25 years, 25 - 35 years" ~ '15 to 35 years',
    `Target audience` == "15 - 25 years, 25 - 35 years, 35 - 45 years" ~ '15 to 45 years',
    `Target audience` == "25 - 35 years" ~ "25 to 35 years",
    `Target audience` == "25 - 35 years, 35 - 45 years" ~ '25 to 45 years',
    `Target audience` == "25 - 35 years, 35 - 45 years, more than 45 years" ~ '25 and above',
    `Target audience` == "35 - 45 years" ~ '35 to 45 years',
    `Target audience` == "35 - 45 years, more than 45 years"  ~ '35 and above',
    `Target audience` == "less than 15 years, 15 - 25 years, 25 - 35 years"  ~ '< 35 years',
    `Target audience` == "less than 15 years, 15 - 25 years, 25 - 35 years, 35 - 45 years"  ~ '< 45 years',
    `Target audience` == "less than 15 years, 15 - 25 years, 25 - 35 years, 35 - 45 years, more than 45 years" ~ 'All age groups',
    `Target audience` == "15 - 25 years, 25 - 35 years, 35 - 45 years, more than 45 years" ~ '15 and above'
  ))

#Convert Mode of Delivery into a proper categorical column:

datasetnew = datasetnew%>%
  mutate(ModeofDelivery = case_when(
    `Mode of delivery`== 'Couriers' ~ 'Couriers',
    `Mode of delivery`== "Delivery Partner" |
      `Mode of delivery` == "Delivery Partner (BlueDart, Ekart, etc)" ~ "Delivery Partner",
    `Mode of delivery` == "Digital media" ~ "Digital media",
    `Mode of delivery` == "Indiapost"  ~ "Indiapost" ,
    `Mode of delivery` == "NA" ~ "NA",
    `Mode of delivery` == "Self Delivery" ~ "Self Delivery",
    `Mode of delivery` == "Self Pick-Up" ~ "Self Pick-Up",
    `Mode of delivery` == "Staff" ~ "Staff",
    `Mode of delivery` == "Wefast Swiggy Uber" ~ "Wefast Swiggy Uber"
  ))

#Removing inconsistent observation(s):

ind = which(c(datasetnew$`Months of running` > 50 & 
                datasetnew$`Covid, when?` == 'During Covid-19'))

datasetnew = datasetnew[-c(ind),]

# Removing an observation which has 'Mode of Delivery' as 'Not Applicable'
# and it isn't clear why it is so. So removing such rows.

ind = which(datasetnew$ModeofDelivery == 'NA')
datasetnew = datasetnew[-c(ind),]

# Removing the variables which won't be used for any plots.

datasetnew = datasetnew[,-c(3,11,12,14,26,30)]

# Renaming the columns for better ui:

names(datasetnew) = c('Age of owner','Gender of owner',
                      'Months of running the business',
                      'When was the business launched?',
                      'Product Category',
                      'Rank for Instagram as aid to business',
                      'Rank for Facebook as aid to business',
                      'Rank for YouTube as aid to business',
                      'Rank for WhatsApp as aid to business',
                      'Average Net Profit per month (in %)',
                      "Content interaction",
                      "Marketing strategy?",
                      "Does the owner face budget constraints?",
                      'Does the owner face lack of clients?',
                      'Does the owner face lack of inventory?',
                      'Does the owner face shortage of labour?',
                      'Does the owner face family barrier?',
                      "Does the owner face time constraint?",
                      "Does the owner face place constraint?",
                      'Does the owner face lack of management?',
                      'What is the capital invested?',
                      'Mode of payment',
                      'Is the business registered on other established websites?',
                      'Does the business have physical store?',
                      'Monthly Orders',
                      'Shipping',
                      "Target Audience",
                      'Mode of Delivery')


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


verticalbar = function(dataset,column,rearrange=TRUE)
{
  
  library(ggplot2)
  if(rearrange==TRUE){
    column = factor(column,
                    levels = names(sort(table(column),decreasing=T)))}
  
  ggplot(dataset,aes(x = column,fill=column))+
    geom_bar(show.legend = F,
             aes(y = (after_stat(count))/sum(after_stat(count))),color = 'black')+
    theme_bw()+
    theme(panel.grid.major.x = element_blank())+
    scale_fill_brewer(type = 'qual')
  
}

horizontalbar = function(dataset,column)
{
  library(ggplot2)
  column = factor(column,
                  levels = names(sort(table(column))))
  
  ggplot(dataset,aes(column,fill=column))+
    geom_bar(show.legend = F,
             aes(y = (after_stat(count))/sum(after_stat(count))))+
    theme_bw()+
    theme(panel.grid.major.x = element_blank())+
    scale_fill_brewer(type = 'qual',palette = sample.int(8,1),aesthetics = 'fill') +
    coord_flip()
}

polarchart = function(dataset,column)
{
  
  library(ggplot2)
  ggplot(dataset,aes(column,fill=column))+
    geom_bar(width = 1,colour = 'white')+
    geom_text(stat='count',
              aes(y=after_stat(..count..),
                  label=after_stat(scales::percent(..count../sum(..count..),1))),
              position = position_stack(vjust=0.5))+
    coord_polar()+
    theme_gray()+
    theme(axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.line = element_blank())
}

histogram = function(dataset,column)
{
  library(ggplot2)
  ggplot(dataset,aes(column))+
    geom_histogram(bins = 10,fill=sample.int(100,1),color = 'black')+
    theme_bw()+
    theme(panel.grid.major.x = element_blank())
}
scatterplot = function(dataset,col1,col2)
{
  
  library(ggplot2)
  ggplot(dataset,aes(x=col1,y=col2))+
    geom_point(col=sample.int(100,1),size=2) +
    theme_bw()
}

boxplotfn = function(dataset,catv,cont,rearrangeasc = TRUE,
                     rearrangedsc = FALSE)
{
  if(length(unique(catv)) <= 8){
  if(rearrangeasc == T & rearrangedsc == F)
    {
      ggplot(dataset,aes(x=reorder(catv,cont),y=cont,fill=catv))+
        geom_boxplot(show.legend = F)+
        scale_color_brewer(type='qual',palette = sample.int(8,1),aesthetics = 'fill')+
        theme_bw()}
    else if(rearrangedsc==T & rearrangeasc == F){
          ggplot(dataset,aes(x=reorder(catv,-cont),y=cont))+
            geom_boxplot(fill=sample.int(100,1)) + theme_bw()
    }else if(rearrangedsc==F & rearrangeasc == F){
      ggplot(dataset,aes(x=catv,y=cont,fill=catv))+
        geom_boxplot(show.legend = F)+
        scale_color_brewer(type='qual',palette = sample.int(8,1),aesthetics = 'fill')+
        theme_bw()
    }
    }else{
      if(rearrangeasc == T & rearrangedsc == F){
      ggplot(dataset,aes(x=reorder(catv,cont),y=cont))+
        geom_boxplot(fill=sample.int(100,1)) + theme_bw()
    }
      else if(rearrangeasc == F & rearrangedsc == T){
        ggplot(dataset,aes(x=reorder(catv,-cont),y=cont))+
          geom_boxplot(fill=sample.int(100,1)) + theme_bw() 
    
  }else if(rearrangeasc == F & rearrangedsc == F)
    {
      ggplot(dataset,aes(x=catv,y=cont))+
        geom_boxplot(fill=sample.int(100,1)) +
        theme_bw()
    }
  }}



stackedbarplotvertical = function(dataset,catv1,catv2)
{
  
  ggplot(dataset,mapping = aes(catv1,
                        fill=catv2)) +
    geom_bar(position = 'stack',
             aes(y = (after_stat(count))/sum(after_stat(count))),color = 'black') +
    scale_color_brewer(type='qual',palette = 1,aesthetics = 'fill')+
    theme_bw()+
    theme(panel.grid.major.x = element_blank())
}


stackedbarplothorizontal = function(dataset,catv1,catv2)
{
  
  library(ggplot2)
  ggplot(datasetnew,aes(catv1,
                        fill=factor(catv2)))+
    geom_bar(position = 'stack',
             aes(y = (after_stat(count))/sum(after_stat(count)))) +
    coord_flip()+
    scale_color_brewer(type='qual',palette = 1,aesthetics = 'fill')+
    theme_bw()+
    theme(panel.grid.major.x = element_blank())
}



