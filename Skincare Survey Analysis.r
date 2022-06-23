library(shiny, warn.conflicts = FALSE)
library(shinydashboard, warn.conflicts = FALSE)
library(broom, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(RColorBrewer, warn.conflicts = FALSE)
library(slickR, warn.conflicts = FALSE)
library(ggthemes, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(tidyr)
library(fastDummies)
library(treemapify)
library(data.table)

data = read.csv("https://raw.githubusercontent.com/JingSu1/WQD7001-GroupProject/main/Skincare_SurveyInMalaysia_cleandata.csv")
vis_cols = c("Gender.","Age.","Race.","Occupation.","Which..if.any..of.the.following.statements.applies.to.you.",
             "Which.of.the.following.types.of.ingredients.would.make.you.more.likely.to.buy.a.skin.care.product.",
             "How.do.did.you.choose.your.products.",
             "On.average..how.much.do.you.spend.on.skincare.products.each.month.",
             "I.experienced.allergies.after.use.a.new.skincare.products...Eg...Rashness..acne..purging.etc.",
             "Do.share.your.skincare.goals.and.motivation.with.us.")
length(vis_cols)

dataID = data.frame(unique(data$Timestamp))
dataID$RecID = seq.int(nrow(dataID)) 
dataID$RecID = paste("RcdID",dataID$RecID,sep="_", collapse=NULL)
colnames(dataID) = c("Timestamp","RecID")
FinalData = left_join(data, dataID, by ='Timestamp')

Count_Gender = FinalData %>% group_by(Gender.)%>%summarise(count = n_distinct(RecID))
Count_Age. = FinalData %>% group_by(Age.)%>%summarise(count = n_distinct(RecID))
Count_Race. = FinalData %>% group_by(Race.)%>%summarise(count = n_distinct(RecID))
Count_Occupation. = FinalData %>% group_by(Occupation.)%>%summarise(count = n_distinct(RecID))
Count_Which..if.any..of.the.following.statements.applies.to.you. = FinalData %>% group_by(Which..if.any..of.the.following.statements.applies.to.you.)%>%summarise(count = n_distinct(RecID))
Count_Which.of.the.following.types.of.ingredients.would.make.you.more.likely.to.buy.a.skin.care.product. = FinalData %>% group_by(Which.of.the.following.types.of.ingredients.would.make.you.more.likely.to.buy.a.skin.care.product.)%>%summarise(count = n_distinct(RecID))
Count_How.do.did.you.choose.your.products. = FinalData %>% group_by(How.do.did.you.choose.your.products.)%>%summarise(count = n_distinct(RecID))
Count_On.average..how.much.do.you.spend.on.skincare.products.each.month. = FinalData %>% group_by(On.average..how.much.do.you.spend.on.skincare.products.each.month.)%>%summarise(count = n_distinct(RecID))
Count_I.experienced.allergies.after.use.a.new.skincare.products...Eg...Rashness..acne..purging.etc. = FinalData %>% group_by(I.experienced.allergies.after.use.a.new.skincare.products...Eg...Rashness..acne..purging.etc.)%>%summarise(count = n_distinct(RecID))
Count_Do.share.your.skincare.goals.and.motivation.with.us. = FinalData %>% group_by(Do.share.your.skincare.goals.and.motivation.with.us.)%>%summarise(count = n_distinct(RecID))

activity_table <- as.data.table(
  cbind(activity_level = c("Malay","Korean","Chinese","Bumiputra sabah",
                           "Indian","African","JAVANESSE","Arab","Arabian","Iban","Libya","Nigerian","sabahan","Sudanese"), 
        activity_multiplier = c(1.2,
                                1.375,
                                1.55,
                                1.725,
                                1.9)
  )
)

ui <- dashboardPage(
  title = "Skincare Survey Analysis",
  dashboardHeader(title = span("Skincare Survey Analysis", style = "font-family: Tahoma, Helvetica, sans-serif; font-weight: bold; font-size: 75%;")),
  dashboardSidebar(sidebarMenu(
    menuItem("Introduction", tabName = "Introduction", icon = icon("heart"),
    badgeLabel = "Overview", badgeColor = "purple"),
    
    menuItem("My Profile", tabName = "dashboard", icon = icon("user"),
    badgeLabel = "Input", badgeColor = "teal"),
    
    menuItem("Object Features", tabName = "Variable", icon = icon("chart-line"),
    badgeLabel = "Analysis", badgeColor = "aqua"),
    
    menuItem("Test Result", tabName = "Match", icon = icon("equals"),
    badgeLabel = "MOdel", badgeColor = "yellow"),
    
    menuItem(text = "Source Code", icon = icon("code"), 
             href = "https://github.com/RongSong1110/WQD7001",newtab = F,
             badgeLabel = "GitHub", badgeColor = "red")
    
  )),
  
  
  dashboardBody(
    tags$head(tags$style(HTML(".main-sidebar { font-size: 16px; }"))),
    tabItems(
      tabItem(tabName = "Introduction",
              fluidRow(
                column(width =3,
                       box(title = tags$p("Welcome to Skincare Platform", style = "font-family: Helvetica; font-weight: bold; font-size: 30px;"),
                           width = 12,
                           height = 460,
                           background = "light-blue",
                           br(),
                           img(src = "logo.webp", height = 110, width = 200),
                           br(),
                           br(),
                           tags$p("This app serves as a free and simple platform to help identify your skin type. Anyone could find solutions to their skin problems and choose suitable skincare products.",
                                  style = "font-size: 120%;"),
                           align = "justify"),
                       box(title = tags$p("User Manual", style = "font-family: Helvetica; font-weight: bold; font-size: 30px;"),
                           width = 12,
                           height = 490,
                           background = "teal",
                           tags$li("Introduction tabs: A brief introduction about the most common skin problems and skincare routine.",
                                   style = "font-size: 115%;"),
                           br(),
                           tags$li("Objective Feature: Exploratory data analysis helps users to better understand the distribution and features that will affect the products choosen.",
                                   style = "font-size: 115%;"),
                           br(),
                           tags$li("Test Result: Applying Random Forest model to predict the cost on the skincare products and thus help choose the suitable products accordingly.",
                                   style = "font-size: 115%;"),
                           align = "justify"),
                ),
                box(title = tags$b("Glowing Skin"), 
                    status = "info",
                    width = 4,
                    height = 460,
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    br(),
                    img(src = "different skin.jpg", height = 380, width = 300),
                    align = "center"),
                box(title = tags$b("Skin Problems"), 
                    status = "info",
                    width = 4,
                    height = 460,
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    br(),
                    img(src = "Skin problems.jpg", height = 380, width = 320),
                    align = "left"),
                box(title = tags$b("Skincare Routine"), 
                    status = "info",
                    width = 8,
                    height = 490,
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    br(),
                    img(src = "Skincare Routine.jpg", height = 400, width = 670),
                    align = "center"),
              )),
      tabItem(tabName = "dashboard",
              box(radioButtons(inputId = "user_gender", label = "My Gender", choices = c("Male", "Female"), selected = "Male", inline = T),
                  numericInput(inputId = "user_age", label = "My Age", min = 0, max = 150, step = 1, value = 0),
                  selectInput(label = "Race", inputId = "user_activity", choices = activity_table[, activity_level], selected = activity_table[1, activity_level]),
                  actionButton(label = "Save & Continue", inputId = "next_button"),
                  actionButton(label = "Reset", inputId = "reset_button"), background = NULL, width = NULL 
              )
      ),
      
      
      tabItem(tabName = "Variable",
              h5("Overview of distribution of skin condition survey results among different population groups in Malaysia."),
              Info<- sidebarLayout(
                sidebarPanel( width = 4, style = "font-size: 90%;", style = "color:blue",
                              radioButtons("q", "Please select your desire exploratory descriptive analysis: ", 
                                           list("Gender" = "a1", "Age" = "a2", "Race" = "a3", "Occupation" = "a4","skin condition" = "a5", "allergy symptom" = "a6",
                                                "ingredients selection" = "a7", "products selection" = "a8","average spend" = "a9",
                                                "skincare goals" = "a10"))
                ),
                box(title = tags$b("Object Features"), 
                    status = "info",
                    width = 10,
                    height = 600,
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    br(),
                    plotOutput("plot"),
                    align = "center"),
              )),
      tabItem(tabName = "Match",
              fluidRow(
                box(title = tags$b("Predictive Model Results using Random Forest Algorithm"),
                    status = "info",
                    width = 12,
                    height = 700,
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    img(src = "result.png", height = 200, width = 670),
                    br(),
                    img(src = "Rmodel.jpg", height =350, width = 670),
                    br(),
                    img(src = "conclusion.png", height = 80, width = 670),
                    align = "center"),
                
      
              
                
              ))
    )
  ),
 )    





server <- function(input, output) {
  output$plot<- renderPlot({
    if(input$q=='a1'){
      Count_Gender %>% 
        ggplot(aes(x = Gender.,y = count)) + geom_bar(stat = 'identity',fill=13)
    }
    else if(input$q=='a2'){
      Count_Age. %>% 
        ggplot( aes(x = Age.,y = count)) + geom_bar(stat = 'identity',fill=2)+coord_flip()
    }
    else if(input$q=='a3'){
      Count_Race. %>%
        ggplot( aes(x = Race.,y = count)) + geom_bar(stat = 'identity',fill=3)+coord_flip()
    }
    else if(input$q=='a4'){
      Count_Occupation. %>%
        ggplot( aes(x = Occupation.,y = count)) + geom_bar(stat = 'identity',fill=4)+coord_flip()
    }
    else if(input$q=='a5'){
      Count_Which..if.any..of.the.following.statements.applies.to.you. %>%
        ggplot(aes(x = Which..if.any..of.the.following.statements.applies.to.you.,y = count)) + geom_bar(stat = 'identity',fill=5)+theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()
    }
    else if(input$q=='a6'){
      Count_I.experienced.allergies.after.use.a.new.skincare.products...Eg...Rashness..acne..purging.etc. %>% 
      ggplot(aes(x = I.experienced.allergies.after.use.a.new.skincare.products...Eg...Rashness..acne..purging.etc.,y = count)) + geom_bar(stat = 'identity',fill=6)+theme(axis.text.x = element_text(angle = 0, hjust = 1))
      
    }
    else if(input$q=='a7'){
      Count_Which.of.the.following.types.of.ingredients.would.make.you.more.likely.to.buy.a.skin.care.product. %>% 
        ggplot( aes(x = Which.of.the.following.types.of.ingredients.would.make.you.more.likely.to.buy.a.skin.care.product.,y = count)) + geom_bar(stat = 'identity',fill=7)+theme(axis.text.x = element_text(angle = 30, hjust = 1))+coord_flip()
    }
    else if(input$q=='a8'){
      Count_How.do.did.you.choose.your.products. %>% 
        ggplot( aes(x = How.do.did.you.choose.your.products.,y = count)) + geom_bar(stat = 'identity',fill=8)+coord_flip()
    }
    else if(input$q=='a9'){
      Count_On.average..how.much.do.you.spend.on.skincare.products.each.month. %>% 
        ggplot( aes(x = On.average..how.much.do.you.spend.on.skincare.products.each.month.,y = count)) + geom_bar(stat = 'identity',fill=12)
    }
    else if(input$q=='a10'){
      Count_Do.share.your.skincare.goals.and.motivation.with.us. %>% 
        ggplot(aes(area = count,label = Do.share.your.skincare.goals.and.motivation.with.us.))+
        geom_treemap(fill="steelblue") +
        geom_treemap_text(fontface = "italic", colour = "red", place = "centre",grow = TRUE,alpha=.6)+
        scale_fill_distiller(palette="Greens")+coord_flip()
    }
  })
}

shinyApp(ui=ui, server=server)

