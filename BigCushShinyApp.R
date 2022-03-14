library(shiny)
library(ggplot2)
library(ggpubr)
library(readxl)
library(tidyverse)
library(grid)
library(gridExtra)

SimFootwear= read_excel('/Users/labwork/R-shiny-BigCush/SimulatedFootwearData.xlsx')
SimFootwear$Sex = factor(rep(sample(c('Female','Male'),
                                    100,
                                    replace = TRUE),
                             15))
#### change structure of some columns ####
SimFootwear$Brand = factor(SimFootwear$Brand)
SimFootwear$Shoe = factor(SimFootwear$Shoe)
SimFootwear$Dim = factor(SimFootwear$Dim)

change2numeric = function(x) {
  as.numeric(x)
}

SimFootwear[,c(8:22)] = as.data.frame(apply(SimFootwear[,c(8:22)], 2, change2numeric))

change2factor = function(x) {
  ratings_levels = c('Very Dissatisfied',
                     'Dissatisfied',
                     'Slightly Dissatisfied',
                     'Neutral',
                     'Slightly Satisfied',
                     'Satisfied',
                     'Very Satisfied')
  factor(x, ordered = TRUE, levels = ratings_levels)
}

SimFootwear$HeelCushion = change2factor(SimFootwear$HeelCushion)
SimFootwear$ForefootCushion = change2factor(SimFootwear$ForefootCushion)
SimFootwear$Flexibility = change2factor(SimFootwear$Flexibility)
SimFootwear$Transition = change2factor(SimFootwear$Transition)
SimFootwear$Stability = change2factor(SimFootwear$Stability)
SimFootwear$Overall = change2factor(SimFootwear$Overall)

#### app set-up ####
brands_list = sort(unique(SimFootwear$Brand))

subjIDs = as.numeric(unique(SimFootwear$ID))

subj_characteristics = names(SimFootwear)[2:5]

mech_properties_list = names(SimFootwear)[8:22]

perception_list = names(SimFootwear)[23:28]

stride_metrics_list = names(SimFootwear)[29:34]

angle_metrics_list = names(SimFootwear)[34:43]
plot_theme =   theme(
  text = element_text(family = "Times New Roman", face = "bold"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  plot.title = element_text(
    face = "bold", size = 12, color = "black", hjust = 0.5),
  axis.text.x = element_blank(),
  axis.title.x = element_text(
    face = "bold", size = 12, color = "black"),
  axis.title.y = element_text(
    face = "bold", size = 12, color = "black"),
  axis.text.y = element_text(
    face = "bold", size = 10, color = "black"),
  legend.title = element_text(
    face = "bold", size = 12, color = "black"),
  legend.text = element_text(
    face = "bold", size = 10, color = "black"),
  legend.position = "none")

#design/layout of the app
ui = fluidPage(
  
  titlePanel("Big Cush Data"),
  tabsetPanel(
    tabPanel("Perception", fluid = TRUE,
             sidebarLayout(
               position = 'left',
               sidebarPanel(
                 selectInput(
                   inputId = 'brand_name',
                   label = "Brands",
                   choices = brands_list),
                 selectInput(
                   inputId = 'perception',
                   label = "Perception Variable",
                   choices = c("HeelCushion", "ForefootCushion", "Flexibility",
                               "Transition", "Overall"))),
               mainPanel(
                 plotOutput(outputId = 'satisfaction_scatter'))
             )),
    tabPanel("Mechanical Properties", fluid = TRUE,
             sidebarLayout(
               position = 'left',
               sidebarPanel(
                 selectInput(
                   inputId = 'mech_char',
                   label = "Mechanical Properties",
                   choices = mech_properties_list)),
               mainPanel(
                 plotOutput(outputId = 'mech_violin'))
             )),
    tabPanel("Subjects", fluid = TRUE,
             sidebarLayout(
               position = "left",
               sidebarPanel(
                 selectInput(
                   inputId = 'subj_char', 
                   label = 'Subject Characteristic',
                   choices = subj_characteristics)),
               mainPanel(
                 plotOutput(outputId = 'subj_violin'))
             )),
    tabPanel("Biomechanics", fluid = TRUE,
             sidebarLayout(
               position = "left",
               sidebarPanel(
                 numericInput(
                   inputId = 'subj_id', 
                   label = 'Subject ID',
                   value = 1,
                   max = max(subjIDs),
                   min = min(subjIDs),
                   step = 0.5),
                 selectInput(
                   inputId = 'biomech_var', 
                   label = 'Variable',
                   choices = c('Angle Max', 'Angle Min', 'Angle ROM'))),
               mainPanel(
                 plotOutput(outputId = 'biomech_line'))
             ))
  )
)



#most of R code will go here
#generating plots based on criteria
server = function(input,output,session) {
  
  #need to add shoe name and title
  output$satisfaction_scatter = renderPlot({
    
    # ggplot(SimFootwear[which(SimFootwear$Brand == input$brand_name),],
    #       aes(x = factor(OverallSatisfaction))) + 
    # geom_bar(stat = 'count') + 
    #theme_minimal()
    
    
    ggplot(SimFootwear[which(SimFootwear$Brand == input$brand_name),],
           aes_string(x = 'Shoe', y = input$perception, color = 'Shoe')) +
      geom_count() + 
      scale_y_discrete(limits=c("Very Dissatisfied",
                                "Dissatisfied",
                                "Slightly Dissatisfied",
                                "Neutral",
                                "Slightly Satisfied",
                                "Satisfied",
                                "Very Satisfied"))+
      scale_color_manual(values = c('red', 'blue', 'black',
                                    'red', 'blue')) +
      ggtitle(input$perception) + 
      plot_theme + theme(axis.text.x = element_text(face = "bold", size = 12, color = "black"),
                         axis.title.y = element_blank())
    
  })
  
  #need to add shoe name and legend
  output$biomech_line = renderPlot({
    getVarName = function(input_var, joint) {
      varname = paste0(joint, '_', gsub(' ','_', input_var))
      return(varname)
    }
    
    hipvar = getVarName(input$biomech_var, 'Hip')
    kneevar = getVarName(input$biomech_var, 'Knee')
    anklevar = getVarName(input$biomech_var, 'Ankle_RMF')
    
    hip_plot = ggplot(SimFootwear[which(SimFootwear$ID == input$subj_id),],
                      aes_string(x = 'Shoe', y = hipvar, color = 'Dim' )) +
      geom_line(aes(group = Dim))+
      geom_point() +
      scale_color_manual(values = c("black", "red", "blue"))+
      labs(y = paste0('Hip ', input$biomech_var)) +
      plot_theme + theme(axis.text.x = element_text(face = "bold", size = 12, color = "black"))
    
    knee_plot = ggplot(SimFootwear[which(SimFootwear$ID == input$subj_id),],
                       aes_string(x = 'Shoe', y = kneevar, color = 'Dim' )) +
      geom_line(aes(group = Dim))+
      geom_point() +
      scale_color_manual(values = c("black", "red", "blue"))+
      labs(y = paste0('Knee ', input$biomech_var)) +
      plot_theme + theme(axis.text.x = element_text(face = "bold", size = 12, color = "black"))
    
    ankle_plot = ggplot(SimFootwear[which(SimFootwear$ID == input$subj_id),],
                        aes_string(x = 'Shoe', y = anklevar, color = 'Dim' )) +
      geom_line(aes(group = Dim))+
      geom_point() +
      scale_color_manual(values = c("black", "red", "blue"))+
      labs(y = paste0('Ankle ', input$biomech_var)) +
      plot_theme + theme(axis.text.x = element_text(face = "bold", size = 12, color = "black"))
    
    ggarrange(hip_plot, knee_plot, ankle_plot, ncol=1, nrow=3, common.legend = TRUE, legend="bottom")
    
  })
  
  
  output$mech_violin = renderPlot({
    
    ggplot(SimFootwear[!duplicated(SimFootwear$Brand),],
           aes_string(x = "factor(1)",
                      y = input$mech_char)) +
      geom_violin(fill = 'blue', trim = FALSE) +
      labs(title = input$mech_char) +
      plot_theme + theme(axis.title.x = element_blank(),
                         axis.title.y = element_blank())
  })
  
  output$subj_violin = renderPlot({
    ggplot(SimFootwear[!duplicated(factor(SimFootwear$ID)),],
           aes_string(x="Sex",
                      y = input$subj_char,
                      fill = "Sex")) +
      geom_violin(trim = FALSE) +
      scale_fill_manual(values = c('red', 'blue'))+
      labs(title = input$subj_char) +
      plot_theme + theme(axis.title.y = element_blank(),
                         axis.text.x = element_text(
                           face = "bold", size = 12, color = "black")
      )
  })
  
}

#runs the app - needed to generate the app
shinyApp(ui, server)


#EXTRA CODE - NOT RUN
#createAngleData = function(data, varNames) {
# dimX = cbind(
#   data[,which(names(data) == 'ID')],
#   data[,which(names(data) == 'shoes')],
#   rep('X', nrow(data)),
#   data[,which(names(data) == varNames[1])],
#   data[,which(names(data) == varNames[4])],
#   data[,which(names(data) == varNames[7])])
# dimY = cbind(
#   data[,which(names(data) == 'ID')],
#   data[,which(names(data) == 'shoes')],
#   rep('Y', nrow(data)),
#   data[,which(names(data) == varNames[2])],
#   data[,which(names(data) == varNames[5])],
#   data[,which(names(data) == varNames[8])])
# dimZ = cbind(
#   data[,which(names(data) == 'ID')],
#   data[,which(names(data) == 'shoes')],
#   rep('Z', nrow(data)),
#   data[,which(names(data) == varNames[3])],
#   data[,which(names(data) == varNames[6])],
#   data[,which(names(data) == varNames[9])])
# names(dimX) = c('ID', 'Shoes','Dim', 'Hip', 'Knee', 'Ankle')
# names(dimY) = c('ID', 'Shoes','Dim', 'Hip', 'Knee', 'Ankle')
# names(dimZ) = c('ID', 'Shoes','Dim', 'Hip', 'Knee', 'Ankle')
# 
# tempData = rbind(dimX, dimY, dimZ)
# tempData$ID = factor(tempData$ID)
# tempData$Shoes = factor(tempData$Shoes)
# tempData$Dim = factor(tempData$Dim)
# tempData$Hip = as.numeric(tempData$Hip)
# tempData$Knee = as.numeric(tempData$Knee)
# tempData$Ankle = as.numeric(tempData$Ankle)
# return(tempData)
# }
# 
# tempAngleData = createAngleData(SimFootwear[which(SimFootwear$ID == input$subj_id),],
#                                 varList)
