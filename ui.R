####################################################
#      Segmentation Discriminant and Targeting     #
####################################################

library("shiny")
#library("foreign")

shinyUI(pageWithSidebar(
  # Header:
  headerPanel("Segmentation"),
  # Input in sidepanel:
  sidebarPanel(

    fileInput("file", "Upload Segmentation data (csv file with header)"),
    fileInput("file1", "Upload Discriminant data (csv file with header)"),
    fileInput("file2", "Upload Classification data (csv file with header)"),
        
    selectInput("select", "Choose Segmentation Algo", 
                       c("K-Means","Hierarchical","Model Based"), selected = "K-Means"),
    
    numericInput("Clust", "Number of Segments:", 3),
    
    br(),

submitButton(text = "Apply Changes", icon("refresh"))
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #
                
                tabPanel("Overview",h5(p("Segmentation")), 
                         p("Market segmentation is a marketing strategy which involves dividing a broad target market into subsets of consumers, businesses, or countries who have, or are perceived to have, common needs, interests, and priorities, and then designing and implementing strategies to target them. Market segmentation strategies are generally used to identify and further define the target customers, and provide supporting data for marketing plan elements such as positioning to achieve certain marketing plan objectives. Businesses may develop product differentiation strategies, or an undifferentiated approach, involving specific products or product lines depending on the specific demand and attributes of the target segment.",align="justify"),
                         a(href="https://en.wikipedia.org/wiki/Market_segmentation","- Wikipedia"),
                         h5(p("How to use this shiny application")),
                         p("This shiny application requires one data input from the user. To do so, click on the Browse (in left side-bar panel) and upload the csv data input file.
                           Note that this application can read only csv file(comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                           and then proceed. Make sure you have top row as variable names and first column as respondent id/name in csv file",align="justify"),
                         p("Once csv file is uploaded successfully, by-default application will perform K-means segmentation with 3 segments. In left-side bar panel you can change the segmentation algorithm and number of segments. Click on Apply changes after making any change in the inputs. Accordingly results will be updates in all the tabs",align="justify"),
                         br()
                         ),
                
                #tabPanel("Data",h3(textOutput("caption"),tableOutput("table"))),
                
                tabPanel("Summary - Segmentation",h3(textOutput("caption1")), h4(div(textOutput("caption2"),style = "color:Red")),
                         plotOutput("plotpca",height = 400, width = 500),verbatimTextOutput("summary")),
                
                tabPanel("Summary - Discriminant", verbatimTextOutput("discriminat")),
                tabPanel("Summary - Targeting", verbatimTextOutput("targeting")),
                
                # discriminat
                
                tabPanel("Plot",h3("Segments Plot"), plotOutput("plot",height = 700, width = 840)),
                tabPanel("Data Segment",tableOutput("table")), 
                tabPanel("Data Target",tableOutput("table1")) 
                )
      ) 
    ) 
  )
# tabPanel("PCA Variance Plot",plotOutput("plot1", width = "100%")),
# tabPanel("JSM Plot",plotOutput("plot", height = 800, width = 840)),