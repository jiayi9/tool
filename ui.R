ui <- fluidPage(
#  theme = "style.css",
  tags$script("$(document).on('click', '.dynamicSI button', function () {
                var id = document.getElementById(this.id).getAttribute('data');
                var name = document.getElementById(this.id).getAttribute('name');
                Shiny.onInputChange('lastSelectId',id);
                Shiny.onInputChange('lastSelectName',name);
                // to report changes on the same selectInput
                Shiny.onInputChange('lastSelect', Math.random());
                });"),
  tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                              }
                              "))),
  br(),
  fluidRow(style="font-size:x-small",
    
    column(2,style="background-color: #F2F2F2;",
#      checkboxInput("dataOption","More Data Option",value = FALSE),
#       conditionalPanel("input.dataOption==1",
#       radioButtons("fileSourceType","File:",choices = c("PID List","Local Upload","Enter PID"), selected = "PID List")       
#       ),
      br(),
      HTML('<button data-toggle="collapse" data-target="#dataOptions">More data options</button>'),
      br(),
      tags$div(id = 'dataOptions',  class="collapse", 
               radioButtons("fileSourceType","File:",choices = c("PID List","Local Upload","Enter PID"), selected = "Local Upload") 
               
      ),
      br(),
      #ecube list
      conditionalPanel("input.fileSourceType=='PID List'",
        uiOutput("PID_1")
      ),
      conditionalPanel("input.fileSourceType=='Local Upload'",
        fileInput('file1', '',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv'))
      ),
      conditionalPanel("input.fileSourceType=='Enter PID'",
                       
        fluidRow(column(9,textInput("pid_3",NULL,placeholder = "Enter PID Here")),column(3,actionButton("confirmPID","OK",class="btn btn-primary btn-sm",
                                                                                                        style="font-size:98%;height:25px;width:35px")))
      ),
      hr(),
      
      

      
      HTML('<button data-toggle="collapse" data-target="#filters">Use Filters</button>'),

      tags$div(id = 'filters',  class="collapse", 
               uiOutput("FILTERS"),
               uiOutput("FILTER_GROUP")
      ),
      #checkboxInput("useFilters","Use Filters",FALSE),
#       conditionalPanel("input.useFilters == 1",
#         uiOutput("FILTERS"),
#         uiOutput("FILTER_GROUP")
#       ),
      hr(),
      uiOutput("LAYER"),
      uiOutput("RED"),
      checkboxInput("allNoneNA","Define non-missing as fail", value = FALSE),
      
      
      hr(),
      actionButton("ok","OK !",class = "btn btn-primary btn-xs",style="font-size:98%;height:25px;width:65px"),
      hr(),
      checkboxInput("attr_use_list","Use only variables defined in global.R", value = FALSE),
      p("")
    ),
    column(10,

           div(style="font-size:x-small",textOutput("info")),
           
#           wellPanel(style = "background-color: #ffffff;",
           tabsetPanel(type="pill",
             tabPanel("Charts",style="background-color:#FCFCFC;padding:10px",
#                       fluidRow(
#                         column(2,radioButtons("plotType",NULL,choices = c("ggplot2","plotly"),selected = "ggplot2",inline = TRUE)),
#                         #column(6,textOutput("info")),
#                         column(3,checkboxInput("deletePoints","Select Points to Exclude",value = FALSE))
#                       ),
br(),
                      splitLayout(
                        uiOutput("Y"),
                        uiOutput("X"),
                        uiOutput("SCATTER_X"),
                        uiOutput("SCATTER_Y"),
                        uiOutput("FACET_1"),
                        uiOutput("FACET_2"),
                        radioButtons("plotType",NULL,choices = c("ggplot2","plotly"),selected = "ggplot2",inline = FALSE)
                        
                      ),
                      tabsetPanel(type="tabs",
                        tabPanel(div(style="font-size:small","Boxplot"),
                                 uiOutput("BOXPLOT"),
                                 
                                 
                                 fluidRow(column(2,numericInput("boxplot_width","Width",value = 1000, min=100,max = 2000, step = 10)),
                                          column(2,numericInput("boxplot_height","Height",value = 400,min=100, max = 1000, step = 10))
                                 ),
                                 #verbatimTextOutput("L"),
                                 #checkboxInput("show_delete_boxplot","Delete Specific Points",value = FALSE),
#                                  conditionalPanel(
#                                    "input.deletePoints",
#                                    uiOutput("BOXPLOT_DELETE")
#                                    #plotOutput("boxplot_delete",brush = "brush_1")
#                                  ),
                                 #               plotOutput("boxplot_delete",brush = "brush_1"),
                                 tags$div(  class="deletePoints collapse", 
                                          uiOutput("BOXPLOT_DELETE")
                                          
                                 ),
                                 ""
                                 
                        ),
                        tabPanel("Scatterplot",
                                 uiOutput("SCATTER"),
                                 fluidRow(column(2,numericInput("scatter_width","Width",value = 1000, min=100,max = 2000, step = 10)),
                                          column(2,numericInput("scatter_height","Height",value = 400,min=100, max = 1000, step = 10))
                                 ),
                                 #checkboxInput("show_delete_scatter","Delete Specific Points",value = FALSE),
#                                  conditionalPanel(
#                                    "input.deletePoints",
#                                    #plotOutput("scatter_delete", brush = "brush_1", width = 1000, height=400),
#                                    uiOutput("SCATTER_DELETE")
#                                  ),
                                 tags$div( class="deletePoints collapse", 
                                          uiOutput("SCATTER_DELETE")
                                          
                                 ),
                                 ""      
                                 
                        ),
                        tabPanel("CDF",
                                 uiOutput("CDF"),
                                 fluidRow(column(2,numericInput("cdf_width","Width",value = 1000, min=100,max = 2000, step = 10)),
                                          column(2,numericInput("cdf_height","Height",value = 400,min=100, max = 1000, step = 10))
                                 ),
                                 ""      
                                 
                        )
                      ),


                      tags$div(  class="deletePoints collapse", 
                               fluidRow(
                                 column(1,actionButton("delete","Delete",class = "btn btn-primary btn-xs",style="font-size:98%;height:25px;width:85px")),
                                 column(1,actionButton("reset","Reset",class = "btn btn-primary btn-xs",style="font-size:98%;height:25px;width:85px"))
                               )

                      ),
br(),
HTML('<button data-toggle="collapse" data-target=".deletePoints">Select Points to Exclude</button>')

#                       checkboxInput("deletePoints","Select Points to Exclude",value = FALSE),
# 
#                       conditionalPanel("input.deletePoints",
#                                        fluidRow(
#                                          column(1,actionButton("delete","Delete",class = "btn btn-primary btn-xs",style="font-size:98%;height:25px;width:85px")),
#                                          column(1,actionButton("reset","Reset",class = "btn btn-primary btn-xs",style="font-size:98%;height:25px;width:85px"))
#                                        )
#                       )
                      
               
             ),
             tabPanel("Analysis",style="background-color:#FCFCFC;padding:10px",
                      fluidRow(
                        column(3,
                               uiOutput("BY"),
                               uiOutput("ATTRIBUTE"),
                               numericInput("min_prop","Min Category Proportion",value = 0.05, min=0,step=0.01),
                               
                               numericInput("sig","Min Failures if above unsatisfied",value = 3,min = 0,step = 1),
                               numericInput("table_width","Table width (px)", value = 400, step=5,min=400,max=1000)
#                               uiOutput("TARGET")
#                               uiOutput("FAIL_DEFINE"),
                               
                        ),
                        column(9,
                               #tableOutput("PVALUE_TABLE_2"),
                               #tags$b("P value Summary by Group"),
                               #div(style="font-size:x-small",
                               
                               #dataTableOutput("PVALUE_TABLE")
                               #tableOutput("TABU"),
                               #DT::dataTableOutput("TABU_2",width = 100)
                               uiOutput("TABU_2_UI")
                               #)
                               
                        )
                      ),
                      helpText("Code 401 means the chisq test cannot be performed. N/A is not included in the chisq test."),
                     #tags$b("Contigency Table"),

                     uiOutput("CROSSTAB")

               
             ),
             tabPanel("Download",style="background-color:#FCFCFC;padding:10px",
                      uiOutput("DOWNLOADLIST"),
                      radioButtons("useSelected",NULL,c("All rows in raw data"=1,"Rows selected by filters"=2)),
                      downloadButton("downloadData","Download",class = "btn btn-primary btn-xs")
             )
           
          ),
             

          br(),

#           checkboxInput("showAnalysis","Analysis", value = FALSE),
#           conditionalPanel("input.showAnalysis",
#                            
#                            fluidRow(
#                              column(3,
#                                     uiOutput("BY"),
#                                     uiOutput("ATTRIBUTE"),
#                                     uiOutput("TARGET"),
#                                     uiOutput("FAIL_DEFINE"),
#                                     checkboxInput("allNoneNA","Define non-missing as fail", value = TRUE)
#                                     
#                                     ),
#                              column(9,
#                                     #tableOutput("PVALUE_TABLE_2"),
#                                     tags$b("P value Summary by Group"),
#                                     #div(style="font-size:x-small",
#                                     dataTableOutput("PVALUE_TABLE")
#                                     #)
#                                     ,
#                                     hr(),
#                                     tags$b("Details"),
#                                     uiOutput("CROSSTAB")
#                                     )
#                            )
#                            
# 
#             
#           ),
          

#           checkboxInput("showDownload","Download File", value = FALSE),
#           conditionalPanel("input.showDownload",
#                            uiOutput("DOWNLOADLIST"),
#                            radioButtons("useSelected",NULL,c("All rows in raw data"=1,"Rows selected by filters"=2)),
#                            downloadButton("downloadData","Download",class = "btn btn-primary btn-xs")
#                            ),
          br(),br()
    )
  ),

  absolutePanel(
    top = 90, right = 50, width = 300,draggable = TRUE,fixed = TRUE,

    checkboxInput("hint","    ",value = FALSE),

    conditionalPanel("input.hint==1",
                       div( style="font-size:x-small",
                            textOutput("pvalue"),
                            textOutput("pvalue_2"),
                            tableOutput("info_table")

                       )
                     )
  )

  
)
#fluidPage