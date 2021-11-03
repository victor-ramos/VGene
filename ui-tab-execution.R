tabPanel("Submit data",
         
         shinyalert::useShinyalert(),
         column(4,
                fluidRow(
                  column(12, wellPanel( 
                    
                    fileInput('tsv_file', 'TSV file', multiple = FALSE, accept = c(".tsv") ),
                    
                    conditionalPanel("output.databasesPanel", checkboxGroupInput("databases", "Compare to:",
                                                                                 c("Repertoire of healthy individuals" = "database",
                                                                                   "COVID Conv. 1.3m" = "cov_1.3m",
                                                                                   "COVID Conv. 6m" = "cov_1.3m"))),
                    
                    conditionalPanel("output.fileUploaded", actionButton("ssh_bt", "Run"))
                    # actionButton("ssh_bt", "Run")
                    
                  )) #end of the first column
                )), # end of fluid row 
         column(8,
                fluidRow(
                  
                  column(12, uiOutput("data_panel_teste") ),
                  column(12, uiOutput("restart_button") )
                  
                ))
)