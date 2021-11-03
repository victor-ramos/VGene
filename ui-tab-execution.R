tabPanel("Submit data",
         
         shinyalert::useShinyalert(),
         column(4,
                fluidRow(
                  column(12, wellPanel( 
                    
                    fileInput('tsv_file', 'TSV file', multiple = FALSE, accept = c(".tsv") ),
                    
                    conditionalPanel("output.databasesPanel", checkboxGroupInput("databases", "Compare to ( Restricted up to 2 options ): ",
                                                                                 c("Repertoire of healthy individuals" = "Repertoire_Heavy_and_Light_healthy_database_freq",
                                                                                   "COVID Conv. 1.3m" = "Repertoire_Heavy_and_Light_Conv_1.3m_freq",
                                                                                   "COVID Conv. 6.2m" = "Repertoire_Heavy_and_Light_Conv_6.2m_freq",
                                                                                   "COVID Conv. 1y - Vaccinees Only" = "Repertoire_Heavy_and_Light_1y_Vaccinees_freq",
                                                                                   "COVID Conv. 1y - Non-Vaccinees Only" = "Repertoire_Heavy_and_Light_1y_Non_vaccinees_freq",
                                                                                   "COVID Conv. 1y - Combined" = "Repertoire_Heavy_and_Light_Conv_1y_combined_freq"
                                                                                   ))),
                    
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
