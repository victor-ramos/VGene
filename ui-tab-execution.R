tabPanel("Submit data",
         
         shinyalert::useShinyalert(),
         column(4,
                fluidRow(
                  column(12, wellPanel( 
                    
                    radioButtons('execution_mode','Execution mode',
                                 c('Compare pre-calculated frequencies'="database_mode",
                                   'Upload new file'="new_file_mode"
                                 ), selected = 'database_mode'),
                    
                    conditionalPanel(condition = "input.execution_mode=='new_file_mode'",
                                     fileInput('tsv_file', 'TSV file', multiple = FALSE, accept = c(".tsv") )
                    ),
                    
   
                    checkboxGroupInput("databases", "Compare to ( max 2 options if new file or 3 for pre-calculated freq. ): ",
                                       c("Repertoire of healthy individuals" = "Repertoire_Heavy_and_Light_healthy_database_freq",
                                         "COVID Conv. 1.3m (anti-RBD antibodies)" = "Repertoire_Heavy_and_Light_Conv_1.3m_freq",
                                         "COVID Conv. 6.2m (anti-RBD antibodies)" = "Repertoire_Heavy_and_Light_Conv_6.2m_freq",
                                         "COVID Conv. 1y - Vaccinees Only (anti-RBD antibodies)" = "Repertoire_Heavy_and_Light_1y_Vaccinees_freq",
                                         "COVID Conv. 1y - Non-Vaccinees Only (anti-RBD antibodies)" = "Repertoire_Heavy_and_Light_1y_Non_vaccinees_freq",
                                         "COVID Conv. 1y - Combined (anti-RBD antibodies)" = "Repertoire_Heavy_and_Light_Conv_1y_combined_freq",
                                         "COVID Conv. Combined (anit-NTD antibodies)" = "Repertoire_Heavy_and_Light_NTD_vacc_and_non_vacc_freq",
                                         "COVID Conv. Vaccinees Only (anit-NTD antibodies)" = "Repertoire_Heavy_and_Light_NTD_vacc_freq",
                                         "COVID Conv. Non-Vaccinees Only (anit-NTD antibodies)" = "Repertoire_Heavy_and_Light_NTD_non_vacc_freq",
                                         "Homotypic (mouse)" = "homotypic_vgenes_freq",
                                         "Mosaic (mouse)" = "mosaic_vgenes_freq",
                                         "mRNA (mouse)" = "mRNA_vgenes_freq"
                                       )),
                    
                    conditionalPanel("output.fileUploaded", actionButton("ssh_bt", "Run"))
                    
                    
                  )) #end of the first column
                )), # end of fluid row 
         column(8,
                fluidRow(
                  
                  column(12, uiOutput("data_panel_teste") ),
                  column(12, uiOutput("restart_button") )
                  
                ))
)
