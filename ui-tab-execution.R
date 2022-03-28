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
                                         "COVID Conv. Combined 1m (anti-NTD antibodies)" = "Repertoire_Heavy_and_Light_Conv_NTD_1m_final_freq",
                                         "COVID Conv. Combined 1y (anti-NTD antibodies)" = "Repertoire_Heavy_and_Light_Conv_NTD_1y_final_freq",
                                         "COVID Conv. Vaccinees Only 1m (anti-NTD antibodies)" = "Repertoire_Heavy_and_Light_Conv_NTD_Vacc_1m_final_freq",
                                         "COVID Conv. Vaccinees Only 1y (anti-NTD antibodies)" = "Repertoire_Heavy_and_Light_Conv_NTD_Vacc_1y_final_freq",
                                         "COVID Conv. Non-Vaccinees Only 1m (anti-NTD antibodies)" = "Repertoire_Heavy_and_Light_Conv_NTD_Non-Vac_1m_final_freq",
                                         "COVID Conv. Non-Vaccinees Only 1y (anti-NTD antibodies)" = "Repertoire_Heavy_and_Light_Conv_NTD_Non-Vac_1y_final_freq",
                                         "COVID Conv. anti-NTD antibodies - 1m and 1y combined" = "Repertoire_Heavy_and_Light_Conv_NTD_1m_1y_freq",
                                         "COVID Naive Vaccinees (mRNA) 1.3m" = "mRNA_vax_1.3m_freq",
                                         "COVID Naive Vaccinees (mRNA) 5mo" = "mRNA_vax_5mo_freq",
                                         "COVID Naive Vaccinees (mRNA) Booster" = "Booster_Vax_gene_freq",
                                         "COVID AZ. Prime AZ. Boost - 6mo after prime" = "AZ_AZ_vax_6m_freq",
                                         "COVID AZ. Prime BNT. Boost - 1m after prime" = "AZ_BNT_vax_1m_freq",
                                         "COVID AZ. Prime BNT. Boost - 6mo after prime" = "AZ_BNT_vax_6m_freq",
                                         "COVID J&J Vaccinees 1m after prime" = "J&J_RBD_vax_1m_freq",
                                         "COVID J&J Vaccinees 6mo after prime" = "J&J_RBD_vax_6m_freq",
                                         
                                         "COVID Conv. Vaccinees (mRNA) Breakthrough Delta" = "Conv_Vac_Breakthrough_Delta_freq",
                                         "COVID Conv. Vaccinees (mRNA) Breakthrough Omicron" = "Conv_Vac_Breakthrough_Omicron_freq",
                                         
                                         "TBEV - BOS" = "TBEV_BOS_freq",
                                         "TBEV - CZV" = "TBEV_CZV_freq",
                                         "TBEV - CZ" = "TBEV_CZ_freq"
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
