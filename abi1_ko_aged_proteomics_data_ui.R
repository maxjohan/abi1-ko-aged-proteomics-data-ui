#
# This is an UNPUBLISHED , INCOMPLETE Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load in packages
library(shiny)
library(plotly)
library(scales)
library(ggplot2)
library(ReactomePA)
library(DT)
library(enrichplot)
library(org.Mm.eg.db)
library(org.Hs.eg.db)


##################### Start UI #####################

ui <- fluidPage(tabsetPanel(
    
    #Tab panel showing file upload and metadata
    
    # tabPanel("Upload data",
    #          
    #          # Page title
    #          titlePanel("File upload + metadata"),
    #          
    #          # sidebar for file upload and metadata (file upload selector)
    #          sidebarLayout(position = 'right',
    #                        sidebarPanel(
    #                            
    #                            # File upload button
    #                            fileInput("dataset", "Choose CSV File",
    #                                      accept = c(
    #                                          "text/csv",
    #                                          "text/comma-separated-values,text/plain",
    #                                          ".csv")),
    #                            checkboxInput("header", "Header", TRUE)),
    #                        
    #                        # Main panel for file upload and metadata (metadata plots and top 6 rows datatable)
    #                        mainPanel(
    #                            
    #                            "This is a user interface to explore and interpret mass spectrometry data generated from TurboID-ABI1 proximity labeling experiments reported in Petersen et al (2021)", br(), br(),
    #                            "1) Upload file from supplemental info: 'Supp_File_5_cleaned_data.csv'", br(),
    #                            "2) Adjust statistical filtering on tab: 'Volcano Plots'", br(),
    #                            "3) Initiate new analyses by clicking tabs and waiting for load time", br(),
    #                            "e.g. after adjusting filtering on 'Volcano Plots', click 'Pathway analysis' tab and wait ~10 seconds",
    #                            
    #                            # These are plots for pvalue distribution of dataset (histogram of TurboAbi vs TURBO CTRL p-values)
    #                            # and normal distribution of peak area ratios (histogram of TurboAbi vs TURBO CTRL PA ratios)
    #                            fluidRow(
    #                                column(width = 6, plotOutput("pvalue_distribution")),
    #                                column(width = 6, plotOutput("histogram"))
    #                            ),
    #                            
    #                            # Show first six rows in a table below histograms
    #                            tableOutput("datatable_head")
    #                        )
    #          )
    # ),
    
    # Tab panel for volcano plots
    tabPanel("Volcano plots",
             
             # Page title
             titlePanel("Volcano plots of filtered data"),
             
             # Sidebar with sliders to modify TurboAbi:Control PA pValue/ratio and PSM pValue/ratio 
             sidebarLayout(
                 sidebarPanel(width = 4,
                              verticalLayout(
                                  # 
                                  fluidRow(
                                      radioButtons("set", 
                                                  "Proteomics dataset:",
                                                  c("Plasma",
                                                    "Cellular"),
                                                  selected="Cellular")),
                                  
                                  fluidRow(
                                      selectInput("comp", 
                                                  "Comparison",
                                                  c("Old KO vs. old WT",
                                                    "Young KO vs. young WT",
                                                    "Young KO vs. old KO",
                                                    "Young WT vs. old WT"),
                                                  selected="Old KO vs. old WT")),
                                  
                                  #   selectInput("threshold","Pre-loaded thresholds:",c(none,
                                  #                                                      pa,
                                  #                                                      psm,
                                  #                                                      papsm))
                                  # ),
                                  # 
                                  # fluidRow(
                                  #     radioButtons("tails","Statistics by:",
                                  #                  choices = c("One-tailed tests"="one","Two-tailed tests"="two"),
                                  #                  selected="one")),
                                  
                                  # fluidRow(
                                  #     radioButtons("mod","Show proteins that meet PA, PSM thresholds:",
                                  #                  choices = c("Either"="either","Both"="both"),
                                  #                  selected="both")),
                                  fluidRow(
                                      # Panel with statistical threshold inputs on 'volcano plots' tab
                                      column(width = 6,
                                             sliderInput("logfc",
                                                         "LogFC",
                                                         min=-9,
                                                         max=9,
                                                         value=c(-1,1),
                                                         step=0.1))),
                                  
                                  fluidRow(
                                      column(width = 6,
                                             # numericInput("exp_control_pa_pval",
                                             #     "Peak area p-value TurboAbi vs. TurboControl",
                                             #     min = 0,
                                             #     max = 1,
                                             #     value = 1,
                                             #     step = 0.01),
                                             # 
                                             numericInput("pval",
                                                          "Adjusted p-value",
                                                          min=0,
                                                          max=1,
                                                          value=1 ,
                                                          step = 0.01)),
                                      
                                  fluidRow(
                                      # column(width = 3,
                                      #        actionButton('thresh1', 'Threshold 1')),
                                      # column(width = 3,
                                      #        actionButton('thresh2', 'Threshold 2')),
                                      # #column(width = 3,
                                      # #actionButton('both_filter', 'PA/PSM filter')),
                                      column(width = 3,
                                             actionButton('reset_input', 'Reset filtering')))))),
                 
                 # Show a plot of the generated distributions
                 # SEE FILTERS!! (CTRL+F: filtered_data_reactive / filtered_data_missing_control_reactive)
                 # Bottom plot displays proteins identified in EXP, but not in CONTROL 1 AND / OR CONTROL 2.
                 # Bottom plot is NOT changed by RATIO (it is changed by FDR, PSM count...)
                 # Top plot is everything else (i.e. at least one hit in both controls)
                 mainPanel(
                     plotlyOutput("volcano_plot"), # filtered_data_reactive
                 ))),
    
    # Tab panel for reactome pathway analysis
    tabPanel("Pathway analysis",
             titlePanel("Reactome pathway analysis of filtered data"),
             mainPanel(
                 plotOutput("reactome_bar_plot"),
                 br(),
                 br(),
                 br(),
                 br(),
                 DT::dataTableOutput("reactome_table")
             )),
    
    # Tab panel for stringDB analysis
    tabPanel("stringDB",
             mainPanel(
                 plotOutput("string")
             )),
    
    # This DAVID tab panel is commented out because it started throwing an error as of 9/15/2021 (see server section for error)
    
    # Tab panel for DAVID analysis
    # tabPanel("DAVID",
    #          titlePanel("DAVID analysis"),
    #          # sidebarLayout(
    #          #    sidebarPanel(
    #          #      checkboxGroupInput("david_cats", "Select DAVID annotation categories:",
    #          #                        c("ENTREZ_GENE_ID" = "ENTREZ_GENE_ID",
    #          #                          "BBID" = "BBID",
    #          #                          "BIOCARTA" = "BIOCARTA",
    #          #                          "BIOGRID_INTERACTION" = "BIOGRID_INTERACTION",      
    #          #                          "CGAP_EST_QUARTILE" = "CGAP_EST_QUARTILE",
    #          #                          "CGAP_SAGE_QUARTILE" = "CGAP_SAGE_QUARTILE",
    #          #                          "CHROMOSOME" = "CHROMOSOME",
    #          #                          "COG_ONTOLOGY" = "COG_ONTOLOGY",
    #          #                          "CYTOBAND" = "CYTOBAND",
    #          #                          "DIP" ="DIP",
    #          #                          "ENSEMBL_GENE_ID" = "ENSEMBL_GENE_ID",
    #          #                          "EC_NUMBER" = "EC_NUMBER",
    #          #                          "GAD_DISEASE" = "GAD_DISEASE",
    #          #                          "ENTREZ_GENE_SUMMARY" = "ENTREZ_GENE_SUMMARY",
    #          #                          "GENE3D" = "GENE3D",
    #          #                          "GAD_DISEASE_CLASS" = "GAD_DISEASE_CLASS",
    #          #                          "GNF_U133A_QUARTILE" = "GNF_U133A_QUARTILE",
    #          #                          "GENERIF_SUMMARY" = "GENERIF_SUMMARY",
    #          #                          "GOTERM_BP_2" = "GOTERM_BP_2",
    #          #                          "GOTERM_BP_1" = "GOTERM_BP_1",
    #          #                          "GOTERM_BP_4" ="GOTERM_BP_4",
    #          #                          "GOTERM_BP_3" ="GOTERM_BP_3",
    #          #                          "GOTERM_BP_ALL" = "GOTERM_BP_ALL",
    #          #                          "GOTERM_BP_5" = "GOTERM_BP_5",
    #          #                          "GOTERM_BP_FAT" = "GOTERM_BP_FAT",
    #          #                          "GOTERM_BP_DIRECT" = "GOTERM_BP_DIRECT",
    #          #                          "GOTERM_CC_3" = "GOTERM_CC_3",
    #          #                          "GOTERM_CC_4" = "GOTERM_CC_4",
    #          #                          "GOTERM_CC_1" = "GOTERM_CC_1",
    #          #                          "GOTERM_CC_2" = "GOTERM_CC_2",
    #          #                          "GOTERM_CC_DIRECT" = "GOTERM_CC_DIRECT",
    #          #                          "GOTERM_CC_FAT" = "GOTERM_CC_FAT",
    #          #                          "GOTERM_CC_5" = "GOTERM_CC_5",
    #          #                          "GOTERM_CC_ALL" = "GOTERM_CC_ALL",
    #          #                          "GOTERM_MF_3" = "GOTERM_MF_3",
    #          #                          "GOTERM_MF_4" = "GOTERM_MF_4",
    #          #                          "GOTERM_MF_1" = "GOTERM_MF_1",
    #          #                          "GOTERM_MF_2" ="GOTERM_MF_2",
    #          #                          "GOTERM_MF_DIRECT" = "GOTERM_MF_DIRECT",
    #          #                          "GOTERM_MF_FAT" = "GOTERM_MF_FAT",
    #          #                          "GOTERM_MF_5" = "GOTERM_MF_5",
    #          #                          "GOTERM_MF_ALL" = "GOTERM_MF_ALL",
    #          #                          "HIV_INTERACTION_PUBMED_ID" = "HIV_INTERACTION_PUBMED_ID",
    #          #                          "HIV_INTERACTION_CATEGORY" = "HIV_INTERACTION_CATEGORY",
    #          #                          "HIV_INTERACTION" = "HIV_INTERACTION",
    #          #                          "KEGG_PATHWAY" = "KEGG_PATHWAY",
    #          #                          "INTERPRO" = "INTERPRO",
    #          #                          "INTACT" = "INTACT",
    #          #                          "OMIM_DISEASE" = "OMIM_DISEASE",
    #          #                          "MINT" = "MINT",
    #          #                          "PIR_SEQ_FEATURE" = "PIR_SEQ_FEATURE",
    #          #                          "PIR_SUMMARY" = "PIR_SUMMARY",
    #          #                          "PIR_SUPERFAMILY" = "PIR_SUPERFAMILY",
    #          #                          "PFAM" = "PFAM",
    #          #                          "PUBMED_ID" = "PUBMED_ID",
    #          #                          "REACTOME_PATHWAY" = "REACTOME_PATHWAY",
    #          #                          "SMART" = "SMART",
    #          #                          "PRINTS" = "PRINTS",
    #          #                          "PRODOM" = "PRODOM",
    #          #                          "PROSITE" = "PROSITE",
    #          #                          "UCSC_TFBS" = "UCSC_TFBS",
    #          #                          "UP_KEYWORDS" = "UP_KEYWORDS",
    #          #                          "UNIGENE_EST_QUARTILE" = "UNIGENE_EST_QUARTILE",
    #          #                          "SP_COMMENT_TYPE" = "SP_COMMENT_TYPE",
    #          #                          "SP_COMMENT" = "SP_COMMENT",
    #          #                          "TIGRFAMS" = "TIGRFAMS",
    #          #                          "SUPFAM" = "SUPFAM",
    #          #                          "UP_TISSUE" = "UP_TISSUE",
    #          #                          "UP_SEQ_FEATURE" = "UP_SEQ_FEATURE"))
    #          #   )),
    #          mainPanel(
    #              plotOutput("davidplot"),br(),
    #              textInput("davidconvert","enter entrezid"),br(),
    #              verbatimTextOutput("davidconverted"),br(),
    #              numericInput("cluster","Choose cluster",value=1),br(),
    #              verbatimTextOutput("davidtext"),br(),
    #              actionButton("download_david", "DAVID clusters file as \"termClusterReport1.tab\"")
    #          )),
    
    #Tab panel showing filtered data table
    tabPanel("Data table",
             # Page title
             titlePanel("Filtered datatable"),
             mainPanel(
                 DT::dataTableOutput("datatable"),
                 # Download button to save filtered data as CSV
                 downloadButton("download_datatable", "Download filtered data")
             ))
))

# Server
server <- function(input, output,session) {
    
    plasma <- read.csv("abi1_ko_aged_proteomics_data_ui\\plasma.csv")
    as.data.frame(plasma)
    
    cellular <- read.csv("abi1_ko_aged_proteomics_data_ui\\cellular.csv")
    as.data.frame(cellular)
    
    # This reactive expression checks if a file is uploaded then makes data frame out of uploaded CSV
    # data <- reactive({
    #     inFile <- input$dataset
    #     if (is.null(inFile)) return(NULL)
    #     myData <- read.csv(inFile$datapath, header = TRUE)
    #     as.data.frame(myData)
    # })
    
    # Pre-set thresholds for volcano plots tab
    
    observeEvent(input$reset_input, {
        updateNumericInput(session,"logfc",value=c(-1,1))
        updateNumericInput(session,"pval",value=1)
    })
    
    # This is the filtered data set, controlled by stats inputs and displayed on the top plot of tab "Volcano plots"
    # It is reactive to input changes (side panel in 'volcano plots' tab)
    # See above comment for explanation filtering (CTRL + F variable name)
    
    filtered_data_reactive <- reactive({
        if (input$set == "Plasma"){
            if (input$comp == 'Old KO vs. old WT'){
                    filtData <-
                        
                        plasma %>%
    
                        filter(
                            (as.numeric(as.character(logfc_oldko_oldwt)) <= input$logfc[1]&
                            as.numeric(as.character(pval_oldko_oldwt)) <= input$pval)|
                            (as.numeric(as.character(logfc_oldko_oldwt)) >= input$logfc[2]&
                            as.numeric(as.character(pval_oldko_oldwt)) <= input$pval)
                        )
                }else if (input$comp == "Young KO vs. young WT"){
                    filtData <-
                        
                        plasma %>%
                        
                        filter(
                            (as.numeric(as.character(logfc_youngko_youngwt)) <= input$logfc[1]&
                            as.numeric(as.character(pval_youngko_youngwt)) <= input$pval)|
                            (as.numeric(as.character(logfc_youngko_youngwt)) >= input$logfc[2]&
                            as.numeric(as.character(pval_youngko_youngwt)) <= input$pval)
                        )
                }else if (input$comp == 'Young KO vs. old KO'){
                        filtData <-
                        
                        plasma %>%
                        
                        filter(
                            (as.numeric(as.character(logfc_youngko_oldko)) <= input$logfc[1]&
                            as.numeric(as.character(pval_youngko_oldko)) <= input$pval)|
                            (as.numeric(as.character(logfc_youngko_oldko)) >= input$logfc[2]&
                            as.numeric(as.character(pval_youngko_oldko)) <= input$pval)
                        )
                }else if (input$comp == "Young WT vs. old WT"){
                    filtData <-
                        
                        plasma %>%
                        
                        filter(
                            (as.numeric(as.character(logfc_youngwt_oldwt)) <= input$logfc[1]&
                            as.numeric(as.character(pval_youngwt_oldwt)) <= input$pval)|
                            (as.numeric(as.character(logfc_youngwt_oldwt)) >= input$logfc[2]&
                            as.numeric(as.character(pval_youngwt_oldwt)) <= input$pval)
                        )
                }
    }else if (input$set == "Cellular"){
        if (input$comp == 'Old KO vs. old WT'){
            filtData <-
                
                cellular %>%
                
                filter(
                    (as.numeric(as.character(logfc_oldko_oldwt)) <= input$logfc[1]&
                     as.numeric(as.character(pval_oldko_oldwt)) <= input$pval)|
                    (as.numeric(as.character(logfc_oldko_oldwt)) >= input$logfc[2]&
                     as.numeric(as.character(pval_oldko_oldwt)) <= input$pval)
                )
        }else if (input$comp == "Young KO vs. young WT"){
            filtData <-
                
                cellular %>%
                
                filter(
                    (as.numeric(as.character(logfc_youngko_youngwt)) <= input$logfc[1]&
                     as.numeric(as.character(pval_youngko_youngwt)) <= input$pval)|
                    (as.numeric(as.character(logfc_youngko_youngwt)) >= input$logfc[2]&
                     as.numeric(as.character(pval_youngko_youngwt)) <= input$pval)
                )
        }else if (input$comp == 'Young KO vs. old KO'){
            filtData <-
                
                cellular %>%
                
                filter(
                    (as.numeric(as.character(logfc_youngko_oldko)) <= input$logfc[1]&
                     as.numeric(as.character(pval_youngko_oldko)) <= input$pval)|
                    (as.numeric(as.character(logfc_youngko_oldko)) >= input$logfc[2]&
                     as.numeric(as.character(pval_youngko_oldko)) <= input$pval)
                )
        }else if (input$comp == "Young WT vs. old WT"){
            filtData <-
                
                cellular %>%
                
                filter(
                    (as.numeric(as.character(logfc_youngwt_oldwt)) <= input$logfc[1]&
                     as.numeric(as.character(pval_youngwt_oldwt)) <= input$pval)|
                    (as.numeric(as.character(logfc_youngwt_oldwt)) >= input$logfc[2]&
                     as.numeric(as.character(pval_youngwt_oldwt)) <= input$pval)
                )
        }
    # }else if (input$set == "Both"){
    #     if (input$comp == 'Old KO vs. old WT'){
    #         filtData <-
    #             
    #             filter(
    #                 cellular %>%
    #                 (as.numeric(as.character(logfc_oldko_oldwt)) <= input$logfc[1]&
    #                  as.numeric(as.character(pval_oldko_oldwt)) <= input$pval)|
    #                 (as.numeric(as.character(logfc_oldko_oldwt)) >= input$logfc[2]&
    #                  as.numeric(as.character(pval_oldko_oldwt)) <= input$pval)&
    #                 plasma %>%
    #                 (as.numeric(as.character(logfc_oldko_oldwt)) <= input$logfc[1]&
    #                  as.numeric(as.character(pval_oldko_oldwt)) <= input$pval)|
    #                 (as.numeric(as.character(logfc_oldko_oldwt)) >= input$logfc[2]&
    #                  as.numeric(as.character(pval_oldko_oldwt)) <= input$pval)
    #             )
    #     }else if (input$comp == "Young KO vs. young WT"){
    #         filtData <-
    # 
    #             filter(
    #             cellular %>%
    #                 (as.numeric(as.character(logfc_youngko_youngwt)) <= input$logfc[1]&
    #                  as.numeric(as.character(pval_youngko_youngwt)) <= input$pval)|
    #                 (as.numeric(as.character(logfc_youngko_youngwt)) >= input$logfc[2]&
    #                  as.numeric(as.character(pval_youngko_youngwt)) <= input$pval)&
    #             plasma %>%
    #                 (as.numeric(as.character(logfc_youngko_youngwt)) <= input$logfc[1]&
    #                  as.numeric(as.character(pval_youngko_youngwt)) <= input$pval)|
    #                 (as.numeric(as.character(logfc_youngko_youngwt)) >= input$logfc[2]&
    #                  as.numeric(as.character(pval_youngko_youngwt)) <= input$pval)
    #                 )
    #     }else if (input$comp == 'Young KO vs. old KO'){
    #         filtData <-
    #             
    #             filter(
    #             cellular %>%
    #                 (as.numeric(as.character(logfc_youngko_oldko)) <= input$logfc[1]&
    #                  as.numeric(as.character(pval_youngko_oldko)) <= input$pval)|
    #                 (as.numeric(as.character(logfc_youngko_oldko)) >= input$logfc[2]&
    #                  as.numeric(as.character(pval_youngko_oldko)) <= input$pval)&
    #             plasma %>%
    #                 (as.numeric(as.character(logfc_youngko_oldko)) <= input$logfc[1]&
    #                  as.numeric(as.character(pval_youngko_oldko)) <= input$pval)|
    #                 (as.numeric(as.character(logfc_youngko_oldko)) >= input$logfc[2]&
    #                  as.numeric(as.character(pval_youngko_oldko)) <= input$pval)
    #             )
    #     }else if (input$comp == "Young WT vs. old WT"){
    #         filtData <-
    #             filter(
    #             cellular %>%
    #     
    #                 (as.numeric(as.character(logfc_youngwt_oldwt)) <= input$logfc[1]&
    #                  as.numeric(as.character(pval_youngwt_oldwt)) <= input$pval)|
    #                 (as.numeric(as.character(logfc_youngwt_oldwt)) >= input$logfc[2]&
    #                  as.numeric(as.character(pval_youngwt_oldwt)) <= input$pval)&
    #             plasma %>%
    #                 (as.numeric(as.character(logfc_youngwt_oldwt)) <= input$logfc[1]&
    #                  as.numeric(as.character(pval_youngwt_oldwt)) <= input$pval)|
    #                 (as.numeric(as.character(logfc_youngwt_oldwt)) >= input$logfc[2]&
    #                  as.numeric(as.character(pval_youngwt_oldwt)) <= input$pval)
    #             )
    }})
    
    
    # # This is the filtered data set, controlled by stats inputs and displayed on the BOTTOM plot
    # # It is reactive to input changes (side panel in 'volcano plots' tab
    # # NB! NOT REACTIVE FOR COMMENTED LINES (ie ratios)
    # # See above comment for explanation filtering (CTRL + F variable name) 
    # filtered_data_missing_control_reactive <- reactive({
    #     req(input$dataset)
    #     
    #     if (input$tails == 'one'){
    #         if (input$mod == "both"){
    #             filtData <-
    #                 
    #                 data() %>%
    #                 
    #                 filter(
    #                     
    #                     as.numeric(as.character(average_psm_turbocontrol)) == 0&
    #                         #average_psm_wtcontrol == 0 &
    #                         #    percentage_abi_wt_abundance == 100 |
    #                         #   percentage_abi_wt_psm == 100,
    #                         #exp_control_pa_ratio >= input$exp_control_pa_ratio,
    #                         as.numeric(as.character(average_psm_turboabi)) >= input$exp_psm_average&
    #                         #        exp_control_psm_ratio >= input$exp_control_psm_ratio,
    #                         # ratio_abi_wt_abundance >= input$exp_wt_pa_ratio,
    #                         # ratio_abi_wt_psm >= input$exp_wt_psm_ratio,
    #                         as.numeric(as.character(qval_abi_wt_abundance)) <= input$exp_wt_pa_fdr&
    #                         as.numeric(as.character(qval_abi_turbo_abundance)) <= input$exp_control_pa_fdr&
    #                         as.numeric(as.character(qval_abi_turbo_psm)) <= input$exp_control_psm_fdr&
    #                         as.numeric(as.character(qval_abi_wt_psm)) <= input$exp_wt_psm_fdr
    #                     # exp_control_pa_pval <= input$exp_control_pa_pval
    #                 )
    #         }else if (input$mod == "either"){
    #             filtData <-
    #                 
    #                 data() %>%
    #                 
    #                 filter(
    #                     as.numeric(as.character(average_psm_turbocontrol)) == 0&
    #                         #average_psm_wtcontrol == 0 &
    #                         #    percentage_abi_wt_abundance == 100 |
    #                         #   percentage_abi_wt_psm == 100,
    #                         #exp_control_pa_ratio >= input$exp_control_pa_ratio,
    #                         as.numeric(as.character(average_psm_turboabi)) >= input$exp_psm_average&
    #                         #        exp_control_psm_ratio >= input$exp_control_psm_ratio,
    #                         # ratio_abi_wt_abundance >= input$exp_wt_pa_ratio,
    #                         # ratio_abi_wt_psm >= input$exp_wt_psm_ratio,
    #                         as.numeric(as.character(qval_abi_wt_abundance)) <= input$exp_wt_pa_fdr&
    #                         as.numeric(as.character(qval_abi_turbo_abundance)) <= input$exp_control_pa_fdr|
    #                         as.numeric(as.character(qval_abi_turbo_psm)) <= input$exp_control_psm_fdr&
    #                         as.numeric(as.character(qval_abi_wt_psm)) <= input$exp_wt_psm_fdr
    #                     # exp_control_pa_pval <= input$exp_control_pa_pval
    #                 )
    #         }
    #     }else if (input$tails == 'two'){
    #         if (input$mod == "both"){
    #             filtData <-
    #                 
    #                 data() %>%
    #                 
    #                 filter(
    #                     
    #                     as.numeric(as.character(average_psm_turbocontrol)) == 0&
    #                         #average_psm_wtcontrol == 0 &
    #                         #    percentage_abi_wt_abundance == 100 |
    #                         #   percentage_abi_wt_psm == 100,
    #                         #        exp_control_pa_ratio >= input$exp_control_pa_ratio,
    #                         as.numeric(as.character(average_psm_turboabi)) >= input$exp_psm_average&
    #                         #        exp_control_psm_ratio >= input$exp_control_psm_ratio,
    #                         #ratio_abi_wt_abundance >= input$exp_wt_pa_ratio,
    #                         #ratio_abi_wt_psm >= input$exp_wt_psm_ratio,
    #                         as.numeric(as.character(two_tailed_qval_abi_wt_abundance)) <= input$exp_wt_pa_fdr&
    #                         as.numeric(as.character(two_tailed_qval_abi_turbo_abundance)) <= input$exp_control_pa_fdr&
    #                         as.numeric(as.character(two_tailed_qval_abi_turbo_psm)) <= input$exp_control_psm_fdr&
    #                         as.numeric(as.character(two_tailed_qval_abi_wt_psm)) <= input$exp_wt_psm_fdr
    #                 )
    #         }else if (input$mod == "either"){
    #             filtData <-
    #                 
    #                 data() %>%
    #                 
    #                 filter(
    #                     as.numeric(as.character(average_psm_turbocontrol)) == 0&
    #                         #average_psm_wtcontrol == 0 &
    #                         #    percentage_abi_wt_abundance == 100 |
    #                         #   percentage_abi_wt_psm == 100,
    #                         #exp_control_pa_ratio >= input$exp_control_pa_ratio,
    #                         as.numeric(as.character(average_psm_turboabi)) >= input$exp_psm_average&
    #                         #        exp_control_psm_ratio >= input$exp_control_psm_ratio,
    #                         # ratio_abi_wt_abundance >= input$exp_wt_pa_ratio,
    #                         # ratio_abi_wt_psm >= input$exp_wt_psm_ratio,
    #                         as.numeric(as.character(qval_abi_wt_abundance)) <= input$exp_wt_pa_fdr&
    #                         as.numeric(as.character(qval_abi_turbo_abundance)) <= input$exp_control_pa_fdr|
    #                         as.numeric(as.character(qval_abi_turbo_psm)) <= input$exp_control_psm_fdr&
    #                         as.numeric(as.character(qval_abi_wt_psm)) <= input$exp_wt_psm_fdr
    #                 )
    #         }
    #     }
    #     #        exp_control_pa_fdr_1 >= input$exp_control_pa_fdr_1,
    #     #        exp_control_pa_fdr_2 >= input$exp_control_pa_fdr_2,
    #     #        exp_control_pa_fdr_3 >= input$exp_control_pa_fdr_3
    #     
    # })
    # 
    # # Defining known interactors in dataset
    # filtered_data_missing_control_interactors_reactive <- reactive({
    #     req(input$dataset)
    #     filtDataInts <- 
    #         filtered_data_missing_control_reactive() %>%
    #         filter(
    #             known_interactor == "TRUE"
    #         )
    # })
    # filtered_data_interactors_reactive <- reactive({
    #     req(input$dataset)
    #     filtDataInts <- 
    #         filtered_data_reactive() %>%
    #         filter(
    #             known_interactor == "TRUE"
    #         )
    # })
    
    # metadata
    # output$pvalue_distribution <- renderPlot({
    #   req(input$dataset)
    #   hist(filtered_data_combined()[["pval_abi_turbo_abundance"]],main="Histogram of TurboAbi:Turbo abundance p-value")
    # })
    # 
    # # P-value distribution plot on Data upload/metadata tab
    # output$pvalue_distribution <- renderPlot({
    #     req(input$dataset)
    #     hist(filtered_data_reactive()[["pval_abi_turbo_abundance"]],
    #          main="Histogram of TurboAbi:Turbo abundance p-value",
    #          xlab="TurboAbi:TurboControl PA p-value")
    # })
    
    # output$histogram <- renderPlot({
    #   req(input$dataset)
    #   hist(filtered_data_combined()$percentage_abi_turbo_abundance,main="Histogram of TurboAbi:Turbo abundance ratio")
    # })
    
    # # Percentage enrichment histogram on Data upload/metadata tab
    # output$histogram <- renderPlot({
    #     req(input$dataset)
    #     hist(filtered_data_reactive()$percentage_abi_turbo_abundance,
    #          main="Histogram of TurboAbi:Turbo abundance ratio",
    #          xlab="TurboAbi:TurboControl PA percentage enrichment")
    # })
    
    output$datatable_head <- renderTable({
     #   req(input$dataset)
        head(data())
    })
    
    # volcano_plot
    output$volcano_plot <- renderPlotly({
        
        p <- ggplot(data = filtered_data_reactive(), 
                    if (input$comp == "Old KO vs. old WT"){
                        aes(
                            x = as.numeric(as.character(logfc_oldko_oldwt)),
                            y = -log10(as.numeric(as.character(pval_oldko_oldwt))),
                        text=paste("Protein: ", gene, "\n",
                              "LogFC: ", logfc_oldko_oldwt, "\n",
                              "Adjusted P-value: ", pval_oldko_oldwt))}
                    else if (input$comp == "Young KO vs. young WT"){
                        aes(
                            x = as.numeric(as.character(logfc_youngko_youngwt)),
                            y=-log10(as.numeric(as.character(pval_youngko_youngwt))),
                            text=paste("Protein: ", gene, "\n",
                                       "LogFC: ", logfc_youngko_youngwt, "\n",
                                       "Adjusted P-value: ", pval_youngko_youngwt))}
                    else if (input$comp == "Young KO vs. old KO"){
                         aes(
                            x = as.numeric(as.character(logfc_youngko_oldko)),
                            y=-log10(as.numeric(as.character(pval_youngko_oldko))),
                            text=paste("Protein: ", gene, "\n",
                                       "LogFC: ", logfc_youngko_oldko, "\n",
                                       "Adjusted P-value: ", pval_youngko_oldko))}
                    else if (input$comp == "Young WT vs. old WT"){
                         aes(
                            x = as.numeric(as.character(logfc_youngwt_oldwt)),
                            y=-log10(as.numeric(as.character(pval_youngwt_oldwt))),
                            text=paste("Protein: ", gene, "\n",
                                      "LogFC: ", logfc_youngwt_oldwt, "\n",
                                      "Adjusted P-value: ", pval_youngwt_oldwt))}) +
        # interactors = paste(length(gene), "interactors")) +
        geom_point(aes(size=2)) +
        #     #    xlim(min(log2(filtered_data_reactive()$ratio_abi_turbo_abundance)),max(log2(filtered_data_reactive()$ratio_abi_turbo_abundance))) +
        #     #    ylim(c(0, max(filtered_data_reactive()$neg_log10_pval_abi_turbo_abundance))) +
        labs(x = "LogFC",
             y = "-log10(adjusted p-value)",
             title = "LogFC vs. p-value") +
            # geom_hline(yintercept = 1.3, linetype = "dashed", color = "red") + # Horizontal line at -log10(pval = 0.05)
            # geom_vline(xintercept = 0.59, linetype = "dashed", color = "red") + #Vertical line at EXP vs. CONTROL = 1.5
            theme_classic()
        
        # Makes plot interactive with plotly (mouseover enabled, I can't figure out why the legend doesn't go away)
        p <- ggplotly(p, tooltip = "text", showLegend = FALSE)
        
        # p %>% layout(xaxis = list(range=c(min(log2(filtered_data_reactive()$ratio_abi_turbo_abundance)),max(log2(filtered_data_reactive()$ratio_abi_turbo_abundance)))),
        #              yaxis = if (input$tails == "one"){list(range=c(0,max(filtered_data_reactive()$neg_log10_pval_abi_turbo_abundance)))}
        #              else if (input$tails == "two"){list(range=c(0,max(filtered_data_reactive()$two_tailed_neg_log10_pval_abi_turbo_abundance)))})
        # 
        
        # This adds metadata right below plot
    #     p %>%
    #         layout(
    #             annotations = 
    #                 list(x = 1, y = -0.14, text = paste(length(filtered_data_reactive()$gene), "interactors.", "\n",
    #                                                     length(filtered_data_interactors_reactive()$gene), "known interactors"),
    #                      showarrow = F, xref='paper', yref='paper',
    #                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
    #                      font=list(size=12, color="black")))
    # })
    # 
    # br()
    # 
    # # volcano_plot_missing_control
    # output$volcano_plot_missing_control <- renderPlotly({
    #     
    #     req(input$dataset)
    #     
    #     # x_axis_selected <- switch(input$x_axis,
    #     #              "exp_control_pa_percentage1" = filtered_data_reactive()$exp_control_pa_percentage,
    #     #              "exp_control_pa_ratio1" = filtered_data_reactive()$exp_control_pa_ratio,
    #     #              "exp_psm_count1" = filtered_data_reactive()$exp_psm_count,
    #     #              "exp_wt_pa_ratio1" = filtered_data_reactive()$exp_wt_pa_ratio,
    #     #              "exp_control_psm_ratio1" = filtered_data_reactive()$exp_control_psm_ratio,
    #     #              "exp_control_psm_percentage1" = filtered_data_reactive()$exp_control_psm_percentage,
    #     #              "exp_wt_psm_ratio1" = filtered_data_reactive()$exp_wt_psm_ratio,
    #     #              "exp_wt_psm_percentage1" = filtered_data_reactive()$exp_wt_psm_percentage,
    #     #              "exp_wt_pa_percentage1" = filtered_data_reactive()$exp_wt_pa_percentage)
    #     
    #     p <- ggplot(data = filtered_data_missing_control_reactive(), aes(x = as.numeric(as.character(rsd_abi_abundance)),
    #                                                                      if (input$tails=="one"){y = as.numeric(as.character(neg_log10_pval_abi_turbo_abundance))}
    #                                                                      else if (input$tails=="two"){y = as.numeric(as.character(two_tailed_neg_log10_pval_abi_turbo_abundance))},
    #                                                                      text = if (input$tails == "one"){paste("Protein: ", gene, "\n",
    #                                                                                                             "EXP vs. CONTROL PA (% / ratio): ", percentage_abi_turbo_abundance, "/", ratio_abi_turbo_abundance, "\n",
    #                                                                                                             "EXP vs. WT PA (% / ratio): ", percentage_abi_wt_abundance, "/", ratio_abi_wt_abundance, "\n",
    #                                                                                                             "EXP vs. CONTROL PSM (% / ratio): ", percentage_abi_turbo_psm, "/", ratio_abi_turbo_psm, "\n",
    #                                                                                                             "EXP vs. WT PSM (% / ratio): ", percentage_abi_wt_psm, "/", ratio_abi_wt_psm, "\n",
    #                                                                                                             "EXP vs. CONTROL FDR: ", qval_abi_turbo_abundance, "\n",
    #                                                                                                             "EXP vs. CONTROL p-value", pval_abi_turbo_abundance, "\n",
    #                                                                                                             "EXP vs. WT FDR: ", qval_abi_wt_abundance)}
    #                                                                      else if (input$tails=="two"){paste("Protein: ", gene, "\n",
    #                                                                                                         "EXP vs. CONTROL PA (% / ratio): ", percentage_abi_turbo_abundance, "/", ratio_abi_turbo_abundance, "\n",
    #                                                                                                         "EXP vs. WT PA (% / ratio): ", percentage_abi_wt_abundance, "/", ratio_abi_wt_abundance, "\n",
    #                                                                                                         "EXP vs. CONTROL PSM (% / ratio): ", percentage_abi_turbo_psm, "/", ratio_abi_turbo_psm, "\n",
    #                                                                                                         "EXP vs. WT PSM (% / ratio): ", percentage_abi_wt_psm, "/", ratio_abi_wt_psm, "\n",
    #                                                                                                         "EXP vs. CONTROL FDR: ", two_tailed_qval_abi_turbo_abundance, "\n",
    #                                                                                                         "EXP vs. CONTROL p-value", two_tailed_pval_abi_turbo_abundance, "\n",
    #                                                                                                         "EXP vs. WT FDR: ", two_tailed_qval_abi_wt_abundance)}),
    #                 interactors = paste(length(gene), "interactors")) +
    #         geom_point(aes(fill=known_interactor, size=2)) +
    #         # xlim(c(min(filtered_data_missing_control_reactive()$percentage_abi_turbo_abundance), max(filtered_data_missing_control_reactive()$percentage_abi_turbo_abundance))) +
    #         # ylim(c(0, max(filtered_data_missing_control_reactive()$neg_log10_pval_abi_turbo_abundance))) +
    #         labs(x = "TurboAbi abundance RSD",
    #              y = "-log10(EXP:CONTROL PA p-value)",
    #              title = "As above, with proteins NOT ID'd in TurboControl",
    #              caption = "interactors") +
    #         geom_hline(yintercept = 1.3, linetype = "dashed", color = "red") +
    #         #geom_vline(xintercept = 66, linetype = "dashed", color = "red") + # ctrl+F 'transformation' above
    #         theme_classic()
    #     
    #     p <- ggplotly(p, tooltip = "text", showLegend = FALSE)
    #     
    #     p %>% layout(xaxis = list(range=c(min(as.numeric(as.character(filtered_data_missing_control_reactive()$rsd_abi_abundance))),max(as.numeric(as.character(filtered_data_missing_control_reactive()$rsd_abi_abundance))))),
    #                  yaxis = if (input$tails == "one"){list(range=c(0,max(as.numeric(as.character(filtered_data_missing_control_reactive()$neg_log10_pval_abi_turbo_abundance)))))}
    #                  else if (input$tails == "two"){list(range=c(0,max(as.numeric(as.character(filtered_data_missing_control_reactive()$two_tailed_neg_log10_pval_abi_turbo_abundance)))))})
    #     
    #     p %>%
    #         layout(annotations = 
    #                    list(x = 1, y = -0.14, text = paste(length(filtered_data_missing_control_reactive()$gene), "interactors not shown above.", "\n",
    #                                                        length(filtered_data_missing_control_interactors_reactive()$gene), "known interactors not shown above"),
    #                         showarrow = F, xref='paper', yref='paper',
    #                         xanchor='right', yanchor='auto', xshift=0, yshift=0,
    #                         font=list(size=12, color="black")))
    
        })
    
    # Reactive to attach filtered_data and filtered_data_missing_controls (for pathway analysis and data table)
    # filtered_data_combined <- reactive({
    #   
    #   req(input$dataset)
    #   rbind(filtered_data_reactive(), filtered_data_missing_control_reactive())
    #   
    #   })
    
    # # Use enrich pathway to make reactome bar plots
    # output$reactome_bar_plot <- renderPlot({
    #   
    #   data_entrezid <- filtered_data_combined()$entrezid
    #   x <- enrichPathway(gene=data_entrezid,pvalueCutoff=0.05, organism = "mouse", readable=T)
    #   head(as.data.frame(x))
    #   barplot(x, showCategory = 15)
    # })
    # 
    # # Use enrich pathway to make reactome data table
    # # NOTE: these two (^ v) can be combined to cut query time in half?
    # output$reactome_table <- DT::renderDataTable({
    #   data_entrezid <- filtered_data_combined()$entrezid
    #   x <- enrichPathway(gene=data_entrezid,pvalueCutoff=0.05, organism = "mouse", readable=T)
    #   as.data.frame(x)
    # })
    
    # Use enrich pathway to make reactome bar plots
    
    
    #data_entrezid <- reactive({filtered_data_combined()$ENTREZID})
    
    # ENTREZID column for filtered proteins (need ENTREZID for Reactome analyses)
    
    data_entrezid <- reactive({filtered_data_reactive()$ENTREZID})
    
    # Reactome query
    pathway <- reactive({
        enrichPathway(gene=data_entrezid(),pvalueCutoff=0.05, organism = "mouse", readable=T)
    })
    
    # Reactome bar blot
    output$reactome_bar_plot <- renderPlot({
        head(as.data.frame(pathway()))
        barplot(pathway(), showCategory = 15)
        
    })
    
    # Reactome datatable
    output$reactome_table <- DT::renderDataTable({
        as.data.frame(pathway())
    })
    
    # 
    # pf <- reactive({
    #   filtered_data_reactive()[,c("gene","qval_abi_turbo_abundance")]
    # })
    # 
    # library(purrr)
    # 
    # output$pathfindr <- renderPlot({
    #   possibly(run_pathfindR(pf()))
    # })
    
    # Data table        
    # output$datatable = DT::renderDataTable(
    #   filtered_data_combined()
    #   )
    
    # Datatable on datatable tab
    output$datatable = DT::renderDataTable(
        filtered_data_reactive()
    )
    
    # Output datatable by download button
    output$download_datatable <- downloadHandler(
        
        filename = function() {
            paste(Sys.Date(),"logfc",input$logfc, "pval ",input$pval,".csv",sep="")
        },
        # content = function(file) {
        #   s = input$datatable_rows_all
        #   write.csv(filtered_data_combined()
        #     [s, , drop = FALSE], file)
        # }
        content = function(file) {
            s = input$datatable_rows_all
            write.csv(filtered_data_reactive()
                      [s, , drop = FALSE], file)
        }
    )
    
    # StringDB query
    library(STRINGdb)
    string_db <- STRINGdb$new(version="11", species=10090,
                              score_threshold=400, input_directory="")
    string_mapped_proteins <- reactive({
        string_db$map(filtered_data_reactive(), "gene", removeUnmappedRows = TRUE)
    })
    hits <- reactive({
        string_mapped_proteins()$STRING_id[1:200]
    })
    # string_mapped_pval05 <- reactive({
    #     string_db$add_diff_exp_color(subset(string_mapped_proteins(), qval_abi_turbo_abundance < 1),
    #                                  logFcColStr = "qval_abi_turbo_abundance")
    # })
    # payload_id <- reactive({
    #     string_db$post_payload(string_mapped_pval05()$STRING_id,
    #                            colors=string_mapped_pval05()$color)
    #})
    output$string = renderPlot(  
        #output$string = string_db$plot_network(hits)
        #names(string_mapped_proteins)
        string_db$plot_network(hits()), #payload_id=payload_id()),
        height = 2000, width = 2000
    )
    
    
    # this section implemented DAVID analysis - as of 9/15/2021 this is broken, with error:
    # [INFO] Unable to sendViaPost to url[https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/]
    # org.apache.axis2.AxisFault: Transport error: 403 Error: Forbidden
    # at org.apache.axis2.transport.http.HTTPSender.handleResponse(HTTPSender.java:310)
    # at org.apache.axis2.transport.http.HTTPSender.sendViaPost(HTTPSender.java:200)
    # at org.apache.axis2.transport.http.HTTPSender.send(HTTPSender.java:76)
    # at org.apache.axis2.transport.http.CommonsHTTPTransportSender.writeMessageWithCommons(CommonsHTTPTransportSender.java:400)
    # at org.apache.axis2.transport.http.CommonsHTTPTransportSender.invoke(CommonsHTTPTransportSender.java:225)
    # at org.apache.axis2.engine.AxisEngine.send(AxisEngine.java:435)
    # at org.apache.axis2.description.OutInAxisOperationClient.send(OutInAxisOperation.java:402)
    # at org.apache.axis2.description.OutInAxisOperationClient.executeImpl(OutInAxisOperation.java:229)
    # at org.apache.axis2.client.OperationClient.execute(OperationClient.java:165)
    # at sample.session.client.stub.DAVIDWebServiceStub.getAllListNames(DAVIDWebServiceStub.java:2533)
    # at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
    # at sun.reflect.NativeMethodAccessorImpl.invoke(Unknown Source)
    # at sun.reflect.DelegatingMethodAccessorImpl.invoke(Unknown Source)
    # at java.lang.reflect.Method.invoke(Unknown Source)
    # at RJavaTools.invokeMethod(RJavaTools.java:386)
    # Warning: Error in <Anonymous>: error in evaluating the argument 'object' in selecting a method for function 'summary': org.apache.axis2.AxisFault: Transport error: 403 Error: Forbidden
    # 132: <Anonymous>
    #     Warning: Error in <Anonymous>: error in evaluating the argument 'object' in selecting a method for function 'plot2D': org.apache.axis2.AxisFault: Transport error: 403 Error: Forbidden
    # 173: <Anonymous>
    
    # library(RDAVIDWebService)
    # 
    # termCluster<-reactive({
    #     david<-DAVIDWebService(email="max_petersen@brown.edu", url="https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
    #     setAnnotationCategories(david, c(input$david_cats))
    #     result <-addList(david, data_entrezid(),
    #                      idType="ENTREZ_GENE_ID",
    #                      listName="data_entrezid()", listType="Gene")
    #     #file1<-getClusterReportFile(david,type="Term",filename="termClusterReport1.tab")
    #     file <- getClusterReportFile(david, type="Term",
    #                                  fileName="termClusterReport1.tab")
    #     file<-getClusterReport(david, type="Term")
    #     
    #     
    # })
    # 
    # output$davidtext = renderPrint(summary(termCluster()))
    # output$davidplot = renderPlot(plot2D(termCluster(),input$cluster))
    # output$davidconverted = renderPrint(mapIds(org.Mm.eg.db,input$davidconvert,'SYMBOL','ENTREZID'))
    # 
    # Output david clusters by download button
    # output$download_david <-getClusterReportFile(david, type="Term",
    #                                               fileName="termClusterReport1.tab")
    # )
    
    #enrichment() <- string_db$get_enrichment(hits())
    
    # output$string_datatable = DT::renderDataTable(
    #  head(as.data.frame(enrichment()), n=20)
}

# citations for packages
citation("shiny")
citation("plotly")
citation("scales")
citation("ggplot2")
citation("ReactomePA")
citation("DT")
citation("enrichplot")
citation("org.Mm.eg.db")
citation("org.Hs.eg.db")

# Run the application 
shinyApp(ui = ui, server = server)
