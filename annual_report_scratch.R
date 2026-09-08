#############UI definition

strong("Table 5-1: Summary of Post-Construction CWL Monitoring of Public SMPs"),
	reactableOutput((ns("Summary of Post-Construction CWL Monitoring of Public SMPs"))),
	
strong("Table 5-2: Post-Construction CWL Monitoring of Public SMPs Listed by Type"),
	reactableOutput((ns("Post-Construction CWL Monitoring of Public SMPs Listed by Type"))),
	
strong("Table 5-3: Post-Construction SRTs performed on Public Systems"),
	reactableOutput((ns("Post-Construction SRTs performed on Public Systems"))),
	
strong("Table 5-4: Public Systems with Post-Construction SRTs Performed"),
	reactableOutput((ns("Public Systems with Post-Construction SRTs Performed"))),
	
strong("Table 5-5: Construction-Phase SRTs Performed on Public Systems"),
	reactableOutput((ns("Construction-Phase SRTs Performed on Public Systems"))),
	
strong("Table 5-6: Public Systems with Construction-Phase SRTs Performed"),
	reactableOutput((ns("Public Systems with Construction-Phase SRTs Performed"))),
	
strong("Table 5-7: Public Systems with CETs Administered"),
	reactableOutput((ns("Public Systems with CETs Administered"))),
	
strong("Table 5-8: Public Systems with Infiltration Testing Administered"),
	reactableOutput((ns("Public Systems with Infiltration Testing Administered"))),
	
strong("Table 5-9: Public Systems with Inlet Leakage Tests Administered"),
	reactableOutput((ns("Public Systems with Inlet Leakage Tests Administered"))),
	
strong("Table 5-10: Inlet Conveyance Tests Performed on Public Systems"),
	reactableOutput((ns("Inlet Conveyance Tests Performed on Public Systems"))),
	
strong("Table 5-11: Groundwater Monitoring for Public GSI"),
	reactableOutput((ns("Groundwater Monitoring for Public GSI"))),
	
strong("Table 6-1: Summary of Post-Construction CWL Monitoring of Private Systems"),
	reactableOutput((ns("Summary of Post-Construction CWL Monitoring of Private Systems"))),
	
strong("Table 6-2: Post-Construction CWL Monitoring of Private Systems Listed by Type"),
	reactableOutput((ns("Post-Construction CWL Monitoring of Private Systems Listed by Type"))),
	
strong("Table 6-3: Post-Construction SRTs performed on Private Systems"),
	reactableOutput((ns("Post-Construction SRTs performed on Private Systems"))),
	
strong("Table 6-4: Private SMPs with Post-Construction SRTs Performed"),
	reactableOutput((ns("Private SMPs with Post-Construction SRTs Performed"))),
	
strong("Table 6-5: Private Systems with CETs Administered"),
	reactableOutput((ns("Private Systems with CETs Administered"))),
	
strong("Table 6-6: Private Systems with ICTs Administered"),
	reactableOutput((ns("Private Systems with ICTs Administered"))),
	
strong("Table 6-7: Private Systems with WWIs Administered"),
	reactableOutput((ns("Private Systems with WWIs Administered")))

#UI output placeholder names
table_5_1 <- reactive({

	})


table_5_2 <- reactive({

	})


table_5_3 <- reactive({

	})


table_5_4 <- reactive({

	})


table_5_5 <- reactive({

	})


table_5_6 <- reactive({

	})


table_5_7 <- reactive({

	})


table_5_8 <- reactive({

	})


table_5_9 <- reactive({

	})


table_5_10 <- reactive({

	})


table_5_11 <- reactive({

	})


table_6_1 <- reactive({

	})


table_6_2 <- reactive({

	})


table_6_3 <- reactive({

	})


table_6_4 <- reactive({

	})


table_6_5 <- reactive({

	})


table_6_6 <- reactive({

	})


table_6_7 <- reactive({
	
	})
	
########## UI output
output$`Summary of Post-Construction CWL Monitoring of Public SMPs` <- renderReactable(reactable(table_5_1()), striped = TRUE, pagination = FALSE)
output$`Post-Construction CWL Monitoring of Public SMPs Listed by Type` <- renderReactable(reactable(table_5_2()), striped = TRUE, pagination = FALSE)
output$`Post-Construction SRTs performed on Public Systems` <- renderReactable(reactable(table_5_3()), striped = TRUE, pagination = FALSE)
output$`Public Systems with Post-Construction SRTs Performed` <- renderReactable(reactable(table_5_4()), striped = TRUE, pagination = FALSE)
output$`Construction-Phase SRTs Performed on Public Systems` <- renderReactable(reactable(table_5_5()), striped = TRUE, pagination = FALSE)
output$`Public Systems with Construction-Phase SRTs Performed` <- renderReactable(reactable(table_5_6()), striped = TRUE, pagination = FALSE)
output$`Public Systems with CETs Administered` <- renderReactable(reactable(table_5_7()), striped = TRUE, pagination = FALSE)
output$`Public Systems with Infiltration Testing Administered` <- renderReactable(reactable(table_5_8()), striped = TRUE, pagination = FALSE)
output$`Public Systems with Inlet Leakage Tests Administered` <- renderReactable(reactable(table_5_9()), striped = TRUE, pagination = FALSE)
output$`Inlet Conveyance Tests Performed on Public Systems` <- renderReactable(reactable(table_5_10()), striped = TRUE, pagination = FALSE)
output$`Groundwater Monitoring for Public GSI` <- renderReactable(reactable(table_5_11()), striped = TRUE, pagination = FALSE)
output$`Summary of Post-Construction CWL Monitoring of Private Systems` <- renderReactable(reactable(table_6_1()), striped = TRUE, pagination = FALSE)
output$`Post-Construction CWL Monitoring of Private Systems Listed by Type` <- renderReactable(reactable(table_6_2()), striped = TRUE, pagination = FALSE)
output$`Post-Construction SRTs performed on Private Systems` <- renderReactable(reactable(table_6_3()), striped = TRUE, pagination = FALSE)
output$`Private SMPs with Post-Construction SRTs Performed` <- renderReactable(reactable(table_6_4()), striped = TRUE, pagination = FALSE)
output$`Private Systems with CETs Administered` <- renderReactable(reactable(table_6_5()), striped = TRUE, pagination = FALSE)
output$`Private Systems with ICTs Administered` <- renderReactable(reactable(table_6_6()), striped = TRUE, pagination = FALSE)
output$`Private Systems with WWIs Administered` <- renderReactable(reactable(table_6_7()), striped = TRUE, pagination = FALSE)

          output$download_table <- downloadHandler(
            
            filename = function() {
              paste("FY",input$fy,"_","AnnualReport","_",Sys.Date(),".xlsx", sep = "")
            },
            content = function(filename){
              
              df_list <- list(table_5_1 = table_5_1(),
								table_5_2 = table_5_2(),
								table_5_3 = table_5_3(),
								table_5_4 = table_5_4(),
								table_5_5 = table_5_5(),
								table_5_6 = table_5_6(),
								table_5_7 = table_5_7(),
								table_5_8 = table_5_8(),
								table_5_9 = table_5_9(),
								table_5_10 = table_5_10(),
								table_5_11 = table_5_11(),
								table_6_1 = table_6_1(),
								table_6_2 = table_6_2(),
								table_6_3 = table_6_3(),
								table_6_4 = table_6_4(),
								table_6_5 = table_6_5(),
								table_6_6 = table_6_6(),
								table_6_7 = table_6_7())
              write.xlsx(x = df_list , file = filename, rowNames = TRUE)
            }
          ) 