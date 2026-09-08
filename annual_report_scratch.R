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
	
########## UI output
output$`Summary of Post-Construction CWL Monitoring of Public SMPs` <- renderReactable(reactable(xxx), striped = TRUE, pagination = FALSE)
output$`Post-Construction CWL Monitoring of Public SMPs Listed by Type` <- renderReactable(reactable(xxx), striped = TRUE, pagination = FALSE)
output$`Post-Construction SRTs performed on Public Systems` <- renderReactable(reactable(xxx), striped = TRUE, pagination = FALSE)
output$`Public Systems with Post-Construction SRTs Performed` <- renderReactable(reactable(xxx), striped = TRUE, pagination = FALSE)
output$`Construction-Phase SRTs Performed on Public Systems` <- renderReactable(reactable(xxx), striped = TRUE, pagination = FALSE)
output$`Public Systems with Construction-Phase SRTs Performed` <- renderReactable(reactable(xxx), striped = TRUE, pagination = FALSE)
output$`Public Systems with CETs Administered` <- renderReactable(reactable(xxx), striped = TRUE, pagination = FALSE)
output$`Public Systems with Infiltration Testing Administered` <- renderReactable(reactable(xxx), striped = TRUE, pagination = FALSE)
output$`Public Systems with Inlet Leakage Tests Administered` <- renderReactable(reactable(xxx), striped = TRUE, pagination = FALSE)
output$`Inlet Conveyance Tests Performed on Public Systems` <- renderReactable(reactable(xxx), striped = TRUE, pagination = FALSE)
output$`Groundwater Monitoring for Public GSI` <- renderReactable(reactable(xxx), striped = TRUE, pagination = FALSE)
output$`Summary of Post-Construction CWL Monitoring of Private Systems` <- renderReactable(reactable(xxx), striped = TRUE, pagination = FALSE)
output$`Post-Construction CWL Monitoring of Private Systems Listed by Type` <- renderReactable(reactable(xxx), striped = TRUE, pagination = FALSE)
output$`Post-Construction SRTs performed on Private Systems` <- renderReactable(reactable(xxx), striped = TRUE, pagination = FALSE)
output$`Private SMPs with Post-Construction SRTs Performed` <- renderReactable(reactable(xxx), striped = TRUE, pagination = FALSE)
output$`Private Systems with CETs Administered` <- renderReactable(reactable(xxx), striped = TRUE, pagination = FALSE)
output$`Private Systems with ICTs Administered` <- renderReactable(reactable(xxx), striped = TRUE, pagination = FALSE)
output$`Private Systems with WWIs Administered` <- renderReactable(reactable(xxx), striped = TRUE, pagination = FALSE)
