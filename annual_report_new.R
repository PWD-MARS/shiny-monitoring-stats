#Quarterly Report
#Select Quarter Only 
#Table will match what is in the quarterly report

#1.0 UI --------
a_reportUI <- function(id, label = "a_report", current_fy, years){
  ns <- NS(id)
  tabPanel(title = "Annual Report", value = "a_report",
           fluidPage(#theme = shinytheme("cerulean"),
             titlePanel("Annual Report Counts"), 
             #1.1 General Inputs
             sidebarPanel(
               fluidRow(column(12, selectInput(ns("fy"), "Fiscal Year (FY)", choices = years))
               ),
               #1.2 Buttons
               fluidRow(column(6,
                               actionButton(ns("table_button"), "Generate Table")),
                        column(6, 
                               shinyjs::disabled(downloadButton(ns("download_table"), "Download xlsx")))
                        
               ), width = 3),
             #1.3 Main Panel (Outputs) -----
             mainPanel(
               
               
              strong("Table 5-1: Summary of Post-Construction CWL Monitoring of Public SMPs"),
                reactableOutput(ns("Summary of Post-Construction CWL Monitoring of Public SMPs")),
                br(),
                
              strong("Table 5-2: Post-Construction CWL Monitoring of Public SMPs Listed by Type"),
                reactableOutput(ns("Post-Construction CWL Monitoring of Public SMPs Listed by Type")),
                br(),
                
              strong("Table 5-3: Post-Construction SRTs performed on Public Systems"),
                reactableOutput(ns("Post-Construction SRTs performed on Public Systems")),
                br(),
                
              strong("Table 5-4: Public Systems with Post-Construction SRTs Performed"),
                reactableOutput(ns("Public Systems with Post-Construction SRTs Performed")),
                br(),
                
              strong("Table 5-5: Construction-Phase SRTs Performed on Public Systems"),
                reactableOutput(ns("Construction-Phase SRTs Performed on Public Systems")),
                br(),
                
              strong("Table 5-6: Public Systems with Construction-Phase SRTs Performed"),
                reactableOutput(ns("Public Systems with Construction-Phase SRTs Performed")),
                br(),
                
              strong("Table 5-7: Public Systems with CETs Administered"),
                reactableOutput(ns("Public Systems with CETs Administered")),
                br(),
                
              strong("Table 5-8: Public Systems with Infiltration Testing Administered"),
                reactableOutput(ns("Public Systems with Infiltration Testing Administered")),
                br(),
                
              strong("Table 5-9: Public Systems with Inlet Leakage Tests Administered"),
                reactableOutput(ns("Public Systems with Inlet Leakage Tests Administered")),
              br(),
                
              strong("Table 5-10: Public Systems with ICTs Administered"),
                reactableOutput(ns("Public Systems with ICTs Administered")),
              br(),
                
              strong("Table 5-11: Public Systems with Groundwater Monitoring"),
                reactableOutput(ns("Public Systems with Groundwater Monitoring")),
              br(),
                
              strong("Table 6-1: Summary of Post-Construction CWL Monitoring of Private Systems"),
                reactableOutput(ns("Summary of Post-Construction CWL Monitoring of Private Systems")),
              br(),
                
              strong("Table 6-2: Post-Construction CWL Monitoring of Private Systems Listed by Type"),
                reactableOutput(ns("Post-Construction CWL Monitoring of Private Systems Listed by Type")),
              br(),
                
              strong("Table 6-3: Post-Construction SRTs performed on Private Systems"),
                reactableOutput(ns("Post-Construction SRTs performed on Private Systems")),
              br(),
                
              strong("Table 6-4: Private SMPs with Post-Construction SRTs Performed"),
                reactableOutput(ns("Private SMPs with Post-Construction SRTs Performed")),
              br(),
                
              strong("Table 6-5: Private Systems with CETs Administered"),
                reactableOutput(ns("Private Systems with CETs Administered")),
              br(),
                
              strong("Table 6-6: Private Systems with Inlet Leakage Tests Administered"),
                reactableOutput(ns("Private Systems with Inlet Leakage Tests Administered")),
              br(),

              strong("Table 6-7: Private Systems with ICTs Administered"),
                reactableOutput(ns("Private Systems with ICTs Administered")),
              br(),
                
              strong("Table 6-8: Private Systems with WWIs Administered"),
                reactableOutput(ns("Private Systems with WWIs Administered")),
                br(),

              strong("Table 7-1: Collection System Dye Tests Administered"),
                reactableOutput(ns("Collection System Dye Tests Administered"))
             )
           )
  )
  
}

#2.0 Server -----
a_reportServer <- function(id, parent_session, current_fy, poolConn){
  moduleServer(
    id, 
    function(input, output, session){
      
      #reactive FY start and END
      FYSTART_reactive <- reactive({
        fystart_string <-"%s-07-01 00:00:00"
        FYSTART <- paste(sprintf(fystart_string, as.character(as.numeric(input$fy)-1)),collapse="")
        return(FYSTART)
      })
      
      FYEND_reactive <- reactive({
        fyend_string <-"%s-06-30 11:59:59"
        FYEND <- paste(sprintf(fyend_string, input$fy),collapse="")
        return(FYEND)
      })
      
      
      
      #2.2 observe event --------
      observeEvent(input$table_button, {
        
        #enable downloading after table in generated
        enable("download_table")
        
        #Reactive table poplutions here-all outputs must be reactive dataframes
        
        table_5_1 <- reactive({
          
          #Public sensors deployed this FY
          fy_public_sensors_deployed <- "select count(*) from fieldwork.viw_deployment_full_cwl
                            where (collection_dtime > '%s' OR collection_dtime is null)
                            and deployment_dtime between '%s' and '%s'
                            and public =  TRUE"
          
          fy_public_sensors_deployed_prod <- dbGetQuery(poolConn, paste(sprintf(fy_public_sensors_deployed, 
                                                                            FYSTART_reactive(),
                                                                            FYSTART_reactive(), 
                                                                            FYEND_reactive()),
                                                                    collapse="")) 
          
          #Public systems monitored this FY
          fy_public_systems_monitored <- "select count(distinct admin.fun_smp_to_system(d.smp_id)) 
		                                from fieldwork.viw_deployment_full_cwl d
		                                where d.public = true and
		                                (deployment_dtime between '%s' and '%s'
		                                or collection_dtime between '%s' and '%s'
		                                or (deployment_dtime < '%s' and collection_dtime is null))"
          fy_public_systems_monitored_prod <- dbGetQuery(poolConn, 
                                                         paste(sprintf(fy_public_systems_monitored, 
                                                                       FYSTART_reactive(), 
                                                                       FYEND_reactive(), 
                                                                       FYSTART_reactive(), 
                                                                       FYEND_reactive(), 
                                                                       FYSTART_reactive()), 
                                                               collapse="")) 
          
          #Public systems newly monitored this FY
          fy_public_systems_newly_monitored <- "select count(*) from 
		                                  fieldwork.viw_first_deployment_cwl f where
		                                  public = true and
		                                  first_deployment between '%s' and '%s'"
          
          fy_public_systems_newly_monitored_prod <- dbGetQuery(poolConn, 
                                                               paste(sprintf(fy_public_systems_newly_monitored, 
                                                                             FYSTART_reactive(), 
                                                                             FYEND_reactive()),
                                                                     collapse=""))
          
          #Sensors deployed to date
          todate_public_sensors_deployed <- "select count(*) from fieldwork.viw_deployment_full_cwl
	                                                  where deployment_dtime <= '%s'
	                                                  and public = TRUE"
          
          todate_public_sensors_deployed_prod <- dbGetQuery(poolConn, paste(sprintf(todate_public_sensors_deployed,
                                                                                FYEND_reactive()),
                                                                        collapse=""))
          
          #Public systems monitored to date
          todate_public_systems_monitored <- "select count(distinct admin.fun_smp_to_system(d.smp_id)) 
	                                          from fieldwork.viw_deployment_full_cwl d
	                                          where deployment_dtime <= '%s'
	                                          and d.public = true"
          
          todate_public_systems_monitored_prod <- dbGetQuery(poolConn, 
                                                             paste(sprintf(todate_public_systems_monitored,
                                                                           FYEND_reactive()),
                                                                   collapse=""))
          
          #Assembling output table
          public_postcon_cwl <- data.frame("fy" = rep(NA, 3), "todate" = rep(NA, 3))
          public_postcon_cwl$fy <- c(fy_public_sensors_deployed_prod$count, #Public sensors deployed
                                     fy_public_systems_monitored_prod$count, #Public systems monitored
                                     fy_public_systems_newly_monitored_prod$count) #Public systems newly monitored
          
          public_postcon_cwl$todate <- c(todate_public_sensors_deployed_prod$count, #Public sensors deployed
                                         todate_public_systems_monitored_prod$count, #Public systems monitored
                                         NA) #Public systems newly monitored is only defined for the FY
          
          colnames(public_postcon_cwl)<- c("This Fiscal Year","To Date")
          rownames(public_postcon_cwl)<-c("Sensors Deployed","Systems Monitored","Systems Newly Monitored")
          
          return(public_postcon_cwl)
        })

        table_5_2 <- reactive({
          #Public systems monitored by type todate
          todate_public_systems_monitored_bytype <- "select sfc.asset_type, count(distinct(d.smp_id)), d.public from
                                                            fieldwork.viw_deployment_full_cwl d
                                                            left join external.mat_assets sfc on d.smp_id = sfc.smp_id
                                                            where sfc.component_id is null
                                                            and d.smp_id is not null
                                                            and d.deployment_dtime < '%s'
                                                            and d.public = true
                                                            group by sfc.asset_type, d.public"

          #Query monitored systems and recode MARS name to GreenIT name
          todate_public_systems_monitored_bytype_prod <- dbGetQuery(poolConn, 
                                                                    paste(sprintf(todate_public_systems_monitored_bytype,
                                                                                  FYEND_reactive()),
                                                                          collapse="")) |>
            mutate(asset_type = fct_recode(asset_type, "Infiltration/Storage Trench" = "Trench"))


          #Public systems constructed by type to date
          #cipit statuses indicating constructed systems are Jillian Simmons's best recommendation
          todate_public_systems_constructed_bytype <- "select count(*), smp_smptype from external.tbl_smpbdv g 
                                                        where g.smp_notbuiltretired is null 
                                                        and (g.cipit_status = 'Closed' 
                                                        or g.cipit_status = 'Construction-Substantially Complete' 
                                                        or g.cipit_status = 'Construction-Contract Closed') 
                                                        group by smp_smptype"

          #Query constructed systems and recode GreenIT name to MARS name
          todate_public_systems_constructed_bytype_prod <- dbGetQuery(poolConn,
                                                                      todate_public_systems_constructed_bytype) |>
            mutate(smp_smptype = fct_recode(smp_smptype, "Permeable Pavement" = "Pervious Paving")) 
          

          #Join and assemble table
          todate_public_prod <- todate_public_systems_constructed_bytype_prod |> 
            left_join(todate_public_systems_monitored_bytype_prod, 
                      by=c("smp_smptype" = "asset_type"), 
                      suffix = c(".constructed", ".monitored")) |>
            transmute(`SMP Type` = smp_smptype, 
                      `Monitored SMPs` = replace_na(count.monitored, 0),
                      `Total Constructed Public SMPs` = count.constructed,
                      Note = NA)

          #Add Notes
          todate_public_prod$Note[todate_public_prod$`SMP Type` == "Infiltration/Storage Trench"] <- "Also listed as Trench"
          todate_public_prod$Note[todate_public_prod$`SMP Type` == "Permeable Pavement"] <- "Also listed as Pervious Paving"

          return(todate_public_prod)
        })

        table_5_3 <- reactive({
          #Post-construction public SRTs this FY
          fy_public_postcon_srt <- "select count(*), type from fieldwork.viw_srt_full 
                                    where test_date >= '%s'
                                    and test_date <= '%s'
                                    and phase = 'Post-Construction'
                                    and public = TRUE
                                    group by type"

          fy_public_postcon_srt_prod <-dbGetQuery(poolConn, 
                                                  paste(sprintf(fy_public_postcon_srt, 
                                                                FYSTART_reactive(), 
                                                                FYEND_reactive()),
                                                        collapse=""))

          #Post-construction public SRTsto date
          todate_public_postcon_srt <-"select count(*), type from fieldwork.viw_srt_full 
                                                                    where test_date <= '%s'
                                                                    and phase = 'Post-Construction'
                                                                    and public = TRUE
                                                                    group by type"

          todate_public_postcon_srt_prod <- dbGetQuery(poolConn, 
                                                       paste(sprintf(todate_public_postcon_srt,
                                                                     FYEND_reactive()),
                                                             collapse=""))
          
          #Assembling output table
          public_postcon_srt <- left_join(todate_public_postcon_srt_prod,
                                          fy_public_postcon_srt_prod,
                                          by = "type",
                                          suffix = c(".todate", ".fy"))

          rownames(public_postcon_srt)<- public_postcon_srt$type
          public_postcon_srt <- transmute(public_postcon_srt,
                                       "This Fiscal Year" = replace_na(count.fy, 0),
                                       "To Date" = count.todate)

          return(public_postcon_srt)
        })

        table_5_4 <- reactive({
          #Public systems with post-construction srts this FY
          fy_public_postcon_srt_systems <-"select sfc.asset_type, count(distinct(srt.system_id))
                                              from fieldwork.viw_srt_full srt
                                              left join external.mat_assets sfc on srt.system_id = sfc.system_id
                                              where sfc.component_id is null
                                              and test_date >= '%s'
                                              and test_date <= '%s'
                                              and phase = 'Post-Construction'
                                              and public = TRUE
                                              group by sfc.asset_type"

          fy_public_postcon_srt_systems_prod <-dbGetQuery(poolConn, 
                                                          paste(sprintf(fy_public_postcon_srt_systems, 
                                                                        FYSTART_reactive(), 
                                                                        FYEND_reactive()),
                                                                collapse=""))

          #Public Systems with Post-Construction SRTs Performed TO DATE
          todate_public_postcon_srt_systems <-"select sfc.asset_type, count(distinct(srt.system_id))
                                                            from fieldwork.viw_srt_full srt
                                                            left join external.mat_assets sfc on srt.system_id = sfc.system_id
                                                            where sfc.component_id is null
                                                            and test_date <= '%s'
                                                            and phase = 'Post-Construction'
                                                            and public = TRUE
                                                            group by sfc.asset_type"

          todate_public_postcon_srt_systems_prod <-dbGetQuery(poolConn, 
                                                              paste(sprintf(todate_public_postcon_srt_systems, 
                                                                            FYEND_reactive()),
                                                                    collapse=""))

          #Assembling output table
          public_postcon_srt_bysystem <- left_join(todate_public_postcon_srt_systems_prod,
                                          fy_public_postcon_srt_systems_prod,
                                          by = "asset_type",
                                          suffix = c(".todate", ".fy"))

          rownames(public_postcon_srt_bysystem)<- public_postcon_srt_bysystem$asset_type
          public_postcon_srt_bysystem <- transmute(public_postcon_srt_bysystem,
                                       "This Fiscal Year" = replace_na(count.fy, 0),
                                       "To Date" = count.todate)

          return(public_postcon_srt_bysystem)
        })

        table_5_5 <- reactive({

          #Mid-construction SRTs performed on Public Systems this FY
          fy_public_midcon_srt <- "select count(*), type
                                      from fieldwork.viw_srt_full 
                                      where test_date >= '%s'
                                      and test_date <= '%s'
                                      and phase = 'Construction'
                                      and public = TRUE
                                      group by type"
          fy_public_midcon_srt_prod <- dbGetQuery(poolConn, 
                                                  paste(sprintf(fy_public_midcon_srt, 
                                                                FYSTART_reactive(), 
                                                                FYEND_reactive()),
                                                        collapse=""))



          #Mid-construction SRTs performed on Public Systems to date
          todate_public_midcon_srt <- "select count(*), type
                                    from fieldwork.viw_srt_full 
                                    where test_date <= '%s'
                                    and phase = 'Construction'
                                    and public = TRUE
                                    group by type"

          todate_public_midcon_srt_prod <- dbGetQuery(poolConn, 
                                                      paste(sprintf(todate_public_midcon_srt, 
                                                                    FYEND_reactive()),
                                                            collapse=""))

          #Assembling output table
          public_midcon_srt <- left_join(todate_public_midcon_srt_prod,
                                          fy_public_midcon_srt_prod,
                                          by = "type",
                                          suffix = c(".todate", ".fy"))

          rownames(public_midcon_srt)<- public_midcon_srt$type
          public_midcon_srt <- transmute(public_midcon_srt,
                                       "This Fiscal Year" = replace_na(count.fy, 0),
                                       "To Date" = count.todate)

          return(public_midcon_srt)
        })

        table_5_6 <- reactive({

          #Public systems recieving mid-con SRTs this FY
          fy_public_midcon_srt_systems <-"select sfc.asset_type, count(distinct(srt.system_id))
                                        from fieldwork.viw_srt_full srt
                                        left join external.mat_assets sfc on srt.system_id = sfc.system_id
                                        where sfc.component_id is null
                                        and test_date >= '%s'
                                        and test_date <= '%s'
                                        and phase = 'Construction'
                                        and public = TRUE
                                        group by sfc.asset_type"

          fy_public_midcon_srt_systems_prod <- dbGetQuery(poolConn, 
                                                 paste(sprintf(fy_public_midcon_srt_systems, 
                                                               FYSTART_reactive(), 
                                                               FYEND_reactive()),
                                                       collapse=""))

          #Public systems recieving mid-con SRTs to date
          todate_public_midcon_srt_systems <-"select sfc.asset_type, count(distinct(srt.system_id))
                                        from fieldwork.viw_srt_full srt
                                        left join external.mat_assets sfc on srt.system_id = sfc.system_id
                                        where sfc.component_id is null
                                        and test_date <= '%s'
                                        and phase = 'Construction'
                                        and public = TRUE
                                        group by sfc.asset_type"
          todate_public_midcon_srt_systems_prod <- dbGetQuery(poolConn, 
                                                          paste(sprintf(todate_public_midcon_srt_systems,
                                                                        FYEND_reactive()),
                                                                collapse=""))

          #Assembling output table
          public_midcon_srt_bysystem <- left_join(todate_public_midcon_srt_systems_prod,
                                          fy_public_midcon_srt_systems_prod,
                                          by = "asset_type",
                                          suffix = c(".todate", ".fy"))

          rownames(public_midcon_srt_bysystem)<- public_midcon_srt_bysystem$asset_type
          public_midcon_srt_bysystem <- transmute(public_midcon_srt_bysystem,
                                       "This Fiscal Year" = replace_na(count.fy, 0),
                                       "To Date" = count.todate)

          return(public_midcon_srt_bysystem)
        })

        table_5_7 <- reactive({

          #Public CETs this FY
          fy_public_cet <-"select count(distinct system_id) 
                                                from fieldwork.viw_capture_efficiency_full 
                                                where phase = 'Post-Construction'
                                                and test_date >= '%s'
                                                and test_date <= '%s'
                                                and public = TRUE"

          fy_public_cet_prod <- dbGetQuery(poolConn, 
                                           paste(sprintf(fy_public_cet, 
                                                         FYSTART_reactive(), 
                                                         FYEND_reactive()),
                                                 collapse=""))

          #Public CETs to date
          todate_public_cet <-"select count(distinct system_id) 
                                                from fieldwork.viw_capture_efficiency_full 
                                                where phase = 'Post-Construction'
                                                and test_date <= '%s'
                                                and public = TRUE"
          todate_public_cet_prod <- dbGetQuery(poolConn, 
                                               paste(sprintf(todate_public_cet,
                                                             FYEND_reactive()),
                                                     collapse=""))

          #Assembling output table
          public_cet <- data.frame(fy = fy_public_cet_prod$count, 
                                   todate = todate_public_cet_prod$count)

          rownames(public_cet) <- "Systems With CETs Administered"
          public_cet <- transmute(public_cet,
                                   "This Fiscal Year" = replace_na(fy, 0),
                                   "To Date" = todate)

          return(public_cet)
        })

        table_5_8 <- reactive({
          #Public Systems with PP/PPSIRT in this fy
          fy_public_pp <-"select count(distinct admin.fun_smp_to_system(smp_id))
                                                from fieldwork.viw_porous_pavement_full
                                                where test_date >= '%s'
                                                and test_date <= '%s'
                                                and public = TRUE"
          fy_public_pp_prod <- dbGetQuery(poolConn, 
                                          paste(sprintf(fy_public_pp, 
                                                        FYSTART_reactive(), 
                                                        FYEND_reactive()),
                                                collapse=""))

          #Public Systems with PP/PPSIRT to date
          todate_public_pp <-"select count(distinct admin.fun_smp_to_system(smp_id))
                                                    from fieldwork.viw_porous_pavement_full
                                                    where test_date <= '%s'
                                                    and public = TRUE"
          todate_public_pp_prod <- dbGetQuery(poolConn, 
                                              paste(sprintf(todate_public_pp, 
                                                            FYEND_reactive()),
                                                    collapse=""))

          #Assembling output table
          public_pp <- data.frame(fy = fy_public_pp_prod$count, 
                                   todate = todate_public_pp_prod$count)

          rownames(public_pp) <- "Systems With PP/SIRTs Administered"
          public_pp <- transmute(public_pp,
                                   "This Fiscal Year" = replace_na(fy, 0),
                                   "To Date" = todate)

          return(public_pp)
        })

        table_5_9 <- reactive({
          #Public systems with post-construction leakage tests this fy
          fy_public_systems_leakage <- "select count(*) from 
              (select distinct system_id from fieldwork.viw_special_investigation_full 
                 where system_id is not null and 
                 special_investigation_type = 'Leakage Test' and 
                 system_id similar to '\\d+-\\d+' and 
                 phase = 'Post-Construction' and
                 test_date >= '%s' and 
                 test_date <= '%s') leakage_tests"


          fy_public_systems_leakage_prod <- dbGetQuery(poolConn, 
                                                       paste(sprintf(fy_public_systems_leakage,
                                                                     FYSTART_reactive(), 
                                                                     FYEND_reactive()),
                                                             collapse=""))

          #public systems with post-con leakage tests to date
          todate_public_systems_leakage <- "select count(*) from 
              (select distinct system_id from fieldwork.viw_special_investigation_full 
                 where system_id is not null and 
                 special_investigation_type = 'Leakage Test' and 
                 system_id similar to '\\d+-\\d+' and 
                 phase = 'Post-Construction' and
                 test_date <= '%s') leakage_tests"


          todate_public_systems_leakage_prod <- dbGetQuery(poolConn, 
                                                           paste(sprintf(todate_public_systems_leakage,
                                                                         FYEND_reactive()),
                                                                 collapse=""))


          #Assembling output table
          public_systems_leakage <- data.frame(fy = fy_public_systems_leakage_prod$count, 
                                   todate = todate_public_systems_leakage_prod$count)

          rownames(public_systems_leakage) <- "Systems With Leakage Tests Administered"
          public_systems_leakage <- transmute(public_systems_leakage,
                                   "This Fiscal Year" = replace_na(fy, 0),
                                   "To Date" = todate)

          return(public_systems_leakage)
        })

        table_5_10 <- reactive({
          #Public Systems with ICTs this fy
          fy_public_systems_ict <- "select count(*) from 
              (select distinct system_id from fieldwork.viw_inlet_conveyance_full
                where system_id is not null and
                system_id similar to '\\d+-\\d+' and
                phase = 'Post-Construction' and
                test_date >= '%s' and 
                test_date <= '%s') ict"

          fy_public_systems_ict_prod <- dbGetQuery(poolConn,
                                                   paste(sprintf(fy_public_systems_ict,
                                                                 FYSTART_reactive(),
                                                                 FYEND_reactive()),
                                                         collapse = ""))

          #Public Systems with ICTs to date
          todate_public_systems_ict <- "select count(*) from 
              (select distinct system_id from fieldwork.viw_inlet_conveyance_full
                where system_id is not null and
                system_id similar to '\\d+-\\d+' and
                phase = 'Post-Construction' and
                test_date <= '%s') ict"

          todate_public_systems_ict_prod <- dbGetQuery(poolConn,
                                                       paste(sprintf(todate_public_systems_ict,
                                                                     FYEND_reactive()),
                                                             collapse = ""))

          #Assembling output table
          public_systems_ict <- data.frame(fy = fy_public_systems_ict_prod$count, 
                                   todate = todate_public_systems_ict_prod$count)

          rownames(public_systems_ict) <- "Systems With ICTs Administered"
          public_systems_ict <- transmute(public_systems_ict,
                                   "This Fiscal Year" = replace_na(fy, 0),
                                   "To Date" = todate)

          return(public_systems_ict)
        })

        table_5_11 <- reactive({
          #Public systems with preconstruction GW monitoring this FY
          fy_public_precon_gw <-"select count(distinct(site_name)) from fieldwork.viw_deployment_full where smp_id is null 
                                                          and deployment_dtime <= '%s'
                                                          and (collection_dtime >= '%s' OR collection_dtime is null)
                                                          and (ow_suffix LIKE 'GW_' or ow_suffix LIKE 'CW_')"

          fy_public_precon_gw_prod <- dbGetQuery(poolConn, paste(sprintf(fy_public_precon_gw, 
                                                              FYEND_reactive(), 
                                                              FYSTART_reactive()),
                                                      collapse=""))

          #Public systems with postconstruction GW monitoring this FY
          fy_public_postcon_gw <-"select count(distinct(smp_id)) from fieldwork.viw_deployment_full where smp_id is not null 
                                                          and deployment_dtime <= '%s'
                                                          and (collection_dtime >= '%s' OR collection_dtime is null)
                                                          and (ow_suffix LIKE 'GW_' or ow_suffix LIKE 'CW_')"
          fy_public_postcon_gw_prod <- dbGetQuery(poolConn, paste(sprintf(fy_public_postcon_gw, 
                                                               FYEND_reactive(), 
                                                               FYSTART_reactive()),
                                                       collapse=""))

          #Public systems with preconstruction GW monitoring to date
          todate_public_precon_gw <-"select count(distinct(site_name)) from fieldwork.viw_deployment_full where smp_id is null 
                                                          and (ow_suffix LIKE 'GW_' or ow_suffix LIKE 'CW_')"
          todate_public_precon_gw_prod <- dbGetQuery(poolConn, todate_public_precon_gw)

          #Public systems with postconstruction GW monitoring to date
          todate_public_postcon_gw <-"select count(distinct(smp_id)) from fieldwork.viw_deployment_full where smp_id is not null 
                                                                                    and (ow_suffix LIKE 'GW_' or ow_suffix LIKE 'CW_')"
          todate_public_postcon_gw_prod <- dbGetQuery(poolConn, todate_public_postcon_gw)


          #Assembling output table
          public_gw <- data.frame("fy" = rep(NA, 2), "todate" = rep(NA, 2))
          public_gw$fy <- c(fy_public_precon_gw_prod$count, #Public precon GW systems this FY
                                     fy_public_postcon_gw_prod$count) #Public postcon GW systems this FY
          
          public_gw$todate <- c(todate_public_precon_gw_prod$count, #Public postcon GW systems to date
                                         todate_public_postcon_gw_prod$count) #Public postcon GW systems to date
          
          colnames(public_gw)<- c("This Fiscal Year","To Date")
          rownames(public_gw)<-c("Systems with Pre-Construction GW Monitoring", "Systems with Post-Construction GW Monitoring")

          return(public_gw)
        })

        table_6_1 <- reactive({

          #Private sensors deployed this FY
          fy_private_sensors_deployed <-  "select count(*) from fieldwork.viw_deployment_full_cwl
                                            where (collection_dtime > '%s' OR collection_dtime is null)
                                            and deployment_dtime between '%s' and '%s'
                                            and public = FALSE"

          fy_private_sensors_deployed_prod <- dbGetQuery(poolConn, 
                                                         paste(sprintf(fy_private_sensors_deployed, 
                                                                       FYSTART_reactive(), 
                                                                       FYSTART_reactive(), 
                                                                       FYEND_reactive()),
                                                               collapse="")) 

          #Private systems monitored this FY
          fy_private_systems_monitored <- "select count(distinct admin.fun_smp_to_system(d.smp_id)) from fieldwork.viw_deployment_full_cwl d
                                            where deployment_dtime between '%s' and '%s'
                                            and (collection_dtime >= '%s'
                                                or collection_dtime is null) 
                                            and d.public = false"
          fy_private_systems_monitored_prod <- dbGetQuery(poolConn, paste(sprintf(fy_private_systems_monitored, 
                                                                    FYSTART_reactive(), 
                                                                    FYEND_reactive(), 
                                                                    FYSTART_reactive()),
                                                            collapse="")) 

          #Newly monitored systems this fiscal year (private)
          fy_private_systems_newly_monitored <-"select count(distinct admin.fun_smp_to_system(newdeployments.smp_id)) FROM 
                                                      (select d.smp_id FROM fieldwork.viw_deployment_full_cwl d 
                                                         group BY d.smp_id, d.public
                                                         having min(d.deployment_dtime) > '%s'
                                                         and min(d.deployment_dtime) <= '%s'
                                                         and d.public = false) newdeployments"
          fy_private_systems_newly_monitored_prod <- dbGetQuery(poolConn, 
                                                                paste(sprintf(fy_private_systems_newly_monitored,
                                                                              FYSTART_reactive(), 
                                                                              FYEND_reactive()),
                                                                      collapse=""))

          #Private sensor deployments to date
          todate_private_sensors_deployed <- "select count(*) from fieldwork.viw_deployment_full_cwl
                                            where deployment_dtime < '%s'
                                            and public = FALSE"

          todate_private_sensors_deployed_prod <- dbGetQuery(poolConn, 
                                                        paste(sprintf(todate_private_sensors_deployed,
                                                                      FYEND_reactive()),
                                                              collapse="")) 

          #Private systems monitored to date
          todate_private_systems_monitored <- "select count(distinct admin.fun_smp_to_system(d.smp_id)) from fieldwork.viw_deployment_full_cwl d
                                            where deployment_dtime <= '%s'
                                            and d.public = false"
          todate_private_systems_monitored_prod <- dbGetQuery(poolConn, 
                                                    paste(sprintf(todate_private_systems_monitored, 
                                                                  FYEND_reactive()),
                                                          collapse="")) 

          #Assembling output table
          private_postcon_cwl <- data.frame("fy" = rep(NA, 3), "todate" = rep(NA, 3))
          private_postcon_cwl$fy <- c(fy_private_sensors_deployed_prod$count, #private sensors deployed
                                     fy_private_systems_monitored_prod$count, #private systems monitored
                                     fy_private_systems_newly_monitored_prod$count) #private systems newly monitored
          
          private_postcon_cwl$todate <- c(todate_private_sensors_deployed_prod$count, #private sensors deployed
                                         todate_private_systems_monitored_prod$count, #private systems monitored
                                         NA) #private systems newly monitored is only defined for the FY
          
          colnames(private_postcon_cwl)<- c("This Fiscal Year","To Date")
          rownames(private_postcon_cwl)<-c("Sensors Deployed","Systems Monitored","Systems Newly Monitored")
          
          return(private_postcon_cwl)
        })

        table_6_2 <- reactive({
          #Post-Construction Monitored private SMPs by type to date
          todate_private_systems_monitored_bytype <- "select cr.\"smp_type\" as smp_type, count(distinct(d.smp_id)), d.public from
                            fieldwork.viw_deployment_full_cwl d
                            left join external.tbl_planreview_crosstab cr on d.smp_id = cr.\"smp_id\"::text
                            where d.smp_id is not null
                            and d.deployment_dtime < '%s'
                            and d.public = false
                            group by cr.\"smp_type\", d.public;"

          todate_private_systems_monitored_bytype_prod <- dbGetQuery(poolConn, 
                                                           paste(sprintf(todate_private_systems_monitored_bytype,
                                                                         FYEND_reactive()),
                                                                 collapse=""))

          #Total constructed private SMPs to date
          todate_constructed_private_systems_bytype <- "with sfc as (
                                                    select distinct smp_id from external.mat_assets 
                                                    where smp_id is not null
                                                    and component_id is null
                                                  ), pl as (
                                                    select distinct \"SMPID\" from external.tbl_planreview_private
                                                  ), cr as (
                                                    select distinct smp_id, dcia_ft2, smp_type from external.tbl_planreview_crosstab
                                                  )
                                                  
                                                  select count(*), cr.smp_type from pl 
                                                  left join cr on pl.\"SMPID\"::text = cr.smp_id
                                                  inner join sfc on pl.\"SMPID\"::text = sfc.smp_id
                                                  where cr.dcia_ft2 is not null
                                                  group by cr.smp_type"

          todate_constructed_private_systems_bytype_prod<- dbGetQuery(poolConn, todate_constructed_private_systems_bytype)

          #Assembling output table
          todate_private_prod <- todate_constructed_private_systems_bytype_prod |>
            left_join(todate_private_systems_monitored_bytype_prod, 
                      by = "smp_type",
                      suffix = c(".constructed", ".monitored")) |>
            transmute(`SMP Type` = smp_type, 
                      `Monitored SMPs` = replace_na(count.monitored, 0),
                      `Total Constructed Private SMPs` = count.constructed)

            return(todate_private_prod)
        })

        table_6_3 <- reactive({
          #Post-construction private SRTs this FY
          fy_private_postcon_srt <-"select count(*), type
                                    from fieldwork.viw_srt_full 
                                    where test_date >= '%s'
                                    and test_date <= '%s'
                                    and phase = 'Post-Construction'
                                    and public = false
                                    group by type"


          fy_private_postcon_srt_prod <- dbGetQuery(poolConn, 
                                                    paste(sprintf(fy_private_postcon_srt, 
                                                                  FYSTART_reactive(), 
                                                                  FYEND_reactive()),
                                                          collapse=""))

          #Post-construction private SRTs to date
          todate_private_postcon_srt <-"select count(*), type
                                  from fieldwork.viw_srt_full 
                                  where test_date <= '%s'
                                  and phase = 'Post-Construction'
                                  and public = false
                                  group by type"


          todate_private_postcon_srt_prod <- dbGetQuery(poolConn, 
                                                        paste(sprintf(todate_private_postcon_srt,
                                                                      FYEND_reactive()),
                                                              collapse=""))

          #Assembling output table
          private_postcon_srt <- left_join(todate_private_postcon_srt_prod,
                                          fy_private_postcon_srt_prod,
                                          by = "type",
                                          suffix = c(".todate", ".fy"))

          rownames(private_postcon_srt)<- private_postcon_srt$type
          private_postcon_srt <- transmute(private_postcon_srt,
                                       "This Fiscal Year" = replace_na(count.fy, 0),
                                       "To Date" = count.todate)

          return(private_postcon_srt)
        })

        table_6_4 <- reactive({
          #Private systems with post-con SRTs this FY
          fy_private_postcon_srt_systems <- "select cr.\"smp_type\" as smp_type, count(distinct newtests.system_id) FROM 
            (select system_id from fieldwork.viw_srt_full srt
              group by system_id, public
              having min(test_date) >= '%s'
              and min(test_date) <= '%s'
              and public = false) newtests
              left join external.tbl_planreview_crosstab cr on newtests.system_id = cr.\"smp_id\"::text
              group by cr.\"smp_type\""


          fy_private_postcon_srt_systems_prod <- dbGetQuery(poolConn, 
                                                         paste(sprintf(fy_private_postcon_srt_systems, 
                                                                       FYSTART_reactive(), 
                                                                       FYEND_reactive()),
                                                               collapse=""))

          #Private systems with post-con SRTs to date
          todate_private_postcon_srt_systems <-"select cr.\"smp_type\" as smp_type, count(distinct newtests.system_id) FROM 
            (select system_id from fieldwork.viw_srt_full srt
              group by system_id, public
              having min(test_date) <= '%s'
              and public = false) newtests
              left join external.tbl_planreview_crosstab cr on newtests.system_id = cr.\"smp_id\"::text
              group by cr.\"smp_type\""

          todate_private_postcon_srt_systems_prod <- dbGetQuery(poolConn, 
                                                             paste(sprintf(todate_private_postcon_srt_systems,
                                                                           FYEND_reactive()),
                                                                   collapse=""))

          #Assembling output table
          private_postcon_srt_bysystem <- left_join(todate_private_postcon_srt_systems_prod,
                                          fy_private_postcon_srt_systems_prod,
                                          by = "smp_type",
                                          suffix = c(".todate", ".fy"))

          rownames(private_postcon_srt_bysystem)<- private_postcon_srt_bysystem$smp_type
          private_postcon_srt_bysystem <- transmute(private_postcon_srt_bysystem,
                                       "This Fiscal Year" = replace_na(count.fy, 0),
                                       "To Date" = count.todate)

          return(private_postcon_srt_bysystem)
        })

        table_6_5 <- reactive({

          #Public CETs this FY
          fy_private_cet <-"select count(distinct system_id) 
                                                from fieldwork.viw_capture_efficiency_full 
                                                where phase = 'Post-Construction'
                                                and test_date >= '%s'
                                                and test_date <= '%s'
                                                and public = FALSE"

          fy_private_cet_prod <- dbGetQuery(poolConn, 
                                           paste(sprintf(fy_private_cet, 
                                                         FYSTART_reactive(), 
                                                         FYEND_reactive()),
                                                 collapse=""))

          #Public CETs to date
          todate_private_cet <-"select count(distinct system_id) 
                                                from fieldwork.viw_capture_efficiency_full 
                                                where phase = 'Post-Construction'
                                                and test_date <= '%s'
                                                and public = FALSE"
          todate_private_cet_prod <- dbGetQuery(poolConn, 
                                               paste(sprintf(todate_private_cet,
                                                             FYEND_reactive()),
                                                     collapse=""))

          #Assembling output table
          private_cet <- data.frame(fy = fy_private_cet_prod$count, 
                                   todate = todate_private_cet_prod$count)

          rownames(private_cet) <- "Systems With CETs Administered"
          private_cet <- transmute(private_cet,
                                   "This Fiscal Year" = replace_na(fy, 0),
                                   "To Date" = todate)

          return(private_cet)
        })

        table_6_6 <- reactive({
          #private systems with post-con leakage tests this fy
          fy_private_systems_leakage <- "select count(*) from 
              (select distinct system_id from fieldwork.viw_special_investigation_full 
                 where system_id is not null and 
                 special_investigation_type = 'Leakage Test' and 
                 system_id not similar to '\\d+-\\d+' and 
                 phase = 'Post-Construction' and
                 test_date >= '%s' and 
                 test_date <= '%s') leakage_tests"


          fy_private_systems_leakage_prod <- dbGetQuery(poolConn, 
                                                        paste(sprintf(fy_private_systems_leakage,
                                                                      FYSTART_reactive(),
                                                                      FYEND_reactive()),
                                                              collapse=""))

          #private systems with post-con leakage tests to date
          todate_private_systems_leakage <- "select count(*) from 
              (select distinct system_id from fieldwork.viw_special_investigation_full 
                 where system_id is not null and 
                 special_investigation_type = 'Leakage Test' and 
                 system_id not similar to '\\d+-\\d+' and 
                 phase = 'Post-Construction' and
                 test_date <= '%s') leakage_tests"


          todate_private_systems_leakage_prod <- dbGetQuery(poolConn, 
                                                            paste(sprintf(todate_private_systems_leakage,
                                                                          FYEND_reactive()),
                                                                  collapse=""))

          #Assembling output table
          private_leakage <- data.frame(fy = fy_private_systems_leakage_prod$count, 
                                   todate = todate_private_systems_leakage_prod$count)

          rownames(private_leakage) <- "Systems With Leakage Tests Administered"
          private_leakage <- transmute(private_leakage,
                                   "This Fiscal Year" = replace_na(fy, 0),
                                   "To Date" = todate)

          return(private_leakage)
        })

        table_6_7 <- reactive({
          #Private Systems with ICTs this fy
          fy_private_systems_ict <- "select count(*) from 
              (select distinct system_id from fieldwork.viw_inlet_conveyance_full
                where system_id is not null and
                system_id not similar to '\\d+-\\d+' and
                phase = 'Post-Construction' and
                test_date >= '%s' and 
                test_date <= '%s') ict"

          fy_private_systems_ict_prod <- dbGetQuery(poolConn,
                                                   paste(sprintf(fy_private_systems_ict,
                                                                 FYSTART_reactive(),
                                                                 FYEND_reactive()),
                                                         collapse = ""))

          #Private Systems with ICTs to date
          todate_private_systems_ict <- "select count(*) from 
              (select distinct system_id from fieldwork.viw_inlet_conveyance_full
                where system_id is not null and
                system_id not similar to '\\d+-\\d+' and
                phase = 'Post-Construction' and
                test_date <= '%s') ict"

          todate_private_systems_ict_prod <- dbGetQuery(poolConn,
                                                       paste(sprintf(todate_private_systems_ict,
                                                                     FYEND_reactive()),
                                                             collapse = ""))

          #Assembling output table
          private_systems_ict <- data.frame(fy = fy_private_systems_ict_prod$count, 
                                   todate = todate_private_systems_ict_prod$count)

          rownames(private_systems_ict) <- "Systems With ICTs Administered"
          private_systems_ict <- transmute(private_systems_ict,
                                   "This Fiscal Year" = replace_na(fy, 0),
                                   "To Date" = todate)

          return(private_systems_ict)
        })

        table_6_8 <- reactive({
          #Private systems with wet weather inspections this FY
          fy_private_systems_wwi <- "select count(*) from 
              (select distinct system_id from fieldwork.viw_special_investigation_full 
                 where system_id is not null and 
                 special_investigation_type = 'Wet Weather Inspection' and 
                 system_id not similar to '\\d+-\\d+' and 
                 phase = 'Post-Construction' and
                 test_date >= '%s' and 
                 test_date <= '%s') wwi"

          fy_private_systems_wwi_prod <- dbGetQuery(poolConn,
                                                    paste(sprintf(fy_private_systems_wwi,
                                                                  FYSTART_reactive(),
                                                                  FYEND_reactive()),
                                                          collapse = ""))

          #Private systems with wet weather inspections to date
          todate_private_systems_wwi <- "select count(*) from 
              (select distinct system_id from fieldwork.viw_special_investigation_full 
                 where system_id is not null and 
                 special_investigation_type = 'Wet Weather Inspection' and 
                 system_id not similar to '\\d+-\\d+' and 
                 phase = 'Post-Construction' and
                 test_date <= '%s') wwi"

          todate_private_systems_wwi_prod <- dbGetQuery(poolConn,
                                                        paste(sprintf(todate_private_systems_wwi,
                                                                      FYEND_reactive()),
                                                              collapse = ""))

          #Assembling output table
          private_systems_wwi <- data.frame(fy = fy_private_systems_wwi_prod$count, 
                                   todate = todate_private_systems_wwi_prod$count)

          rownames(private_systems_wwi) <- "Systems With Wet-Weather Inspections Administered"
          private_systems_wwi <- transmute(private_systems_wwi,
                                   "This Fiscal Year" = replace_na(fy, 0),
                                   "To Date" = todate)

          return(private_systems_wwi)
        })

        table_7_1 <- reactive({
          #Collection system dye tests this FY
          fy_collection_dye <- "select count(*) from fieldwork.viw_special_investigation_full 
                 where special_investigation_type = 'Private Plumbing' and 
                 test_date >= '%s' and 
                 test_date <= '%s'"

          fy_collection_dye_prod <- dbGetQuery(poolConn,
                                               paste(sprintf(fy_collection_dye,
                                                             FYSTART_reactive(),
                                                             FYEND_reactive()),
                                                     collapse = ""))

          #Collection system dye tests to date
          todate_collection_dye <- "select count(*) from fieldwork.viw_special_investigation_full where
                 special_investigation_type = 'Private Plumbing' and 
                 test_date <= '%s'"

          todate_collection_dye_prod <- dbGetQuery(poolConn,
                                                   paste(sprintf(todate_collection_dye,
                                                                 FYEND_reactive()),
                                                         collapse = ""))

          #Assembling output table
          collection_dye <- data.frame(fy = fy_collection_dye_prod$count, 
                                   todate = todate_collection_dye_prod$count)

          rownames(collection_dye) <- "Collection System Dye Tests Administered"
          collection_dye <- transmute(collection_dye,
                                   "This Fiscal Year" = replace_na(fy, 0),
                                   "To Date" = todate)

          return(collection_dye)
        })
        
        #reactable table outputs
        output$`Summary of Post-Construction CWL Monitoring of Public SMPs` <- renderReactable(reactable(table_5_1(), striped = TRUE, pagination = FALSE))
        output$`Post-Construction CWL Monitoring of Public SMPs Listed by Type` <- renderReactable(reactable(table_5_2(), striped = TRUE, pagination = FALSE))
        output$`Post-Construction SRTs performed on Public Systems` <- renderReactable(reactable(table_5_3(), striped = TRUE, pagination = FALSE))
        output$`Public Systems with Post-Construction SRTs Performed` <- renderReactable(reactable(table_5_4(), striped = TRUE, pagination = FALSE))
        output$`Construction-Phase SRTs Performed on Public Systems` <- renderReactable(reactable(table_5_5(), striped = TRUE, pagination = FALSE))
        output$`Public Systems with Construction-Phase SRTs Performed` <- renderReactable(reactable(table_5_6(), striped = TRUE, pagination = FALSE))
        output$`Public Systems with CETs Administered` <- renderReactable(reactable(table_5_7(), striped = TRUE, pagination = FALSE))
        output$`Public Systems with Infiltration Testing Administered` <- renderReactable(reactable(table_5_8(), striped = TRUE, pagination = FALSE))
        output$`Public Systems with Inlet Leakage Tests Administered` <- renderReactable(reactable(table_5_9(), striped = TRUE, pagination = FALSE))
        output$`Public Systems with ICTs Administered` <- renderReactable(reactable(table_5_10(), striped = TRUE, pagination = FALSE))
        output$`Public Systems with Groundwater Monitoring` <- renderReactable(reactable(table_5_11(), striped = TRUE, pagination = FALSE))
        output$`Summary of Post-Construction CWL Monitoring of Private Systems` <- renderReactable(reactable(table_6_1(), striped = TRUE, pagination = FALSE))
        output$`Post-Construction CWL Monitoring of Private Systems Listed by Type` <- renderReactable(reactable(table_6_2(), striped = TRUE, pagination = FALSE))
        output$`Post-Construction SRTs performed on Private Systems` <- renderReactable(reactable(table_6_3(), striped = TRUE, pagination = FALSE))
        output$`Private SMPs with Post-Construction SRTs Performed` <- renderReactable(reactable(table_6_4(), striped = TRUE, pagination = FALSE))
        output$`Private Systems with CETs Administered` <- renderReactable(reactable(table_6_5(), striped = TRUE, pagination = FALSE))
        output$`Private Systems with Inlet Leakage Tests Administered` <- renderReactable(reactable(table_6_6(), striped = TRUE, pagination = FALSE))
        output$`Private Systems with ICTs Administered` <- renderReactable(reactable(table_6_7(), striped = TRUE, pagination = FALSE))
        output$`Private Systems with WWIs Administered` <- renderReactable(reactable(table_6_8(), striped = TRUE, pagination = FALSE))
        output$`Collection System Dye Tests Administered` <- renderReactable(reactable(table_7_1(), striped = TRUE, pagination = FALSE))


        output$help_text <- renderText({
          paste("A Shiny App to Populate the Annual Report Stats" , 
                "First Version Published on 08/05/2022 by Farshad Ebrahimi",
                sep="\n")
        })
        
        output$download_table <- downloadHandler(
          
          filename = function() {
            paste("FY",input$fy,"_","AnnualReport","_",Sys.Date(),".xlsx", sep = "")
          },
          content = function(filename){
            
            df_list <- list(Table_3_1=table_5_1())
            write.xlsx(x = df_list , file = filename, rowNames = TRUE)
          }
        ) 
        
        
      })
      
      
    }
  )
}






