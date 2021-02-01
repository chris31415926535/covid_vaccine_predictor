#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(lubridate)
library(plotly)
library(leaflet)
library(httr)
library(stringr)
library(dplyr)
library(readr)
library(tidyr)
library(tibble)

update <- FALSE

# if we have never updated before, we need to update now
if (!file.exists("data/last_update.csv")) update <- TRUE

# if we have updated before, we need to update if it's been more than 4 hours
if (file.exists("data/last_update.csv")){
    last_update <- read_csv("data/last_update.csv") %>%
        pull(1)
    
    if (difftime(Sys.time(), last_update, units = "hour") > 4) update <- TRUE
}

# update the data
if (update) {
    message("Updating data...")
    
    provinces <- httr::GET("https://api.covid19tracker.ca/provinces") %>%
        httr::content("text") %>%
        jsonlite::fromJSON() %>%
        filter(id < 14) %>%
        select(code, name, population)
    
    # write provincial data to file
    provinces %>%
        add_row(code = "ALL", name = "Canada", population = sum(provinces$population)) %>%
        write_csv("data/province_info.csv")
    
    # create vector of codes and urls
    prov_urls <- provinces %>% 
        pull(code) %>%
        paste0("province/", .) %>%
        c(., "")
    
    # get the data for each province
    all_stats <- purrr::map_dfr(prov_urls, function(x) {
        # the url is a function of the province code
        url <- paste0( "https://api.covid19tracker.ca/reports/",x,"?stat=vaccinations")
        
        # get the update and do some wrangling to tidy it
        prov_vs <- httr::GET(url) %>%
            httr::content("text") %>%
            jsonlite::fromJSON() %>% 
            enframe() %>% 
            pivot_wider(names_from = name, values_from = value) %>% 
            unnest(cols = c(province, data)) %>%
            mutate(date = lubridate::ymd(date))
    })
    
    # save to file
    all_stats %>%
        mutate(province = toupper(province)) %>%
        write_csv(file = "data/all.csv")
    
    # update the timestamp
    write_csv(file = "data/last_update.csv",
              tibble(x = Sys.time()))
    
}

## here is where we load the data if we didn't need to update it
all_stats <- read_csv("data/all.csv") %>%
    mutate(date = lubridate::ymd(date))
provinces <- read_csv("data/province_info.csv")

# base_url <- "https://api.covid19tracker.ca/reports?stat=vaccinations"
# url <- base_url
# 
# resp <- httr::GET(url)
# 
# if (httr::http_type(resp) != "application/json") {
#     stop("API did not return json.", .call = FALSE)
# }

# can we make a tibble with the supplementary information?
#supp_data 

supp_stats <- tibble(    code = all_stats %>%
               group_by(province) %>%
               select(province) %>%
               arrange(province) %>%
               distinct() %>%
               pull(province),
           
    date_first_vaccine = all_stats %>%
               group_by(province) %>%
               filter(total_vaccinations > 0) %>%
               arrange(province, date) %>%
               slice_head(n=1) %>%
               pull(date),
           
           date_last_vaccine = all_stats %>%
               group_by(province) %>%
               arrange(province, date) %>%
               slice_tail(n=1) %>%
               pull(date),
           
           vaccines_so_far = all_stats %>%
               group_by(province) %>%
        arrange(province) %>%
               select(total_vaccinations) %>%
               summarise(total_vaccinations = max(total_vaccinations, na.rm = TRUE)) %>%
                pull(total_vaccinations)
) %>%
    left_join(provinces) %>%
    mutate (avg_vaccines_per_minute = vaccines_so_far / as.numeric(difftime(date_last_vaccine, date_first_vaccine, unit = "mins")),
            vaccines_reqd = population * 2,
            mins_to_full_vaccination = floor(vaccines_reqd / avg_vaccines_per_minute),
            
            date_of_full_vaccination = Sys.Date() + lubridate::minutes(mins_to_full_vaccination))

all_stats <- all_stats %>%
    left_join(select(supp_stats, code, name), by = c("province"="code"))

# define lubridate date-time-stamp
sf <- stamp("Sunday, January 17, 1999, at 3:34PM")
# 
# all_stats %>%
#     group_by(province) %>%
#     summarise(date_first_vaccine = all_stats %>%
#                   group_by(province) %>%
#                   filter(total_vaccinations > 0) %>%
#                   arrange(date) %>%
#                   slice_head(n=1) %>%
#                   pull(date),
#               
#               date_last_vaccine = all_stats %>%
#                   arrange(date) %>%
#                   slice_tail(n=1) %>%
#                   pull(date),
#               
#               vaccines_so_far = all_stats %>%
#                   pull(total_vaccinations) %>%
#                   max(na.rm = TRUE)
#     )
# 
# jurisdiction_human <- "Canada"
# 
# vaccinations <- all_stats %>%
#     filter(province == "ALL")
# 
# date_first_vaccine <- vaccinations %>%
#     filter(total_vaccinations > 0) %>%
#     arrange(date) %>%
#     slice_head(n=1) %>%
#     pull(date)
# 
# date_last_vaccine <- vaccinations %>%
#     arrange(date) %>%
#     slice_tail(n=1) %>%
#     pull(date)
# 
# vaccines_so_far <- vaccinations %>%
#     pull(total_vaccinations) %>%
#     max()
# 
# avg_vaccines_per_minute <- vaccines_so_far / as.numeric(difftime(date_last_vaccine, date_first_vaccine, unit = "mins"))
# 
# pop <- 37900000
# 
# vaccines_reqd <- pop * 2
# 
# mins_to_full_vaccination <- floor(vaccines_reqd / avg_vaccines_per_minute)
# 
# date_of_full_vaccination <- Sys.Date() + lubridate::minutes(mins_to_full_vaccination)
# 
# predicted_line <- tribble(~date, ~total_vaccinations,
#                           date_first_vaccine, 0,
#                           date_of_full_vaccination, vaccines_reqd) %>%
#     mutate(date = as_date(date))
# 
# 
# 
# date_string <- sf(date_of_full_vaccination)
# 
# 
# vac_plot <- vaccinations %>%
#     filter(total_vaccinations > 0) %>%
#     ggplot(aes(x = date, y = total_vaccinations)) +
#     geom_line(aes(colour = "Vaccines Administered",
#                   linetype = "Vaccines Administered")) +
#     geom_point(aes(colour = "Vaccines Administered",
#                    linetype = "Vaccines Administered")) +
#     geom_hline(aes(yintercept = vaccines_reqd, 
#                    colour = "Total Vaccines Required",
#                    linetype = "Total Vaccines Required")) +
#     geom_line(data = predicted_line,
#               aes(linetype = "Predicted Vaccinations",
#                   colour = "Predicted Vaccinations")) +
#     scale_y_continuous(label = scales::comma_format()) +
#     theme_minimal() +
#     labs(x = "Date",
#          y = "Vaccines Administered",
#          title = paste0(jurisdiction_human," will be fully vaccinated on ", date_string, "!")) +
#     scale_linetype_manual("Vaccines Administered", values = c("dotted", "twodash", "solid")) +
#     scale_colour_brewer(palette = "Dark2" )
# 
# vac_plot %>%
#     plotly::ggplotly(dynamicTicks = TRUE)


prov_options <- c(provinces$code)
names(prov_options) <- provinces$name


# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    
    dashboardHeader(title="COVID Countdown!"),
    dashboardSidebar(
        
        
        shiny::selectInput("jurisdiction_select",
                           label = "Choose a Jurisdiction:",
                           choices = prov_options,
                           selected = "ALL")
    ),
    
    dashboardBody(
        
        
        # Sidebar with a slider input for number of bins 
        fluidRow(
            
            column(width = 12,
                   box(width = 12,
                       h1(textOutput("title_text")),
                       h5("Some conditions apply. See below."),
                       align = "center")
            ),
            
            box(width = 12,
                title = "Actual and Predicted Vaccinations",
                tags$div(class = "plot_box",
                         plotlyOutput("vaccine_plot"))
            ),
            
            box(width = 12,
                title = "Assumptions",
                tags$div(
                    tags$ul(
                        tags$li(textOutput("pop_text")),
                        tags$li(textOutput("vac_needed_text")),
                        tags$li(textOutput("vacs_given_text")),
                        tags$li(textOutput("vacs_avg_text")),
                        tags$li(textOutput("vacs_time_needed_text")),
                        tags$li(textOutput("vacs_conclusion_text"))
                    )
                )
            ),
            
            box(width = 12,
                title = "What's going on here?",
                p("This dashboard calculates how long it will take to vaccinate all Canadians if things continue at their current rate. The conclusions may be humourous, but they're based on real data and the implications couldn't be more serious.")
                ),
            
            box(width = 12,
                title = "Credits",
                p("Created by", tags$a(href = "http://cbelanger.netlify.app", "Christopher Belanger, PhD"), "."),
                p(tags$a(href = "http://cbelanger.netlify.app", "Read the blog post here"), ", and ", tags$a(href = "https://github.com/chris31415926535/covid_vaccine_predictor", "see the code on GitHub here"), "."),
                p("All data courtesy of the most-excellent ", tags$a(href = "https://covid19tracker.ca/", "COVID-19 Tracker Canada Project"), " and ", tags$a(href = "https://api.covid19tracker.ca/docs/1.0/overview", "their incredible API"), ".")
            ),
            
            tags$head(tags$style(type="text/css", ".plot_box {  max-width: 800px; text-align: center;   display: block;
  margin-left: auto;
  margin-right: auto;}"))
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$pop_text <- renderText({
        paste0(filteredStats()$name," has a population of ", filteredStats()$population %>% format(big.mark = ","),".")
    })
    
    output$vac_needed_text <- renderText({
        paste0("Each person will need two shots, for a total of ",filteredStats()$vaccines_reqd %>% format(big.mark = ",")," vaccines.")
    })
    
    output$vacs_given_text <- renderText({
        paste0(filteredStats()$name, " has administered ",filteredStats()$vaccines_so_far %>% format(big.mark = ",")," vaccines since giving its first shot on ", filteredStats()$date_first_vaccine, ".")
    })

    output$vacs_avg_text <- renderText({
        paste0("This works out to an average of ", filteredStats()$avg_vaccines_per_minute %>% round(digits = 1), " vaccines per minute.")
    })
    
    
    output$vacs_time_needed_text <- renderText({
        paste0("To fully vaccinate ", filteredStats()$name, " at this rate it will take ", filteredStats()$mins_to_full_vaccination, " minutes, which is ", (filteredStats()$mins_to_full_vaccination / 60 / 24) %>% round(digits = 1), " days.")
    })
    
    output$vacs_conclusion_text <- renderText({
        paste0("If we start right now, this will take until ", sf(filteredStats()$date_of_full_vaccination), ".")
        })
    
    output$vaccine_plot <- renderPlotly({
        
        
        vaccinations <- filteredData()
        statistics <- filteredStats()
        print(vaccinations)
        print(statistics)
        # 
        # date_first_vaccine <- vaccinations %>%
        #     filter(total_vaccinations > 0) %>%
        #     arrange(date) %>%
        #     slice_head(n=1) %>%
        #     pull(date)
        # 
        # message(paste0("date_first_vacc ", date_first_vaccine))
        # 
        # date_last_vaccine <- vaccinations %>%
        #     arrange(date) %>%
        #     slice_tail(n=1) %>%
        #     pull(date)
        # message(paste0("date last vacc ", date_last_vaccine))
        # 
        # vaccines_so_far <- max(vaccinations$total_vaccinations, na.rm = TRUE)
        # message(paste0("vaccines so far ",vaccines_so_far))
        # 
        # 
        # avg_vaccines_per_minute <- vaccines_so_far / as.numeric(difftime(date_last_vaccine, date_first_vaccine, unit = "mins"))
        # 
        # pop <- 37000000
        # 
        # prov_code <- filteredData() %>% 
        #             pull(province) %>%
        #             head(1)
        # 
        # #provinces[code == filteredData()$province[[1]]]$population
        # pop <- provinces[provinces$code == prov_code,]$population
        # message(paste("Pop: ", pop))
        # 
        # vaccines_reqd <- pop * 2
        # 
        # mins_to_full_vaccination <- floor(vaccines_reqd / avg_vaccines_per_minute)
        # message(paste0("mins to full: ", mins_to_full_vaccination))
        # 
        # date_of_full_vaccination <- Sys.Date() + lubridate::minutes(statistics()$mins_to_full_vaccination)
        # message(paste0("date of full ",statistics()$date_of_full_vaccination))
        
        predicted_line <- tribble(~date, ~total_vaccinations,
                                  statistics$date_first_vaccine, 0,
                                  statistics$date_of_full_vaccination, statistics$vaccines_reqd) %>%
            mutate(date = as_date(date))
        
        message(predicted_line)
        
        vac_plot <- filteredData() %>% #vaccinations %>%
            filter(total_vaccinations > 0) %>%
            ggplot(aes(x = date, y = total_vaccinations)) +
            geom_line(aes(colour = "Administered",
                          linetype = "Administered",
                      #    text = paste0("Jurisdiction: ", name,
                      #                  "\nDate: ", date,
                      #                  "\nVaccines Administered: ", total_vaccinations)
                      )) +
            geom_point(aes(colour = "Administered",
                           linetype = "Administered",
                           text = paste0("Jurisdiction: ", name,
                                         "\nDate: ", date,
                                         "\nVaccines Administered: ", total_vaccinations))) +
            geom_hline(aes(yintercept = statistics$vaccines_reqd, 
                           colour = "Required",
                           linetype = "Required")) +
            geom_line(data = predicted_line,
                      aes(linetype = "Predicted",
                          colour = "Predicted")) +
            scale_y_continuous(label = scales::comma_format()) +
            theme_minimal() +
            labs(x = "Date",
                 y = "Vaccines Administered",
                 title = NULL) + #"Actual and Predicted Vaccinations") +
            scale_linetype_manual("Vaccines Administered", values = c("dotted", "twodash", "solid")) +
            scale_colour_brewer(palette = "Dark2" ) +
            labs(linetype = NULL,
                 colour = NULL,
                 legend = NULL) +
            theme(legend.title = element_blank()) +
            scale_linetype(c("solid", "dashed", "dotted"))
        
        vac_plot %>%
            plotly::ggplotly(dynamicTicks = TRUE,
                             tooltip = c("text")) %>%
            plotly::layout(legend = list(orientation = "h", x = 0.33, y = -0.2))
        
    })
    
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
        all_stats %>%
            filter(province == input$jurisdiction_select)
        
    })
    
    filteredStats <- reactive({
        supp_stats %>%
            filter(code == input$jurisdiction_select)
    })
    
    title_text <- reactive({
        # jur_name <- provinces %>%
        #     filter(code == input$jurisdiction_select) %>%
        #     pull(name)
        # 
        date_string <- sf(filteredStats()$date_of_full_vaccination)
        text <- paste0(filteredStats()$name," will be fully vaccinated against COVID-19 on ", date_string, "!")
        message(text)
        
    })
    
    output$title_text <- renderText({
        date_string <- sf(filteredStats()$date_of_full_vaccination)
        paste0(filteredStats()$name," will be fully vaccinated against COVID-19 on ", date_string, "!")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
