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
        write_csv(file = "data/all.csv")
    
    # update the timestamp
    write_csv(file = "data/last_update.csv",
              tibble(x = Sys.time()))
    
}

## here is where we load the data if we didn't need to update it
all_stats <- read_csv("data/all.csv")
provinces <- read_csv("data/province_info.csv")

# base_url <- "https://api.covid19tracker.ca/reports?stat=vaccinations"
# url <- base_url
# 
# resp <- httr::GET(url)
# 
# if (httr::http_type(resp) != "application/json") {
#     stop("API did not return json.", .call = FALSE)
# }

jurisdiction_human <- "Canada"

vaccinations <- jsonlite::fromJSON(content(resp, "text")) %>% 
    enframe() %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    unnest(cols = c(province, data)) %>%
    mutate(date = lubridate::ymd(date))

date_first_vaccine <- vaccinations %>%
    filter(total_vaccinations > 0) %>%
    arrange(date) %>%
    slice_head(n=1) %>%
    pull(date)

date_last_vaccine <- vaccinations %>%
    arrange(date) %>%
    slice_tail(n=1) %>%
    pull(date)

vaccines_so_far <- vaccinations %>%
    pull(total_vaccinations) %>%
    max()

avg_vaccines_per_minute <- vaccines_so_far / as.numeric(difftime(date_last_vaccine, date_first_vaccine, unit = "mins"))

pop <- 37900000

vaccines_reqd <- pop * 2

mins_to_full_vaccination <- floor(vaccines_reqd / avg_vaccines_per_minute)

date_of_full_vaccination <- Sys.Date() + lubridate::minutes(mins_to_full_vaccination)

predicted_line <- tribble(~date, ~total_vaccinations,
                          date_first_vaccine, 0,
                          date_of_full_vaccination, vaccines_reqd) %>%
    mutate(date = as_date(date))

# define lubridate date-time-stamp
sf <- stamp("Sunday, January 17, 1999, at 3:34PM")

date_string <- sf(date_of_full_vaccination)


vac_plot <- vaccinations %>%
    filter(total_vaccinations > 0) %>%
    ggplot(aes(x = date, y = total_vaccinations)) +
    geom_line(aes(colour = "Vaccines Administered",
                  linetype = "Vaccines Administered")) +
    geom_point(aes(colour = "Vaccines Administered",
                   linetype = "Vaccines Administered")) +
    geom_hline(aes(yintercept = vaccines_reqd, 
                   colour = "Total Vaccines Required",
                   linetype = "Total Vaccines Required")) +
    geom_line(data = predicted_line,
              aes(linetype = "Predicted Vaccinations",
                  colour = "Predicted Vaccinations")) +
    scale_y_continuous(label = scales::comma_format()) +
    theme_minimal() +
    labs(x = "Date",
         y = "Vaccines Administered",
         title = paste0(jurisdiction_human," will be fully vaccinated on ", date_string, "!")) +
    scale_linetype_manual("Vaccines Administered", values = c("dotted", "twodash", "solid")) +
    scale_colour_brewer(palette = "Dark2" )

vac_plot %>%
    plotly::ggplotly(dynamicTicks = TRUE)


jurisdiction_options <- c(
    "Canada" = "all",
    
)


# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    
    dashboardHeader(title="COVID Countdown!"),
    dashboardSidebar(
        
        
        shiny::selectInput("jurisdiction_select",
                           label = "Choose a Jurisdiction:",
                           choices = c(1,2),
                           selected = 1)
    ),
    
    dashboardBody(
        
        
        # Sidebar with a slider input for number of bins 
        fluidRow(
            
            column(width = 12,
                   box(width = 12,
                       h1(paste0(jurisdiction_human," will be fully vaccinated against COVID-19 on ", date_string, "!")),
                       align = "center")
            ),
            
            box(width = 12,
                # got full-height leaflet map from here:
                # https://stackoverflow.com/questions/31278938/how-can-i-make-my-shiny-leafletoutput-have-height-100-while-inside-a-navbarpa
                plotlyOutput("vaccine_plot")
                #  leafletOutput("mymap")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    
    output$vaccine_plot <- renderPlotly({
        
        vac_plot <- vaccinations %>%
            filter(total_vaccinations > 0) %>%
            ggplot(aes(x = date, y = total_vaccinations)) +
            geom_line(aes(colour = "Vaccines Administered",
                          linetype = "Vaccines Administered")) +
            geom_point(aes(colour = "Vaccines Administered",
                           linetype = "Vaccines Administered")) +
            geom_hline(aes(yintercept = vaccines_reqd, 
                           colour = "Total Vaccines Required",
                           linetype = "Total Vaccines Required")) +
            geom_line(data = predicted_line,
                      aes(linetype = "Predicted Vaccinations",
                          colour = "Predicted Vaccinations")) +
            scale_y_continuous(label = scales::comma_format()) +
            theme_minimal() +
            labs(x = "Date",
                 y = "Vaccines Administered",
                 title = "Actual and Predicted Vaccinations") +
            scale_linetype_manual("Vaccines Administered", values = c("dotted", "twodash", "solid")) +
            scale_colour_brewer(palette = "Dark2" ) +
            labs(linetype = NULL,
                 colour = NULL,
                 legend = NULL) +
            theme(legend.title = element_blank())
        
        vac_plot %>%
            plotly::ggplotly(dynamicTicks = TRUE)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
