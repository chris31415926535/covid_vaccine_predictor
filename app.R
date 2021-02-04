# COVID VACCINE PREDICTOR
# Christopher Belanger, 2021-02-01, christopher.a.belanger@gmail.com
# A demo project to learn more about building Shiny dashboards?
# A darkly humourous data-driven dive into Canada's COVID-19 vaccination delivery?


library(shiny)
library(shinydashboard)
library(lubridate)
library(plotly)
library(httr)
library(stringr)
library(dplyr)
library(readr)
library(tidyr)
library(tibble)


# debugging flag
verbose <- FALSE

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
    if (verbose) message("Updating data...")
    
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
last_update <- read_csv("data/last_update.csv") %>% pull(1)


# make a tibble with the supplementary information

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
            
            date_of_full_vaccination = Sys.time() + lubridate::minutes(mins_to_full_vaccination))

all_stats <- all_stats %>%
    left_join(select(supp_stats, code, name), by = c("province"="code"))

# define lubridate date-time-stamp
sf <- stamp("Sunday, January 17, 1999, at 3:34PM")

prov_options <- c(provinces$code)
names(prov_options) <- provinces$name

tz_options <- c(
    "Pacific  (GMT-8:00)" = "America/Vancouver",
    "Mountain (GMT-7:00)" = "America/Edmonton",
    "Central  (GMT-6:00)" = "America/Winnipeg",
    "Eastern  (GMT-5:00)" = "America/Toronto",
    "Atlantic (GMT-4:00)" = "America/Moncton",
    "Nfld.    (GMT-3:30)" = "America/St_Johns"
)

tz_abbrs <- c(
    "America/Vancouver" = "PST",
    "America/Edmonton" = "MST",
    "America/Winnipeg" = "CST",
    "America/Toronto" = "EST",
    "America/Moncton" = "AST",
    "America/St_Johns" = "NST"
)


# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    dashboardHeader(title="COVID Countdown!"),
    
    dashboardSidebar(
        
        
        shiny::selectInput("jurisdiction_select",
                           label = "Choose a Jurisdiction:",
                           choices = prov_options,
                           selected = "ALL"),
        
        shiny::selectInput("tz_select",
                           label = "Choose a Time Zone:",
                           choices = tz_options,
                           selected = "America/Toronto")
    ),
    
    dashboardBody(
        # google analytics tag, as per https://shiny.rstudio.com/articles/google-analytics.html
        #tags$head(includeHTML(("google_analytics.html"))),
        tags$head(HTML(
           '<!-- Global site tag (gtag.js) - Google Analytics -->
                <script async src="https://www.googletagmanager.com/gtag/js?id=G-DNQWNHXBR7"></script>
                <script>
                window.dataLayer = window.dataLayer || [];
            function gtag(){dataLayer.push(arguments);}
            gtag("js", new Date());
            
            gtag("config", "G-DNQWNHXBR7");
            </script>'
                
        )),
        
        tags$head(HTML(
            '<meta property="og:image" content="https://cbelanger.netlify.app/img/headers/2021-02-04-linkedin-thumbnail.png">'
        )),
        
        tags$head(HTML(
            '<meta property="og:description" content="This dashboard calculates how long it will take to vaccinate all Canadians if things continue at their current rate. The conclusions may be humourous, but they\'re based on real data and the implications couldn\'t be more serious.">'
        )),
        
        tags$head(HTML(
            '<meta name="author" content="Christopher Belanger">'
        )),
        
        tags$head(HTML(
            '<meta name="title" property="og:title" content="COVID Countdown Dashboard! How long until Canada and the provinces/territories are fully vaccinated against COVID-19?">'
        )),
        
        
        # Sidebar with a slider input for number of bins 
        fluidRow(
            
            column(width = 12,
                   box(width = 12,
                       h3("Good news! At the rate we're going,"),
                       h1(textOutput("title_text")),
                       h5(textOutput("last_updated_text")),
                       h5("Not an official prediction! Some conditions apply. See below."),
                       align = "center")
            ),
            
            box(width = 12,
                title = "Actual and Predicted Vaccinations (Click and Drag to Zoom)",
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
                p("Created by Christopher Belanger, Phd ", tags$a(href = "http://cbelanger.netlify.app", "(Sometimes I Blog!)"), tags$a(href = "https://www.linkedin.com/in/christopherabelanger/", "(I'm on LinkedIn!)")),
                p(tags$a(href = "http://cbelanger.netlify.app/post/covid-countdown-building-a-shiny-dashboard-to-track-vaccinations-in-canada/", "Read the blog post here,"), " and ", 
                  tags$a(href = "https://github.com/chris31415926535/covid_vaccine_predictor", "see the code on GitHub here.")),
                p("All data courtesy of the most-excellent ", tags$a(href = "https://covid19tracker.ca/", "COVID-19 Tracker Canada Project"), " and ", tags$a(href = "https://api.covid19tracker.ca/docs/1.0/overview", "their incredible API."))
            ),
            
            # css to set max-width for the plot
            tags$head(tags$style(type="text/css", ".plot_box {  max-width: 800px; 
                                 text-align: center;   
                                 display: block; 
                                 margin-left: auto;  
                                 margin-right: auto;}"))
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$last_updated_text <- renderText({
        paste0("Data last updated at ", sf(last_update %>% with_tz(tzone = input$tz_select)), " ", tz_abbrs[[input$tz_select]], ".")
    })
    
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
        paste0("This works out to an average of ", filteredStats()$avg_vaccines_per_minute %>% round(digits = 2), " vaccines per minute.")
    })
    
    
    output$vacs_time_needed_text <- renderText({
        paste0("To fully vaccinate ", filteredStats()$name, " at this rate it will take ", filteredStats()$mins_to_full_vaccination, " minutes, which is ", (filteredStats()$mins_to_full_vaccination / 60 / 24) %>% round(digits = 1), " days.")
    })
    
    output$vacs_conclusion_text <- renderText({
        paste0("If we started when you loaded this page, this would take until ", sf(filteredStats()$date_of_full_vaccination %>% with_tz(tzone = input$tz_select)), " ", tz_abbrs[[input$tz_select]], ".")
    })
    
    output$vaccine_plot <- renderPlotly({
        
        vaccinations <- filteredData()
        statistics <- filteredStats()
        #print(vaccinations)
        #print(statistics)
 
        predicted_line <- tribble(~date, ~total_vaccinations,
                                  statistics$date_first_vaccine, 0,
                                  statistics$date_of_full_vaccination, statistics$vaccines_reqd) %>%
            mutate(date = as_date(date))
        
        if (verbose) message(predicted_line)
        
        vac_plot <- filteredData() %>% #vaccinations %>%
            filter(total_vaccinations > 0) %>%
            ggplot(aes(x = date, y = total_vaccinations)) +
            geom_line(aes(colour = "Administered",
                          linetype = "Administered")) +
            geom_point(aes(colour = "Administered",
                           linetype = "Administered",
                           text = paste0("Jurisdiction: ", name,
                                         "\nDate: ", date,
                                         "\nTotal Vaccines Administered: ", total_vaccinations))) +
            geom_hline(aes(yintercept = statistics$vaccines_reqd, 
                           colour = "Required",
                           linetype = "Required")) +
            geom_line(data = predicted_line,
                      aes(linetype = "Predicted",
                          colour = "Predicted")) +
            scale_y_continuous(label = scales::comma_format()) +
            theme_minimal() +
            labs(x = "Date",
                 y = "Total Vaccines Administered",
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
            plotly::layout(legend = list(orientation = "h", x = 0.27, y = -0.2))
        
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
    
    output$title_text <- renderText({
        paste0(filteredStats()$name," will be fully vaccinated against COVID-19 on ", sf(filteredStats()$date_of_full_vaccination %>% with_tz(tzone = input$tz_select)), " ", tz_abbrs[[input$tz_select]], "!")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
