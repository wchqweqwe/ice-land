library(sf)
library(shiny)
library(ggplot2)
library(leaflet)
library(shinydashboard)
library(dplyr)
library(DT)
library(maps)
library(readr)
library(base64enc)

img_data <- base64encode("map.jpg")
img_data1 <- base64encode("licensed-image.jpeg")
#data load
greenland_people = read_csv("demograghic data/UNdata_Export_20231216_233521232.csv")
fareo_people = read_csv("demograghic data/UNdata_Export_20231216_233659425.csv")
iceland_people =read_csv("demograghic data/population.csv")
iceland_marriage =  read_csv("demograghic data/marriage.csv")
iceland_death <- read_csv("demograghic data/death.csv")



# data processing
iceland_marriage = na.omit(iceland_marriage )
iceland_marriage <- iceland_marriage %>%
  group_by(Year) %>%
  summarise(marriage = sum(Value))
iceland_marriage$Year = as.numeric(iceland_marriage$Year )

iceland_marriage$show_point <- iceland_marriage$Year %% 5 == 0


iceland_death1 <- iceland_death  %>%
  group_by(Year) %>%
  summarise(death = sum(Value))



# Plot

# Verbal Comparison Content
verbal_comparison_content <- function() {
  comparison_text <- "
  Iceland, the Faroe Islands, and Greenland are unique in their own ways, each offering distinct geographical, economic, and social characteristics:

  Geography:
  - Iceland, known for its volcanic landscape and geothermal activity, is the largest and most populous of the three, located southeast of Greenland.
  - The Faroe Islands, an archipelago between the Norwegian Sea and the North Atlantic Ocean, are characterized by rugged, rocky coasts and a maritime climate.
  - Greenland, the world's largest island, is predominantly covered by ice, with a sparse population mostly along the coast.

  Economy:
  - Iceland's economy is diverse, with strong sectors in fisheries, renewable energy, and tourism.
  - The economy of the Faroe Islands is primarily based on fishing and aquaculture, along with tourism and some petroleum activity.
  - Greenland's economy is heavily dependent on fishing and subsidies from Denmark, though there is potential for mining and oil exploration.

  Demographics:
  - Iceland has a population of about 340,000, with most people living in the capital, Reykjavik.
  - The Faroe Islands have a smaller population of around 54,000, with a majority living in the capital, Tórshavn.
  - Greenland has the smallest population, about 56,000, with Nuuk as its largest city and capital.

  Social Factors:
  - Iceland ranks high in terms of quality of life, education, and healthcare. It's known for its progressive social policies.
  - The Faroe Islands also enjoy high standards of living, with a strong sense of community and cultural heritage.
  - Greenland faces more social challenges, including higher rates of poverty and lower life expectancy compared to Iceland and the Faroe Islands.

  While these islands share some similarities, such as their Nordic ties and challenges related to their remote and harsh climates, each has developed its unique way of life and societal structure."
  return(comparison_text)
}


# Define the cities data
cities <- data.frame(
  name = c("Reykjavik", "Akureyri", "Keflavik", "Hafnarfjörður", "Kópavogur", "Egilsstaðir"),
  lat = c(64.1466, 65.6835, 64.0028, 64.0671, 64.1101, 65.2642),
  lng = c(-21.9426, -18.1059, -22.6216, -21.9403, -21.9055, -14.3948)
)





server <- function(input, output) {
  
  # Iceland Population Projection Plot
  output$combinedPlot <- renderPlot({
    selected_variant <- input$variantSelect
    
    filtered_iceland_people <- iceland_people[iceland_people$Variant == selected_variant, ]
    filtered_fareo_people <- fareo_people[fareo_people$Variant == selected_variant, ]
    filtered_greenland_people <- greenland_people[greenland_people$Variant == selected_variant, ]
    
    # Combine the filtered data for plotting
    combined_data <- rbind(filtered_iceland_people, filtered_fareo_people, filtered_greenland_people)
    
    ggplot(combined_data, aes(x = `Year(s)`, y = Value, color = `Country or Area`)) +
      geom_point() +
      theme_minimal() +
      labs(title = paste("Population Projections for", selected_variant), x = "Year", y = "Population")
  })
  
  # Verbal Comparison
  output$Comparison <- renderPrint({
    cat("Here you can provide a detailed comparison of Iceland, the Faroe Islands, and Greenland in terms of population, economy, geography, and other relevant factors.")
  })
  
  # Detailed map of Iceland
  output$icelandMap <- renderLeaflet({
    iceland_data <- st_read("iceland_regions.topo.geojson")
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = iceland_data, fillColor = "blue", weight = 1, smoothFactor = 0.5) %>%
      setView(lng = -19.0208, lat = 64.9631, zoom = 5.25) %>%
      addMarkers(data = cities, ~lng, ~lat, popup = ~name)
  })
  
  # Verbal Comparison
  output$verbalComparison <- renderPrint({
    cat(verbal_comparison_content())
  })
  
  # World map showing Iceland's location
  output$worldMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      setView(lng = -19.0208, lat = 64.9631, zoom = 1) %>%
      addMarkers(lng = -19.0208, lat = 64.9631, popup = "Iceland")
  })
  
  # Marriage in Iceland plot
  output$marriage_plot <- renderPlot({
    ggplot(iceland_marriage, aes(x = Year, y = marriage)) +
      geom_line() + 
      geom_point() +
      labs(title = "Yearly Values for Iceland", x = "Year", y = "Value")
  })

  # Deaths in Iceland plot
  output$deathPlot <- renderPlot({
    ggplot(iceland_death1, aes(x = Year, y = death)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma)
  })
  
  # Render the data table in the UI
  output$data_table <- renderDT({
    datatable(iceland_death, options = list(pageLength = 10))
  })
}




ui <- fluidPage(
  # Include custom CSS to increase font size
  tags$head(
    tags$style(HTML("
            body { 
                font-size: 18px; /* Adjust the size as needed */
            }
        "))
  ),
  navbarPage("Iceland Analysis Dashboard",
                 # Introduction tab
                 tabPanel("Introduction",
                          fluidPage(
                            h1("Welcome to the Iceland Analysis Dashboard"),
                            p("This dashboard was created by Chenghan Wu."),
                            p("This dashboard presents a comprehensive and interactive examination of a specific island state, crafted using R and Shiny. This project represents the culmination of an extensive study in data visualization and analytical techniques."),
                            p("It features an engaging live presentation that includes detailed maps, vital statistics, narrative descriptions, and thorough demographic insights. Additionally, the dashboard offers a comparative analysis with neighboring island states and a detailed SWOT analysis, highlighting the strengths, weaknesses, opportunities, and threats of the focal island."),
                            p("Prioritizing clarity and conciseness, this presentation skillfully blends various data representation forms like interactive maps, dynamic charts, insightful images, informative tables, and compelling narratives.")
                          ),
                          img(src = paste0("data:image/jpg;base64,", img_data1), style = "width:60%; height:auto;")
                 ),
                 # Key Demographics tab with subpanels for maps
                 tabPanel("Iceland Maps",
                          fluidPage(
                            h2("Iceland Location"),
                            tabsetPanel(
                              tabPanel("Detailed Map of Iceland",
                                       fluidPage(
                                         leafletOutput("icelandMap", height = "400px"), # Set the height as needed
                                         p("Description: 
                                           This detailed map showcases Iceland’s topography and administrative regions. It provides a closer look at the geographic distribution and key landmarks of the country.
                                           Moreover, as we can see from the map, the states are clearly divided, and most of large cities are around southwest corner.")
                                       )
                              ),
                              tabPanel("Iceland on World Map",
                                       fluidPage(
                                         leafletOutput("worldMap", height = "400px"), # Set the height as needed
                                         p("Description: 
                                           This world map indicates the location of Iceland, Iceland is situated in the North Atlantic Ocean, close to the Arctic Circle.
                                           It lies northwest of the United Kingdom and east of Greenland, bridging the gap between North America and Europe. 
                                           The island's temperate climate, influenced by the North Atlantic Current, belies its northern latitude. Renowned for its 
                                           dramatic landscapes of volcanoes and glaciers, Iceland's location contributes to its unique geological and cultural significance 
                                           on the world stage.")
                                       )
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Key Demographics",
                          fluidPage(
                            h2("Key Demographics of Iceland"),
                            tabsetPanel(
                              tabPanel("Marriage in Iceland",
                                       fluidPage(
                                         h2("Marriage in Iceland by Selected Year"),
                                         plotOutput("marriage_plot"),
                                         h3("Explanation of Data"),
                                         p("This plot shows marriage data in Iceland for all years."),
                                         p("As we can see form this plot, the up and down patterns are pretty steep, indicating that this merriage is not steady 
                                           and may be same as somthing important events happend  ")
                                       )
                              ),
                              tabPanel("Deaths in Iceland",
                                       fluidPage(
                                         h2("Deaths in Iceland by Selected Month Across Years"),
                                         plotOutput("deathPlot"),
                                         h3("Explanation of Data"),
                                         p("This plot shows the number of deaths in Iceland for across different years."),
                                         p("from here we see a potential upward trend, which means that more and more people died,
                                           but this may also caused by there are more people borned. "),
                                         dataTableOutput("data_table") 
                                       )
                              )
                            )
                          )
                 ),
                 # Comparison tab with subpanels
                 tabPanel("Comparison",
                          fluidPage(
                            h2("Comparison with Other Island States"),
                            tabsetPanel(
                              tabPanel("Introduction",
                                       fluidPage(
                                         p("This dashboard provides insights into the demographics and key characteristics of Iceland, along with a comparison with other island states like the Faroe Islands and Greenland."),
                                         p("We mainly focus on islands near Iceland, Faroe Islands, and Greenland. In the second page, we will show the demographic population projection for these three islands to enable a comparison."),
                                         p("In the final page, we will compare these three islands in various aspects to see the differences between them and gain insights for a SWOT analysis for Iceland.")
                                       ),
                                       img(src = paste0("data:image/jpg;base64,", img_data), style = "width:60%; height:auto;")
                              ),
                              tabPanel("Population Projections",
                                       fluidPage(
                                         h2("Population Projections: Faroe Islands and Greenland"),
                                         selectInput("variantSelect", "Select Variant:", choices = c("Constant fertility", "Constant mortality", "Zero migration", "High", "Low", "Medium")),
                                         plotOutput("combinedPlot"),
                                         h3("Explanation of Variants"),
                                         p("Medium: Represents a median projection, assuming average trends in fertility, mortality, and migration continue."),
                                         p("High/Low: These projections show the potential population size under higher or lower than expected trends in fertility, mortality, and migration."),
                                         p("Constant Fertility: Assumes that fertility rates remain constant at current levels."),
                                         p("Constant Mortality: Assumes that mortality rates do not change over time."),
                                         p("Zero Migration: Assumes no migration in or out of the population.")
                                       )
                              ),
                              tabPanel("Text Comparison",
                                       fluidPage(
                                         h2("Comparative Analysis"),
                                         verbatimTextOutput("verbalComparison")
                                       )
                              )
                            )
                          )
                 ),
                 # SWOT Analysis tab
                 tabPanel("SWOT Analysis",
                          fluidPage(
                            h2("SWOT Analysis of Iceland"),
                            fluidRow(
                              column(6,
                                     h3("Strengths"),
                                     tags$ul(
                                       tags$li("Iceland has abundant renewable energy resources, particularly geothermal and hydroelectric power, making it one of the world's leaders in renewable energy."),
                                       tags$li("The country's unique landscape, including glaciers, hot springs, and volcanoes, attracts tourists worldwide, boosting its economy."),
                                       tags$li("Iceland is known for its stable political environment and high level of social trust."),
                                       tags$li("High living standards, excellent healthcare, and education systems.")
                                     )
                              ),
                              column(6,
                                     h3("Weaknesses"),
                                     tags$ul(
                                       tags$li("Heavy reliance on tourism and fishing, which makes its economy vulnerable to global fluctuations in these sectors."),
                                       tags$li("Being an island nation, Iceland faces challenges related to transportation and access to certain goods and markets."),
                                       tags$li("The small population limits the scale of the domestic market and workforce."),
                                       tags$li("Due to its remote location, the cost of living and doing business in Iceland can be high.")
                                     )
                              )
                            ),
                            fluidRow(
                              column(6,
                                     h3("Opportunities"),
                                     tags$ul(
                                       tags$li("Opportunities to expand in green energy, eco-tourism, and sustainable fisheries."),
                                       tags$li("Potential for growth in tech sectors, especially those leveraging Iceland's unique data center advantages (cool climate and renewable energy)."),
                                       tags$li("Forming strategic alliances can enhance trade, cultural exchange, and political cooperation."),
                                       tags$li("Leveraging its high education standards for research and development, particularly in environmental sciences and renewable energy.")
                                     )
                              ),
                              column(6,
                                     h3("Threats"),
                                     tags$ul(
                                       tags$li("As a country with significant ice masses, Iceland faces direct threats from climate change, including melting glaciers and changing ecosystems."),
                                       tags$li("Global economic downturns can significantly impact its key sectors like tourism and fishing."),
                                       tags$li("Increased tourism and industrial activity could strain Iceland's pristine environment."),
                                       tags$li("Competition in the tourism sector and renewable energy market from other countries.")
                                     )
                              )
                            )
                          )
                 ),
                 navbarMenu(
                   title = "Bibliography",
                   align = "right",
                   tabPanel(tags$a(href = "https://data.un.org/Search.aspx?q=iceland", "data source")),
                   tabPanel(tags$a(href = "https://www.shinyapps.io/", "shinyapps.io for publishing")),
                   tabPanel(tags$a(href = "https://en.wikipedia.org/wiki/Iceland", "Wikipedia/iceland")),
                   tabPanel(tags$a(href = "https://chat.openai.com/", "Chatgpt"))
                 )
))

# Run the application
shinyApp(ui = ui, server = server)
