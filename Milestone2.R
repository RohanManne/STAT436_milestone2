library(shiny)
library(tidyverse)
library(plotly)
library(countrycode)

energy <- read.csv("owid-energy.csv")
renewable_share <- read.csv("renewable-share-energy.csv")

# data for choropleth map
renewable_share <- renewable_share %>%
  rename(Country = Entity, Code = Code, Year = Year, 
         Renewable_Percentage = Renewables) %>%
  mutate(continent = countrycode(Country, origin = "country.name", destination = "continent"))

energy_long <- energy %>%
  pivot_longer(
    c("biofuel_electricity", "coal_electricity", "fossil_electricity", "gas_electricity", 
      "hydro_electricity", "low_carbon_electricity", "nuclear_electricity", "oil_electricity", 
      "other_renewable_electricity", "renewables_electricity", "solar_electricity", "wind_electricity"),
    names_to = "energy_type",
    values_to = "energy_values"
  ) %>%
  mutate(
    continent = countrycode(country, origin = "country.name", destination = "continent"),
    energy_type = str_to_title(str_replace_all(energy_type, "_", " "))
  ) %>%
  filter(continent != "NA" & continent != "Antarctica" & energy_values != 0)

facet_data <- function() {
  ggplot(energy_long) + 
    geom_boxplot(aes(energy_values, energy_type, color = continent)) + 
    scale_x_log10() +
    facet_wrap(. ~ continent) +
    theme_bw() + 
    xlab("Electricity Generation (Terawatt-Hours)") +
    ylab("Energy Type") +
    theme(axis.text.x = element_text(angle = 45)) + 
    ggtitle("Electricity Generation vs Energy Type, Faceted by Continent")
}

ribbon_data <- function() {
  energy_long %>% group_by(continent, year) %>% 
    summarize(rc = sum(renewables_consumption, 
                       na.rm = TRUE), .groups = "drop") %>%

    ggplot() +
    geom_ribbon(aes(x = year, ymin = 0, 
                    ymax = rc, color = continent, fill = continent),
                linewidth = 0.7, alpha = 0.3) +
    #scale_y_log10() +
    scale_x_continuous(expand = c(0, 0)) +
    xlab("Year") +
    ylab("Renewable Energy Consumption (terawatt-hours)") +
    ggtitle("Renewable Energy Consumption by Year")
}

ui <- fluidPage(
  titlePanel("Renewable Energy Analysis by Country"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = sort(unique(renewable_share$Year)), selected = 1965)
    ),
    mainPanel(
      plotlyOutput("choropleth_map"),
      plotOutput("plot"),
      plotOutput("ribbon")
    )
  )
)

server <- function(input, output, session) {
  
  # choropleth map using Plotly
  output$choropleth_map <- renderPlotly({
    selected_data <- renewable_share %>% filter(Year == input$year)
    
    all_countries <- unique(countrycode::codelist$iso3c)
    full_data <- tibble(Code = all_countries) %>%
      left_join(selected_data, by = "Code") %>%
      mutate(Display_Value = ifelse(is.na(Renewable_Percentage), -1, Renewable_Percentage))
    
    # custom color scale where -1 (NA) is gray
    custom_colorscale <- list(
      list(0, "gray"),
      list(0.0001, "lightblue"),
      list(1, "darkblue")
    )
    
    max_percent <- max(full_data$Renewable_Percentage, na.rm = TRUE)
    
    plot_ly(data = full_data, type = 'choropleth', locations = ~Code,
            z = ~Display_Value, text = ~paste0(Country, "<br>Year: ", input$year, "<br>Renewable %: ", ifelse(is.na(Renewable_Percentage), "N/A", Renewable_Percentage)),
            colorscale = custom_colorscale, zmin = 0, zmax = max_percent,
            marker = list(line = list(color = 'white', width = 0.5))) %>%
      colorbar(title = "Renewable Energy %") %>%
      layout(
        geo = list(
          showframe = FALSE,
          showcoastlines = FALSE,
          projection = list(type = 'equirectangular'),
          bgcolor = "white"
        ),
        title = paste("Renewable Energy Percentage by Country in", input$year)
      )
  })
  
  output$plot <- renderPlot({
    facet_data()
  })
  
  output$ribbon <- renderPlot({
    ribbon_data()
  })
}

shinyApp(ui, server)