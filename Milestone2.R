library(shiny)
library(countrycode)
library(tidyverse)
energy <- read.csv("./owid-energy.csv")
energy_long <- energy |> pivot_longer(c("biofuel_electricity", "coal_electricity", "fossil_electricity", "gas_electricity", "hydro_electricity", "low_carbon_electricity", "nuclear_electricity", "oil_electricity", "other_renewable_electricity", "renewables_electricity", "solar_electricity", "wind_electricity"), names_to= "energy_type", values_to = "energy_values")
energy_long <- energy_long |> 
  mutate(continent = countrycode(country, origin="country.name", destination="continent"), energy_type = str_to_title(str_replace_all(energy_type, "_", " "))) |>
  filter(continent != "NA" & continent != "Antarctica" & energy_values != 0) 


facet_data <- function (){
  energy_long |>
    ggplot() + 
    geom_boxplot(aes(energy_values, energy_type, col=continent)) + 
    scale_x_log10() +
    facet_wrap(.~continent) +
    theme_bw() + 
    xlab("Electricity Generation (Terawatt-Hours)")+
    ylab("Energy Type") +
    theme(axis.text.x = element_text(angle=45)) + 
    ggtitle("Electricity Generation vs Energy Type, Faceted by Continent")
}

ui <- fluidPage(
  h1("Shiny Energy Data Visualization"),
  plotOutput("plot")
)

server <- function(input, output){
  output$plot <- renderPlot({
    facet_data()
  })
}

app <- shinyApp(ui, server)
app