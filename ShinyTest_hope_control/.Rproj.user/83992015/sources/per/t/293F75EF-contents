
library(dplyr)
library(readxl)
library(raster)
library(stringr)
library(leaflet)
library(ggplot2)
library(shiny)

Sys.setlocale("LC_CTYPE", "russian")

GetFilePopulation <- function() {
    
    file <- read.csv(
        "C:\\Users\\User\\Downloads\\forFBpost.csv",
        fileEncoding = "utf-8",
        sep = ";"
    )
    
    colnames(file) <-
        c("Town",
          "Year",
          "Fact_value",
          "Predicted_value",
          "Lower_border",
          "Upper_border")
    
    file$Town <- stringr::str_trim(file$Town)
    
    return(file)
}

GetTownsRegionsFile <- function() {
    
    regions_towns <- readxl::read_xlsx("C:/Users/User/Documents/ShinyTest2/data/table.xlsx")
    regions_towns[c("Район/подчинение", "Центр")] <- NULL
    colnames(regions_towns) <- c("Town", "Code", "Region")
    regions_towns$Region <- stringr::str_replace_all(regions_towns$Region, "Область", "область")
    regions_towns$Region <- stringr::str_replace_all(regions_towns$Region, "Край", "край")
    
    return(regions_towns)
}

GetShapeFile <- function() {
    shape_file <- readRDS("C:/Users/User/Documents/ShinyTest2/data/gadm36_RUS_1_sp.rds")
    return(shape_file)
}

ExtendPopulationFile <- function(region_population, regions_towns) {
    
    extend_region_population <- region_population %>% left_join(regions_towns, by = "Town")
    
    extend_region_population$Region <-
        plyr::revalue(
            extend_region_population$Region,
            c(
                "Мордовия Республика" = "Республика Мордовия",
                "Санкт-Петербург Город" = "Санкт-Петербург (горсовет)",
                "Камчатский край" = "Камчатская край",
                "Пермский край" = "Пермская край",
                "Ханты-Мансийский Автономный округ - Югра Автономный округ" = "Ханты-Мансийский АОк",
                "Ямало-Ненецкий Автономный округ" = "Ямало-Ненецкий АОк",
                "Чукотский Автономный округ" = "Чукотский АОк",
                "Башкортостан Республика" = "Республика Башкортостан",
                "Калмыкия Республика" = "Республика Калмыкия",
                "Карелия Республика" = "Республика Карелия",
                "Коми Республика" = "Республика Коми",
                "Марий Эл Республика" = "Республика Марий Эл",
                "Северная Осетия - Алания Республика" = "Республика Северная Осетия-Алания",
                "Карачаево-Черкесская Республика" = "Карачаево-Черкессия Республика",
                "Татарстан Республика" = "Республика Татарстан",
                "Тыва Республика" = "Республика Тыва",
                "Хакасия Республика" = "Республика Хакасия",
                "Чеченская Республика" = "Республика Чечено-Ингушская",
                "Чувашская Республика - Чувашия" = "Чувашская Республика",
                "Саха /Якутия/ Республика" = "Республика Саха",
                "Еврейская Автономная область" = "Eврейская АОб"
            )
        )
    
    return(extend_region_population)
    
}

AverageNumberOfPopulation <- function(region_population) {
    
    average_values <- region_population %>%
        filter(!is.na(Fact_value)) %>%
        filter(!is.na(Year)) %>%
        group_by(Year) %>%
        summarise(
            fact_total_population = sum(Fact_value, na.rm = TRUE),
            predicted_total_population = sum(Predicted_value, na.rm = TRUE)
        )
    
    average_values_long <- tidyr::pivot_longer(data = average_values,
                                               cols =  c("fact_total_population",
                                                         "predicted_total_population"))
    
    population_fact_values <- average_values_long %>%
        ggplot(aes(x= Year, y = value, group = name,col = name)) +
        geom_line(size = 1) +
        labs(title = "Сравнение фактических и предсказанных \n значений численности населения",
             x = "Год",
             y = "Численность",
             legend = "colour") +
        guides() +
        scale_y_continuous(labels = scales::label_number_si()) +
        scale_x_continuous(labels = scales::label_number(accuracy = 1, big.mark = ""))+
        scale_color_manual(values = c("royalblue4", "firebrick3"),
                           labels = c("Факт", "Прогноз")) +
        theme(legend.position = "top",
              legend.justification = "left",
              legend.title = element_blank(),
              legend.background = element_rect(fill = "whitesmoke"),
              plot.background = element_rect(fill = "whitesmoke"),
              panel.background = element_rect(fill = "whitesmoke"),
              panel.grid = element_line(colour = "grey90"),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank()
        )
    
    return(population_fact_values)
}

ConfidenceIntervalForEachTown <- function(region_population, input_town) {
        
        if(input_town == "") {
            input_town <- "Томск"
        }
    
        population_for_town <- region_population %>%
            filter(!is.na(Fact_value)) %>%
            filter(Town == input_town)
        
        tmp_df1 <- data.frame(count = population_for_town$Fact_value, type = c("Фактическое значение"))
        tmp_df2 <- data.frame(count = population_for_town$Predicted_value, type = c("Предсказанное значение"))
        full_data <- rbind(tmp_df1, tmp_df2)
        
        ready_boxplot <- boxplot(
            count ~ type,
            data = full_data,
            xlab = " ",
            ylab = "Численность",
            main =  paste("Статистическая характеристика для ", input_town),
            col = c("firebrick3", "royalblue4")
        )
        
        return(ready_boxplot)
}

Merge2FilesForPopulation <- function(data1, data2) {
    
    tmp_df1 <- as.data.frame(data1)
    colnames(tmp_df1) <- "Region"
    tmp_df1$id <- 1:nrow(tmp_df1)
    tmp_df2 <- merge(x = tmp_df1, y = data2, by = "Region", all.x = TRUE)
    tmp_df2[order(tmp_df2$id), ]
    
    return(tmp_df2)
}

PrepareDataForMap <- function(region_population, extend_region_population, input_year = "2021") {
    
        shape_file <- GetShapeFile()
            
        population_by_region <- extend_region_population %>%
            filter(!is.na(Town)) %>%
            filter(!is.na(Region)) %>%
            filter(Year == input_year) %>%
            group_by(Region) %>%
            summarise(population = sum(Predicted_value, na.rm = TRUE))
        
        data <- Merge2FilesForPopulation(shape_file$NL_NAME_1, population_by_region)
        shape_file@data$population <- data$population
        
        return(shape_file)
}

MapOfRussia <- function(country) {
    palpop <- colorBin("YlOrBr", country$population)
    
    map <- leaflet(country) %>%
        setView(lng = 90, lat = 70, zoom = 3) %>%
        addPolygons(
            color = ~ palpop(population),
            smoothFactor = 0.5,
            weight = 4.0,
            opacity = 1.0,
            fillOpacity = 1.0,
            highlightOptions = highlightOptions(
                weight = 1,
                color = "brown",
                fillOpacity = 0.7,
                bringToFront = TRUE
            ),
            label = paste(
                "<strong>County:</strong>",
                country$NAME_1,
                "<br>",
                "<strong>Total Population:</strong>",
                country$population
                
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(
                style = list("font-weight" = "normal",
                             padding = "3px 8px"),
                textsize = "13px",
                direction = "auto"
            ),
            popup = ~ paste(
                "<strong>County:</strong>",
                NAME_1,
                "<br>",
                "<strong>Total Population:</strong>",
                population
            )
        ) %>% addLegend(
            title = "Total Population",
            pal = palpop,
            values = country$population,
            opacity = 1
        )
    
    return(map)
}

server <- shinyServer(function(input, output, session) {
    
    region_population <- GetFilePopulation()
    regions_towns <- GetTownsRegionsFile()
    extend_region_population <- ExtendPopulationFile(region_population, regions_towns)
    
    country2021 <- PrepareDataForMap(region_population, extend_region_population)
    output$map <- renderLeaflet(MapOfRussia(country2021))
    
    observe({
        country <- PrepareDataForMap(region_population, extend_region_population, input$inputYear)
        palpop <- colorBin("YlOrBr", country$population)
        
        proxy <- leafletProxy("map") %>% clearControls()
        
        proxy %>%
            addPolygons(
                data = country,
                color = ~ palpop(population),
                smoothFactor = 0.5,
                weight = 2,
                opacity = 1.0,
                fillOpacity = 1.0,
                highlightOptions = highlightOptions(
                    weight = 1,
                    color = "brown",
                    fillOpacity = 0.7,
                    bringToFront = TRUE
                ),
                label = paste(
                    "<strong>County:</strong>",
                    country$NAME_1,
                    "<br>",
                    "<strong>Total Population:</strong>",
                    country$population
                    
                ) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal",
                                 padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto"
                ),
                
                popup = ~ paste(
                    "<strong>County:</strong>",
                    NAME_1,
                    "<br>",
                    "<strong>Total Population:</strong>",
                    population
                    
                )
                
            ) %>%
            addLegend(
                title = paste("Total Population in", input$inputYear),
                pal = palpop,
                values = country$population,
                opacity = 1
            )

    })
    
    output$hist_average_number_of_population <- renderPlot(AverageNumberOfPopulation(region_population))
    output$comparison_boxplot <- renderPlot(ConfidenceIntervalForEachTown(region_population, input$town))
    
    updateSelectInput(session, "town", choices = sort(unique(region_population$Town)), selected = "Томск")
    updateSelectInput(session, "inputYear", choices = sort(unique(region_population$Year)), selected = "2021")
    
    output$text_for_plot <- renderText(
            "С первого взгляда мы видим тенденцию к росту численности Российского населения.
             Так же, поверхностно мы можем оценить предсказанные значения, обозначенные зеленой линией"
        )
    
})
