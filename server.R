library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(Cairo)
library(fmsb)

shinyServer(function(input, output) {
  
  ## Reordering the dataset columns for the full dataset object
  music_table <- all_music[, c(22, 15, 14, 17, 23, 3, 2, 4, 7)]
  
  ## Reodering the dataset columns for the threshold dataset object
  median_stats <- median_stats[, c(1, 15, 19, 5, 3, 4, 8, 9, 10, 6, 13)]
  
  # plotly box and whisker plot with user input for the y-axis
  data_for_selected_genre <- reactive(all_music %>% filter(all_music$genre %in% input$genres))
  
  output$boxplot <- renderPlotly({
    plot_ly(data_for_selected_genre(), y = ~get(input$plot_var), color = ~song_genre, colors = "Set3", type = "box") %>% 
      layout(yaxis = list(title = input$plot_var))
  })
  
  # plotly density plot with user input for the axis variable
  #output$txt <- renderText(paste("You chose", input$genres))
  
  output$densityplot <- renderPlotly({
    print(ggplotly(ggplot(data = data_for_selected_genre(), aes_string(input$plot_var)) +
      geom_density(alpha = 0.6, aes(fill = genre, color = genre)) +
      scale_fill_brewer(palette = "Set3") +
      scale_color_brewer(palette = "Set3") +
      xlab(lab = input$plot_var) +
      theme(legend.title=element_blank()) ))
  #   
  #   #print(ggplotly(dens)) # couldn't directly create density plot in plotly, just converted ggplot object to plotly
   })
  
  # plotly 3d scatter plot, taking user input for the x and y-axis, keeping z-axis fixed on popularity
  output$threeDscatter <- renderPlotly({
    plot_ly(subset_all_music, x = ~get(input$scatter_var1), y = ~get(input$scatter_var2), z = ~popularity,
            color = ~genre, colors = "Set3", key = ~full_name, height = 650, marker = list(size = 4),
            text = ~paste('Artist:', artist_name, '<br>Song:', song_name, '<br>Popularity:', popularity)) %>%
      add_markers() %>%
      layout(scene = list(zaxis = list(title = 'Popularity'),
                          xaxis = list(title = input$scatter_var1),
                          yaxis = list(title = input$scatter_var2)))
    
  })
  
  # a text box to go with the 3d plot to store information once user clicks on a data point
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click to display the song information" else d
  })
  
  # the full dataset so the user can search through and look at all possible values if necessary
  output$table <- DT::renderDataTable({
    datatable(music_table, rownames = F)
  })
  
  # data table containing songs for selected artist
  songs_by_artist <- reactive(all_music %>% filter(all_music$artist_name == input$artist_name))
  output$artist_songs <- DT::renderDataTable({
    datatable(songs_by_artist(), rownames = F, options = list(scrollX = TRUE))
  })
  
  # data table containing the correlation between specified variable to popularity
  output$corr_tbl <- DT::renderDataTable({
    datatable(corr_df, rownames = T)
  })
  
  # radar plot to compare two songs
  song_1 <- reactive(input$radar_var1)
  song_2 <- reactive(input$radar_var2)
  
  song1_data = reactive(all_music %>% filter(song_name==song_1()) %>% select('danceability','acousticness','energy',
                'liveness','loudness','valence'))
  
  song2_data = reactive(all_music %>% filter(song_name==song_2()) %>% select('danceability','acousticness','energy',
                'liveness','loudness','valence'))
  song_data <- reactive(rbind(song1_data(), song2_data()))
  output$radarplot <- renderPlot(fmsb::radarchart(df=song_data(), lwd=2 ))
  # output$radarplot <- renderPlotly({plot_ly(
  #   type = 'scatterpolar',
  #   fill = 'toself'
  # ) %>%
  #     add_trace(
  #       r = all_music %>% filter(song_name==song_1()) %>% select('danceability','acousticness','energy',
  #                                   'liveness','loudness','valence'),
  #       theta = c('danceability','acousticness','energy',
  #                 'liveness','loudness','valence'),
  #       name = song_1()
  #     ) %>%
  #     add_trace(
  #       r = all_music %>% filter(song_name==song_2()) %>% select('danceability','acousticness','energy',
  #                                                          'liveness','loudness','valence'),
  #       theta = c('danceability','acousticness','energy',
  #                 'liveness','loudness','valence'),
  #       name = song_2()
  #     ) %>%
  #     layout(
  #       polar = list(
  #         radialaxis = list(
  #           visible = T,
  #           range = c(0,5)
  #         )
  #       )
  #     )})
  
  
}
)