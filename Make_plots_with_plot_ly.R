library(Ecdat)
library(plotly)
library(crosstalk)

targets_df2 <- targets_df2 |> pivot_wider(names_from = variable, values_from = observation)
targets_with_stable_mean <- targets_with_stable_mean |>  pivot_wider(names_from = variable, values_from = observation)

targets_df2$newdate <- strptime(as.character(targets_df2$datetime), "%Y-%m-%d")
targets_with_stable_mean$newdate <- strptime(as.character(targets_with_stable_mean$datetime), "%Y-%m-%d")

ggplot()+
  geom_point(data=targets_df2, aes(x=as.Date(newdate), y= co2flux_umolm2s_mean), colour = "red")+
  geom_point(data=targets_with_stable_mean, aes(x=as.Date(newdate), y= co2flux_umolm2s_mean), colour = "blue")

ggplot()+
  geom_point(data=targets_df2, aes(x=as.Date(newdate), y= ch4flux_umolm2s_mean), colour = "red")+
  geom_point(data=targets_with_stable_mean, aes(x=as.Date(newdate), y= ch4flux_umolm2s_mean), colour = "blue")

targets_with_stable_mean <- targets_with_stable_mean |> pivot_longer(cols = starts_with(c("ch4", "co2")), names_to = "variable", values_to = "observation")

plotly_play <- SharedData$new(targets_with_stable_mean,
                              key = ~variable, group = 'variable')


plotly_play |> plot_ly(x= ~datetime, y=~observation,
                       hoverinfo = "text",
                       text = ~observation) %>%
  highlight(selectize = TRUE)

scatterplot <- targets_with_stable_mean |> 
  plot_ly(x= ~datetime, y=~observation, color = ~variable,
          hoverinfo = "text",
          text = ~observation) %>%
  highlight(selectize = TRUE)|> 
  add_markers()

bscols(widths = c(3),
       filter_checkbox(id = "variable",
                       label = "Variable",
                       sharedData = targets_with_stable_mean,
                       group = ~variable),
       scatterplot)

################################################################
#PLAYING WITH PLOTLY
#Begin with a simple scatterplot
targets_with_stable_mean %>%
  plot_ly(x=~datetime, y=~observation) %>%
  add_markers()

#Customize the shape and size of markers
targets_with_stable_mean %>%
  plot_ly(x=~datetime, y=~observation, color = ~variable) %>%
  add_markers(marker = list(symbol = "diamond", size = 5, colors = 'orange'))

#Customize the shape and size of markers
targets_with_stable_mean %>%
  plot_ly(x=~datetime, y=~observation, symbol = ~variable) %>%
  add_markers(marker = list(size = 5))

#Add hovers and text displayed
targets_with_stable_mean %>%
  plot_ly(x=~datetime, y=~observation, text = ~variable) %>%
  add_markers() %>%
  layout(hoverlabel = list(font=list(size=10)))

#Customize the information on hovers and text displayed
targets_with_stable_mean %>%
  plot_ly(x=~datetime, y=~observation, hoverinfo = "text",
          text = ~paste("datetime:",datetime,"<br>",
                        "observation:",observation,"<br>",
                        "variable:",variable,"<br>")) %>%
  add_markers() %>%
  layout(hoverlabel = list(font=list(size=10)))

#Fit a regression line to the data and add it to the interactive plot
m <- lm(observation ~ datetime, data = targets_with_stable_mean)
targets_with_stable_mean %>% select(datetime, observation, variable) %>%
  na.omit() %>%
  plot_ly(x=~datetime, y=~observation, hoverinfo = "text",
          text = ~paste("datetime:",datetime,"<br>",
                        "observation:",observation,"<br>",
                        "variable:",variable,"<br>")) %>%
  add_markers(showlegend = FALSE) %>%
  add_lines(y=~fitted(m))

#############################################################




























































