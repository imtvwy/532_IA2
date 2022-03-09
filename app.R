library(dash)
library(dashHtmlComponents)
library(ggplot2)
library(plotly)
library(tidyverse)

app <-  Dash$new()

df <- read_csv('data/world-data-gapminder_raw.csv', show_col_types = FALSE)


marks <- list("1918" = "1918", 
                "1928" = "1928",
                "1938"="1938",
                "1948" = "1948",
                "1958" = "1958",
                "1968" = "1968",
                "1978" = "1978",
                "1988" = "1988",
                "1998" = "1998",
                "2008" = "2008",
                "2018" = "2018"
                )




plot <- function(year_range, filter) {
  if(filter=='') {
    df_by_year <- df %>%
      filter(year>=year_range[1] & year<year_range[2]) %>%
      group_by(year) %>%
      summarize(children_per_woman = mean(children_per_woman))
    p <- ggplot(df_by_year, 
          aes(y = children_per_woman,
              x = year)) +
          geom_line() +
          labs(y="Children per woman", x="Year") +
          ggtitle("Average Number of Children") +
          theme(plot.title = element_text(hjust = 0.5))
  } else {
    df_by_year <- df %>%
      filter(year>=year_range[1] & year<year_range[2]) %>%
      group_by(!!sym(filter), year) %>%
      summarize(children_per_woman = mean(children_per_woman))

    p = ggplot(df_by_year, 
            aes(y = children_per_woman,
                x = year)) +
            geom_line(aes(color=!!sym(filter))) +
            labs(y="Children per woman", x="Year") +
            ggtitle("Average Number of Children") +
            theme(plot.title = element_text(hjust = 0.5))
  }
  return(p)
}

# layout
app$layout(
  htmlDiv(
  list(
    #dccGraph(figure=ggplotly(plot()), id='plot_area'),
    dccGraph(id='plot_area'),
    htmlLabel('Zoom in years: '),
    dccRangeSlider(min=1918, max=2018, step=1-10, value=list(1918, 2018),  id='year_range_slider',
                marks=marks
    ),
    htmlLabel('See breakdown number by: '),
    dccDropdown(
      options=list(
      list(label='All', value=''),
      list(label='Income Group', value='income_group'),
      list(label='Region', value='region')),
    value='', clearable=FALSE, id='filter_dropdown')
    )))

# callback
app$callback(
    output('plot_area', 'figure'),
    list(
      input('year_range_slider', 'value'),
      input('filter_dropdown', 'value')
      ),
    function(year_range, filter) {
      return(ggplotly(plot(year_range, filter)))
    }
)


app$run_server(debug = T)

