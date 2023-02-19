library(tidyverse)
library(plotly)

education_vs_income_full <- read.csv("education_vs_income_full.csv")
plot_ly(data = education_vs_income_full) %>%
  add_trace(x = ~per_capita_income, 
            y = ~bachelor_degree, 
            mode = 'markers', 
            type = 'scatter',
            text = ~county,
            color = ~state,
            hovertemplate = paste(
              "<b>%{text}</b><br>",
              "Per Capita Income (2021): %{x:$,.0f}<br>",
              "Bachelor's Degree: %{y}%",
              "<extra></extra>"
            )) %>% add_lines(x = ~per_capita_income, y = ~lm(bachelor_degree ~ per_capita_income, data = education_vs_income_full)$fitted.values, name = 'lm', line = list(color = "red", dash = "dot")) %>% layout(xaxis = list(tickformat = "$,.0f", title = "Per Capita Income (USD)"), yaxis = list(range = c(0,80), tickformat = ".", title = "Bachelor's Degree %"), annotations = list(x = 0.8, y = 0.85, text = "worldvisualization.com", showarrow = F, opacity = 0.4, xref='paper', yref='paper', xanchor='right', yanchor='auto', xshift=0, yshift=0, font=list(size=13))) %>% layout(title = 'Per Capita Income vs. Education Level', dragmode = FALSE)
