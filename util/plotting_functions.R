library(dplyr)
library(stringr)
library(purrr)
library(plotly)

plot_emoji_chart <- function(df) {
    ax <- list(
        zeroline = FALSE,
        showline = TRUE,
        mirror = FALSE, "ticks",
        showgrid = FALSE,
        anchor = 'free',
        linecolor = toRGB('grey30'),
        linewidth = 4
    )
    df %>% mutate(emoji = str_replace_na(emoji, '')) %>%
        plot_ly() %>%
        add_trace(x = ~rank, y = ~tot, type = "bar", visible = TRUE,
                  text = ~emoji,
                  textposition = "outside",
                  textfont = list(size = 18),
                  customdata = ~value,  
                  hovertemplate = paste0('Rank: %{x}<br>',
                                         '<b>%{customdata}</b><br>',
                                         'Used %{y} Times<extra></extra>'),
                  marker = list(color = '99d8c9',
                                line = list(width = 2, color = '2ca25f'))) %>%
        # frame & hover layout
        layout(xaxis = ax, yaxis = ax,
               hoverlabel = list(font = list(size = 14))) %>%
        # layout: xaxis & yaxis
        layout(xaxis = list(position = 0, title = list(font = list(color = 'grey30')), 
                            tickvals = ~rank,
                            showticklabels = FALSE,
                            tickfont = list(color = 'grey30'),
                            tickangle = 60,
                            range = c(min(df$rank)-1, max(df$rank) + 1)
        ),
        yaxis = list(title = list(text = '<b>Times Used</b>', 
                                  standoff = 10,
                                  font = list(color = 'grey30')),
                     tickfont = list(color = 'grey30'),
                     range = c(0,max(df$tot) * 1.1)
        )
        ) %>%
        # layout: annotations
        layout(annotations = list(text = '<i>Out of a total 165,719 Tweets</i>',
                                  showarrow = FALSE,
                                  x = max(df$rank) + 0.5, xanchor = "right",
                                  y = max(df$tot), yanchor = "bottom", yshift = 0)
               )
}

plot_hashtag_chart <- function(df) {
    ax <- list(
        zeroline = FALSE,
        showline = TRUE,
        mirror = FALSE, "ticks",
        showgrid = FALSE,
        linecolor = toRGB('grey30'),
        linewidth = 4
    )
    df %>% 
        plot_ly() %>%
        add_trace(x = ~rank, y = ~tot, type = "bar", visible = TRUE,
                  customdata = ~rank,
                  hovertemplate = paste0('Rank: %{customdata}<br>',
                                         '<b>%{x}</b><br>',
                                         'Used %{y} Times<extra></extra>'),
                  marker = list(color = 'bcbddc',
                                line = list(width = 2, color = '756bb1'))) %>%
        # frame & hover layout
        layout(xaxis = ax, yaxis = ax,
               hoverlabel = list(font = list(size = 14))) %>%
        # title layout
        # layout(title = list(text = '<b>Most common hashtags</b><br>',
        #                    font = list(size = 20, color = 'grey30')
        # )
        # ) %>%
        # layout: xaxis % yaxis
        layout(xaxis = list(title = list(font = list(color = 'grey30')), 
                            tickvals = ~rank,
                            ticktext = ~value,
                            tickfont = list(color = 'grey30'),
                            tickangle = 60,
                            range = c(0, nrow(df)+1)
        ),
        yaxis = list(title = list(text = '<b>Times Used</b>', 
                                  standoff = 10,
                                  font = list(color = 'grey30')),
                     tickfont = list(color = 'grey30')
        )
        ) %>%
        # layout: annotations
        layout(annotations = list(text = '<i>Out of a total 165,719 Tweets</i>',
                                  showarrow = FALSE,
                                  x = max(df$rank) + 0.5, xanchor = "right",
                                  y = max(df$tot), yanchor = "bottom", yshift = 0)
        )
}

plot_sankey <- function(data, from_ = NULL, to_ = NULL) {
    if (is.null(from_)) from_ <- 1
    if (is.null(to_)) to_ <- nrow(data)
    d <- data[from_:to_, ]
    unique_descriptions <- unique(unlist(str_split(d$pair_, ' ')))
    # lookup table
    source_lookup <- seq_along(unique_descriptions) - 1
    names(source_lookup) <- unique_descriptions
    # lookup table
    target_lookup <- seq_along(unique_descriptions) + length(unique_descriptions) - 1
    names(target_lookup) <- unique_descriptions
    # joining to emojis and untagged desc
    unique_emoji_data <- tibble(tagged_desc = unique_descriptions) %>% 
        left_join(select(emoji_data, tagged_desc, description, emoji), by = 'tagged_desc') %>%
        # removing duplicates due to left join where two emojis have the same
        distinct(tagged_desc, .keep_all = T) %>%
        mutate(description = str_replace_all(description, '^flag$', 'country flag'))
        
    sankey_data <- d %>% 
            mutate(emoji1 = str_extract(pair_,'^jaugur_[\\w-]+'),
                   emoji2 = str_extract(pair_,'jaugur_[\\w-]+$'),
                   source_ = map(emoji1, function(x) source_lookup[[x]]) %>% unlist(),
                   target_ = map(emoji2, function(x) target_lookup[[x]]) %>% unlist(),
            )
    # create fig
    fig <- plot_ly(
        type = "sankey",
        orientation = "h",
        valueformat = ".0f",
        node = list(
            label = rep(unique_emoji_data$emoji, 2),
            customdata = rep(unique_emoji_data$description, 2),
            hovertemplate = '%{customdata}',
            color = rep('fc9272', length(unique_emoji_data$emoji) *2),
            pad = 15,
            thickness = 10,
            line = list(
                color = "de2d26",
                width = 0.5
            )
        ),
        link = list(
            source = sankey_data$source_,
            target = sankey_data$target_,
            value =  sankey_data$tot,
            hovertemplate = '"%{source.customdata}" & "%{target.customdata}"',
            color = rgb(254/255,224/255,210/255,0.8)
        )
    ) %>% 
        # add layout
        layout(
        #title = "Basic Sankey Diagram",
        font = list(
            size = 20
        )
    )
    
    fig
    
}
