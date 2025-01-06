library(ggtext)
library(ggthemes)
library(extrafont)
library(systemfonts)

suppressMessages(loadfonts("all"))

base_family <- "Source Sans 3"
title_family <- "Source Sans 3"
label_color <- "#505050" #Matterhorn
annotation_color <- "#EBEBEB"#Whisper
title_color <- "#202020"#Nero
caption_color <- "#909090"#Suva Gry
main_color_1 <- "#002F6C"
main_color_2 <- "#BA2D42"


if (Sys.info()['sysname'] == 'Darwin') {
  sysfonts::font_add("Source Sans 3",
                     regular = "/Library/Fonts/SourceSans3-Regular.ttf",
                     "/Library/Fonts/SourceSans3-Bold.ttf",
                     "/Library/Fonts/SourceSans3-Italic.ttf",
                     "/Library/Fonts/SourceSans3-BoldItalic.ttf"
  )
}

# Theme USAID -------------------------------------------------------------

#' A USAID-Flavored Theme
#'
#' @description Applies a theme inspired by USAID color branding, 
#'
#' @details 
#' This function references globally defined variables in your environment:
#' \itemize{
#'   \item \code{title_family}
#'   \item \code{base_family}
#'   \item \code{main_color_1}
#'   \item \code{title_color}
#'   \item \code{label_color}
#'   \item \code{caption_color}
#' }
#' Make sure these are defined before using \code{theme_usaid_blue}.
#'
#' By default, the plot title is automatically converted to uppercase, 
#' and the word "USAID" in the caption is wrapped in HTML styling to display 
#' the letters in two different colors.
#'
#' @param rf Logical. If \code{TRUE}, displays a range frame via \code{geom_rangeframe}.
#' @param bl Logical. If \code{TRUE}, shows major y-axis grid lines.
#' @param caption Character. Text to display in the plot caption. Any occurrence 
#'   of "USAID" is automatically styled in two-tone color.
#' @param title Character. The plot title, automatically transformed to uppercase.
#' @param subtitle Character. The plot subtitle.

#'
#' @return A list of ggplot2 layers and theme specifications.
#' 
#' @export

theme_usaid_blue <- function(
    rf       = TRUE,
    bl       = FALSE,
    caption  = NULL,
    title    = NULL,
    subtitle = NULL,
    main_color_1 = "#002F6C",
    main_color_2 = "#BA2D42",
    ...
) {
  # If caption is not null, replace "USAID" with HTML-styled text
  if (!is.null(caption)) {
    caption <- str_replace_all(
      caption,
      "USAID",
      "<span style = 'color:#002F6C'>US<span style = color:#BA2D42>AID</span></span>"
    )
  }
  
  list(
    # Base Tufte theme
    theme_tufte(...),
    
    # Override default theme elements
    theme(
      plot.title = element_textbox_simple(
        margin = margin(5, 5, 5, 5),
        family = title_family,
        size   = rel(1.03),
        face   = "bold",
        color  = main_color_1
      ),
      plot.subtitle = element_textbox_simple(
        margin = margin(5, 5, 12, 5),
        color  = title_color,
        family = title_family
      ),
      strip.text = element_markdown(
        family = base_family,
        color  = label_color
      ),
      strip.text.y = element_markdown(
        family = base_family,
        color  = label_color
      ),
      plot.title.position = "plot",
      legend.text = element_text(
        family = base_family,
        color  = label_color
      ),
      plot.caption = element_textbox_simple(
        family = base_family,
        margin = margin(10, 5, 5, 5),
        color  = caption_color,
        halign = 1,
        size   = rel(0.6)
      ),
      legend.title = element_text(
        family = base_family,
        color  = title_color
      ),
      axis.title.y = element_markdown(
        color  = title_color,
        family = base_family,
        halign = 0.5,
        size   = rel(1)
      ),
      axis.title.x = element_markdown(
        color  = title_color,
        family = base_family,
        size   = rel(1)
      ),
      axis.text = element_markdown(
        color  = label_color,
        family = base_family,
        size   = rel(.8)
      ),
      legend.position = "bottom"
    ),
    
    # Add range frame if rf = TRUE
    if (rf) geom_rangeframe(color = title_color) else geom_rangeframe(sides = ""),
    
    # Conditionally add labs for caption, title, subtitle
    if (is.null(caption)) labs() else labs(caption = caption),
    if (is.null(title))   labs() else labs(title   = str_to_upper(title)),
    if (is.null(subtitle)) labs() else labs(subtitle = subtitle),
    
    # Conditionally add major grid lines if bl = TRUE
    if (isFALSE(bl)) {
      theme()
    } else {
      theme(
        panel.grid.major.y = element_line(
          linetype  = "solid",
          color     = "#D3D3D3",
          linewidth = 0.25
        )
      )
    }
  )
}

theme_usaid <- function(
    rf       = FALSE,
    bl       = FALSE,
    caption  = NULL,
    title    = NULL,
    subtitle = NULL,
    main_color_1 = "#002F6C",
    main_color_2 = "#BA2D42",
    ...
) {
  # If caption is not null, replace "USAID" with HTML-styled text
  if (!is.null(caption)) {
    caption <- str_replace_all(
      caption,
      "USAID",
      "<span style = 'color:#002F6C'>US<span style = color:#BA2D42>AID</span></span>"
    )
  }
  
  list(
    # Base Tufte theme
    theme_tufte(...),
    
    # Override default theme elements
    theme(
      plot.title = element_textbox_simple(
        margin = margin(5, 5, 5, 5),
        family = title_family,
        size   = rel(1),
        face   = "bold",
        color  = main_color_1
      ),
      plot.subtitle = element_textbox_simple(
        margin = margin(5, 5, 12, 5),
        color  = title_color,
        family = title_family
      ),
      strip.text = element_markdown(
        family = base_family,
        color  = label_color
      ),
      strip.text.y = element_markdown(
        family = base_family,
        color  = label_color
      ),
      plot.title.position = "plot",
      legend.text = element_text(
        family = base_family,
        color  = label_color
      ),
      plot.caption = element_textbox_simple(
        family = base_family,
        margin = margin(10, 5, 5, 5),
        color  = caption_color,
        halign = 1,
        size   = rel(0.5)
      ),
      legend.title = element_text(
        family = base_family,
        color  = title_color
      ),
      axis.title.y = element_markdown(
        color  = title_color,
        family = base_family,
        halign = 0.5,
        size   = rel(0.6)
      ),
      axis.title.x = element_markdown(
        color  = title_color,
        family = base_family,
        size   = rel(0.6)
      ),
      axis.text = element_text(
        color  = label_color,
        family = base_family
      ),
      legend.position = "bottom"
    ),
    
    # Add range frame if rf = TRUE
    if (rf) geom_rangeframe(color = title_color) else geom_rangeframe(sides = ""),
    
    # Conditionally add labs for caption, title, subtitle
    if (is.null(caption)) labs() else labs(caption = caption),
    if (is.null(title))   labs() else labs(title   = str_to_upper(title)),
    if (is.null(subtitle)) labs() else labs(subtitle = subtitle),
    
    # Conditionally add major grid lines if bl = TRUE
    if (isFALSE(bl)) {
      theme()
    } else {
      theme(
        panel.grid.major.y = element_line(
          linetype  = "solid",
          color     = "#D3D3D3",
          linewidth = 0.25
        )
      )
    }
  )
}


theme_usaid_void <- function(base_family = "Gill Sans MT",
                             rf = T,
                             main_color_1 = "#002F6C",
                             main_color_2 = "#BA2D42",
                             caption = NULL,
                             ...){

  if(!is.null(caption)) caption <- str_replace_all(caption, "USAID", "<span style = 'color:#002F6C'>US<span style = color:#BA2D42>AID</span></span>")
  list(
    theme_void(...),
    theme(plot.title = element_textbox_simple(margin = margin(5,5,10,5),
                                              family = base_family,
                                              face = "bold",
                                              color = main_color_1
    ),
    plot.subtitle = element_textbox_simple(margin = margin(5,5,5,5),
                                           color ="black",
                                           family = base_family),
    plot.title.position = "plot",
    strip.text = element_text(family = base_family),
    plot.caption = element_textbox_simple(family = base_family, margin = margin(5,5,5,5)),
    legend.title = element_markdown(family = base_family),
    legend.position = "bottom"),
    if(rf == T) geom_rangeframe(color = main_color_1) else geom_rangeframe(sides = ""),
    if(is.null(caption)) labs() else labs(caption = caption)
  )
}

theme_usaid_map <- function(base_family = "Gill Sans",
                             rf = T,
                             main_color_1 = "#002F6C",
                             main_color_2 = "#BA2D42",
                             caption = NULL,...){

  if(!is.null(caption)) caption <- str_replace_all(caption, "USAID", "<span style = 'color:#002F6C'>US<span style = color:#BA2D42>AID</span></span>")
  list(
    theme_map(...),
    theme(plot.title = element_textbox_simple(margin = margin(5,5,10,5),
                                              family = base_family,
                                              face = "bold",
                                              color = main_color_1
    ),
    plot.subtitle = element_textbox_simple(margin = margin(5,5,5,5),
                                           color ="black",
                                           family = base_family),
    plot.title.position = "plot",
    plot.caption = element_textbox_simple(family = base_family, margin = margin(5,5,5,5)),
    legend.title = element_markdown(family = base_family),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    strip.background = element_blank(),
    strip.text = element_markdown(family = base_family)),
    if(rf == T) geom_rangeframe(color = main_color_1) else geom_rangeframe(sides = ""),
    if(is.null(caption)) labs() else labs(caption = caption)
  )
}

gt_theme_usaid <- function (gt_object, ..., color = "lightgrey") 
{
  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in% 
              class(gt_object))
  gt_object %>% 
    #align left
    tab_style(style = cell_borders(sides = "all",
                                   weight = px(1), 
                                   color = "white"),
              locations = list(cells_body())) %>%
    tab_style(style = cell_borders(sides = "left", weight = px(2),
                                   color = "white"),
              locations = list(cells_body(columns = 1),
                               cells_column_labels(columns = 1), cells_stub())) %>%
    tab_style(style = cell_borders(sides = "left", weight = px(1),
                                   color = "white"),
              locations = list(cells_row_groups())) %>%
    tab_style(style = cell_borders(sides = "right", weight = px(2),
                                   color = "white"),
              locations = list(cells_body(columns = dplyr::last_col()),
                               cells_column_labels(columns = dplyr::last_col()),
                               cells_row_groups())) %>%
    opt_table_font(font = "Calibri") %>% 
    tab_options(heading.align = "left",
                heading.border.bottom.color = "white",
                column_labels.background.color = "white",
                column_labels.font.weight = "bold", 
                stub.background.color = "white",
                #stub.border.color = "grey", 
                grand_summary_row.border.color = "white",
                stub_row_group.border.color = "white",
                row_group.background.color = "grey95",
                row_group.border.top.color = "white", 
                row_group.border.bottom.color = "white",
                row_group.border.left.color = "white", 
                #row_group.border.right.color = "grey", 
                row_group.border.left.width = px(1), 
                row_group.border.right.width = px(1),
                column_labels.font.size = pct(95), 
                column_labels.border.top.style = "none", 
                column_labels.border.bottom.color = "grey", 
                column_labels.border.bottom.width = px(2), 
                table.border.left.color = "white", 
                row_group.font.size = pct(90),
                stub.font.size = pct(90),
                table.border.left.style = "solid", 
                table.border.right.style = "solid", 
                table.border.left.width = px(2),
                grand_summary_row.background.color = "grey90",
                # summary_row.background.color = "grey95",
                table.border.right.width = px(2), 
                table.border.right.color = "white",
                table.border.bottom.width = px(2), 
                table.border.bottom.color = "white", 
                table.border.top.width = px(2), 
                table.border.top.color = "white", 
                #table.font.names = "Calibri",
                #row.striping.background_color = color, 
                table_body.hlines.color = "white", 
                table_body.vlines.color = "white", 
                data_row.padding = px(1),table.font.names = "Roboto")
}






# ---- geom panel ----


geom_panel_lines <- function(
    color     = "#D3D3D3",      # Default color (light gray)
    linewidth = 0.25,           # Default line width
    linetype  = "solid",        # Default linetype (solid)
    origin    = "1970-01-01"    # For converting numeric -> Date if needed
) {
  structure(
    list(
      color     = color,
      linewidth = linewidth,
      linetype  = linetype,
      origin    = origin
    ),
    class = "geom_panel_lines"
  )
}

ggplot_add.geom_panel_lines <- function(object, plot, object_name) {
  
  # 1) Safely attempt to build the plot to get scale & data info
  pb <- tryCatch(
    ggplot_build(plot),
    error = function(e) {
      message(
        "geom_panel_lines: Could not build plot.\n",
        "Check your data or scale_x_date()/scale_x_datetime() usage.\n",
        "Returning the plot as-is. Error was:\n  ", e$message
      )
      return(plot)
    }
  )
  
  # If building failed, pb is not a ggplot_built object
  if (!inherits(pb, "ggplot_built")) {
    return(pb)  # Just return the original plot
  }
  
  # 2) Extract major y-breaks from the first panel
  panel_params <- pb$layout$panel_params[[1]]
  y_breaks     <- panel_params$y$breaks
  
  # 3) x_min from the scale's left boundary, x_max from the actual data
  x_min_num <- panel_params$x.range[1]
  layer_data <- pb$data[[1]]
  x_max_num <- max(layer_data$x, na.rm = TRUE)
  
  # For the y-dimension, we'll just let lines exist for any break within the scale
  y_min_num <- panel_params$y.range[1]
  y_max_num <- panel_params$y.range[2]
  
  # 4) Convert numeric x-range back to <Date> (if it's a date axis)
  x_min_date <- as.Date(x_min_num, origin = object$origin)
  x_max_date <- as.Date(x_max_num, origin = object$origin)
  
  # Only keep y-breaks that fall within the scale
  valid_breaks <- y_breaks[y_breaks >= y_min_num & y_breaks <= y_max_num]
  
  # Build a data frame for the horizontal line segments
  hlines <- data.frame(
    x    = x_min_date,
    xend = x_max_date,
    y    = valid_breaks,
    yend = valid_breaks
  )
  
  # 5) Add horizontal lines via geom_segment
  plot + geom_segment(
    data        = hlines,
    aes(x = x, xend = xend, y = y, yend = yend),
    color       = object$color,
    linewidth   = object$linewidth,
    linetype    = object$linetype,
    inherit.aes = FALSE
  )
}

# -------------------------------------------------------------------------


