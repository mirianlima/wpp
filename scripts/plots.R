# Load required libraries for plots
pacman::p_load(
  ggtext,
  ggridges,
  scales,
  ggcharts,
  patchwork,
  geomtextpath,
  tidytext,
  showtext,
  ggiraph
)

showtext_auto()

# Add Google Fonts 'Roboto' and 'Oswald'
font_add_google("Roboto")
font_add_google("Oswald")

#########################
###### PLOTS THEME ######
#########################

# Captions
caption_wpp <- "**Source**: United Nations, World Population Prospects (2022) <br> **Note**: Projections based on UN medium fertility scenario"
caption_owid <- "**Source**: Our World in Data (ourworldindata.org)<br> **Note**: Projections based on UN medium fertility scenario (2022), Historical data based on HYDE (2017) and Gapminder (2022)"

# OSAA custom theme
theme_osaa <- function(title = "Oswald", base_size = 14, dark_text = "#1A242F", x=NA) {
  
  mid_text <-  "#474F58"
  light_text <-  "#757B82"
  
  base_theme <- theme_minimal(base_size = base_size) +
    theme(
      text = element_text(
        colour = mid_text, 
        family = "Roboto", 
        lineheight = 1.1
      ),
      plot.title = element_markdown(
        colour = dark_text,
        family = title,
        size = rel(1.5),
        margin = margin(12, 0, 8, 0)
      ),
      plot.subtitle = element_textbox_simple(
        size = rel(1.2),
        margin = ggplot2::margin(4, 0, 8, 0)
      ),
      axis.text.y = element_text(colour = light_text, size = rel(1)),
      axis.title.y = element_text(size = base_size, margin = margin(0, 4, 0, 0), vjust = 0, hjust = 1, angle = 90),
      axis.text.x = element_text(colour = mid_text, size = rel(1)),
      axis.title.x = element_blank(),
      legend.position = "top",
      legend.justification = 1,
      panel.grid = element_line(colour = "#F3F4F5"),
      panel.grid.minor = element_blank(),
      plot.caption = element_markdown(size = rel(0.7), margin = margin(12, 0, 0, 0)),
      plot.margin = margin(0.25, 0.25, 0.25, 0.25,"cm"),
      strip.background = element_rect(fill = osaa_col$blue, color=NA),
      strip.text = element_text(color = "white")
    )
  
  xaxis_theme <- theme(axis.title.x = element_text(size = 14, 
                                                   margin = margin(4, 0, 0, 0), 
                                                   color = "#474F58",
                                                   lineheight = 1.1,
                                                   vjust = 0, hjust = 0.5)
  )
  
  if (!is.na(x)) {
    return(base_theme + xaxis_theme)
  } else {
    return(base_theme)
  }
}

# OSAA Colors
osaa_col <- list(
  dark_blue = '#18375f', 
  blue = '#009ad9', 
  orange = '#f58220',
  yellow = '#fbd749',
  red = '#ec2e07', 
  dark_red = '#7e1f0c',
  grey = '#666666',
  dark_grey = '#333333',
  light_grey = '#CCCCCC',
  na_value = "#6A686F"
)

# OSAA Palettes
osaa_pal <- list(
  base = c('#18375f', '#19669a', '#009ad9','#8395b9', '#56b182','#85e5b3','#666666','#CCCCCC', '#f58220', '#fbd749'),
  blue_red = c('#18375f', '#234c77', '#3f6189', '#687491', '#9f8384', '#f7a934', '#e87d22', '#cf5416', '#ac320e', '#7e1f0c'),
  blue = c('#18375f', '#1a4e7c', '#19669a', '#1380b9', '#009ad9'), 
  orange = c('#510000', '#862600', '#b74f00', '#ea791a', '#ffb555'),
  yellow = c('#f58220', '#f8982a', '#faae34', '#fbc23f', '#fbd749'),
  grey = c("#333333", "#666666", "#999999", "#CCCCCC", "#E6E6E6"),
  sex = c(
    female = '#19669a',
    male = '#009ad9'
  ),
  income = c(
    `High income` = '#56b182',
    `Low income`= '#ec2e07',        
    `Lower middle income`= '#f8982a',
    `Upper middle income`= '#009ad9'
  ),
  age_wa = c(  # working-age population
    `Children (0-14)` = '#f58220',          # Orange
    `Old-Age (65+)` = '#7e1f0c',            # Dark Red
    `Working-Age (15-64)` = '#009ad9'       # Blue
  ),
  age_labor = c( # premium working age population
    `Dependents (0-14)` = '#f58220',                 # Orange
    `Older Prime-age Workers (35-54)` = '#18375f',   # Dark Blue
    `Older Workers (55+)` = '#009ad9',               # Blue
    `Younger Prime-age Workers (25-34)` = '#56b182', # Green
    `Youth (15-24)` = '#fbd749'                      # Yellow
  ), 
  age_ed = c( # Education Levels
    `Other` = '#666666',                     # Grey
    `Primary (6-11)` = '#f58220',            # Orange
    `Secondary (12-17)` = '#fbd749',         # Yellow
    `Tertiary (18-23)` = '#56b182'           # Green
  ),
  regions = c(
    Africa = '#f58220',
    Asia = '#18375f',
    Europe = '#009ad9',
    `Latin America and the Caribbean` = '#56b182',
    `Northern America` = '#7e1f0c', ######## missing from below
    Oceania = '#666666',
    Americas = '#7e1f0c'
  ), 
  subregions = c(
    `Eastern Africa` = '#f58220',  # Orange
    `Middle Africa` = '#ffa040',   # Lighter Orange
    `Northern Africa` = '#fcc080', # Even Lighter Orange
    `Southern Africa` = '#fbd749', # Yellow
    `Western Africa` = '#fac000',  # Darker Yellow
    
    `Central Asia` = '#1a4e7c',    # Darker Blue
    `Eastern Asia` = '#18375f',    # Dark Blue
    `South-Eastern Asia` = '#009ad9', # Blue
    `Southern Asia` = '#1380b9',   # Lighter Blue
    `Western Asia` = '#1a66ff',    # Even Lighter Blue
    
    `Eastern Europe` = '#234c77',  # Darker Blue-Red
    `Northern Europe` = '#3f6189', # Medium Blue-Red
    `Southern Europe` = '#687491', # Light Blue-Red
    `Western Europe` = '#9f8384',  # Greyish Blue-Red
    
    `Caribbean` = '#56b182',       # Green
    `Central America` = '#85e5b3', # Lighter Green
    `South America` = '#6fc299',   # Darker Green
    `Northern America` = '#39795f',
    
    `Australia/New Zealand` = '#666666', # Grey
    `Melanesia` = '#999999',             # Lighter Grey
    `Micronesia` = '#CCCCCC',            # Even Lighter Grey
    `Polynesia` = '#333333'              # Dark Grey
  ),
  africa = c(
    `Eastern Africa` = '#f58220',  # Orange
    `Middle Africa` = '#3f6189',   # Medium Blue-Red
    `Northern Africa` = '#6fc299', # Light Green
    `Southern Africa` = '#fbd749', # Yellow
    `Western Africa` = '#7e1f0c'  # Darker Red
  ),
  afr_highlight = c(
    Africa = '#f58220',
    Asia = '#CCCCCC',
    Europe = '#CCCCCC',
    `Latin America and the Caribbean` = '#CCCCCC',
    `Northern America` = '#CCCCCC',
    Oceania = '#CCCCCC',
    Americas = '#CCCCCC'
  )
)

# Generate scale colors
scale_osaa <- function(aesthetic = "color", palette = "base", n = NULL, continuous = FALSE, .palettes = osaa_pal, ...) {
  # Number of colors needed
  n_colors <- if (!is.null(n)) n else length(.palettes[[palette]])
  
  # Generate a color function from the palette
  if (!continuous) {
    discrete_palette_func <- colorRampPalette(.palettes[[palette]])
    discrete_values <- discrete_palette_func(n_colors)
    scale_function <- if (aesthetic == "fill") scale_fill_manual else scale_color_manual
    return(scale_function(values = discrete_values, ...))
  } else {
    colors <- .palettes[[palette]]
    scale_function <- if (aesthetic == "fill") scale_fill_gradientn else scale_color_gradientn
    return(scale_function(colours = colors, ...))
  }
}

###################
###### PLOTS ######
###################

########################## 1. Long Run Population Growth (OWID)
world_long_run_owid <- owid |>
  group_by(year, source) |>
  summarise(pop = sum(pop), .groups = 'drop') |>
  ggplot(aes(year, pop)) +
  geom_line(data = ~filter(., source == "Historical"), color = osaa_col$orange) +
  geom_line(data = ~filter(., source == "Projection"), linetype = "dotted") +
  geom_text(data = ~filter(., year == 2100), # peak is actually 2086
            aes(label = "\nWorld\n(2100)"),
            size = 4,
            vjust = 0.3, hjust = 1.2) +
  scale_x_continuous(breaks = seq(-10000, 2100, 1000), limits = c(-10000, 2100)) +
  scale_y_continuous(
    labels = label_number(scale = 0.5, scale_cut = cut_short_scale(), accuracy = 1),
    n.breaks = 12
  ) +
  theme_osaa() +
  theme(panel.grid.major.x = element_blank()) +
  labs(
    y = "Population Total",
    title = "World population growth is a fairly recent phenomena",
    subtitle = "The surge in population growth, catalyzed by technological and medical advancements, propelled the global population size.",
    caption = caption_owid
  )

########################## 2. Population Size Evolution
world_pop_size <- wpp$world |> 
  ggplot(aes(time, t_population1july)) + 
  geom_line(data = ~filter(., time < 2021), color = osaa_col$orange) +
  geom_line(data = ~filter(., time >= 2021), linetype = "dotted") +
  geom_text(data = ~filter(., time == 2100), # peak is actually 2086
            aes(fontface = 2, label = label_number(
              scale = 1000, 
              accuracy = 0.1,
              scale_cut = cut_short_scale())(t_population1july)),
            color = osaa_col$red,
            size = 5,
            vjust = 1.5, hjust = 0.5) +
  scale_x_continuous(breaks = seq(2000, 2100, 10)) +
  scale_y_continuous(
    labels = label_number(scale = 1000, scale_cut = cut_short_scale(), accuracy = 0.1),
    n.breaks = 8
  ) +
  theme_osaa() +
  theme(panel.grid.major.x = element_blank()) +
  labs(
    y = "Population, Total (Jul01)",
    title = "World population will reach its <span style='color:#ec2e07'>peak</span> before the end of the century",
    subtitle = "The United Nations forecasts that by the end of the century, total population size will have surpassed 10 billion individuals.",
    caption = caption_wpp
  )

########################## 3. Fertility Rates
fertility_rates_decline <- wpp$world |>
  mutate(source = if_else(time < 2022, "Historical", "Projection")) |>
  ggplot() +
  geom_line(data = ~filter(., source == "Historical"), aes(time, tfr), color = osaa_col$orange) +
  geom_line(data = ~filter(., source == "Projection"), aes(time, tfr), linetype = "dotted") +
  geom_line(data = ~filter(., source == "Historical"), aes(time, pop_growth_rate), color = osaa_col$blue) +
  geom_line(data = ~filter(., source == "Projection"), aes(time, pop_growth_rate), linetype = "dotted") +
  annotate("text", x = 2025, y = 2.65, hjust = 0.2, color = osaa_col$orange, label = "World Total Fertility Rate") +
  annotate("text", x = 2025, y = 1.3, hjust = 0.2, color = osaa_col$blue, label = "World Population Growth Rate") +
  scale_x_continuous(breaks = seq(2000, 2100, 10), limits = c(2000, 2100)) +
  scale_y_continuous(breaks = seq(0, 3, 0.5), limits = c(0, 3)) +
  theme_osaa() +
  theme(panel.grid.major.x = element_blank()) +
  labs(
    y = "",
    title = "There is a worldwide decline in fertility rates",
    subtitle = "The declining fertility rates determine the population **growth rate**, which will also decline, even as overall population keeps growing.",
    caption = caption_wpp
  )

########################## 4. Africa Doubling Population
africa_double_2060 <- wpp$regions |>
  ggplot(aes(time, t_population1july, color = location, label = location)) +
  geom_vline(xintercept = c(2020, 2060), linetype = 'dotted', color = osaa_col$orange, alpha = 0.7) +
  geom_line() +
  geom_textpath(data = ~filter(., location == "Europe"), aes(label = location), vjust = 1.2, hjust = 0.65, size = 3) +
  geom_textpath(data = ~filter(., location != "Europe"), aes(label = location), vjust = -0.2, hjust = 1, size = 3) +
  geom_label(data = ~filter(., location == "Africa" & time %in% c(2020, 2060)), 
             aes(fontface = 2, label = label_number(scale = 1e+03, scale_cut = cut_short_scale(), accuracy = 0.1)(t_population1july)), 
             vjust = 1, hjust = -0.05, color = "white", fill = osaa_col$orange, size = 3.5) +
  geom_point(data = ~filter(., location == "Africa" & time %in% c(2020, 2060)), color = osaa_col$orange, size = 2) +
  theme_osaa() +
  scale_y_continuous(labels = label_number(scale = 1000, scale_cut = cut_short_scale(), accuracy = 0.1), n.breaks = 8) +
  scale_osaa("color", "regions") +
  scale_x_continuous(breaks = seq(2000, 2100, 10), limits = c(2000, 2100)) +
  guides(color = "none") +
  theme(panel.grid.major.x = element_blank()) +
  labs(
    y = "Population",
    title = "<span style='color:#f58220'>Africa</span>'s population is projected to more than double by 2060",
    subtitle = "While other regions will see varying rates of increase between 2020 and 2060, none experience such large expansion in population size",
    caption = caption_wpp
  )

########################## 5. Declining Fertility Convergence
africa_tfr_decline <- wpp$regions |>
  ggplot(aes(time, pop_growth_rate, color = location, label = location)) + 
  geom_hline(aes(yintercept = 0), linetype = 'dotted', color = osaa_col$light_grey) +
  geom_line() +
  geom_textpath(vjust = 0.01, hjust = 1, size = 3.5) +
  theme_osaa() +
  scale_x_continuous(breaks = seq(2000, 2100, 10), limits = c(2000, 2100)) +
  scale_y_continuous(
    labels = label_percent(scale = 1, accuracy = 0.1),
    n.breaks = 8
  ) +
  scale_osaa("color", "regions") +
  guides(color = "none") +
  labs(
    y = "Population growth rate (%)",
    title = "<span style='color:#f58220'>Africa</span> boasts spectacular population growth rates, but...",
    subtitle = "...It does not escape the declining trend, which is notable across all regions from right before the start of the 2020s",
    caption = caption_wpp
  )

########################## 6. Working-age Population Annual Change
wap_annual_change <- wpp$regions %>%
  arrange(location, time)  |> 
  group_by(location)  |> 
  mutate(
    year_increment = total_working_age - lag(total_working_age),
    year_increment = if_else(is.na(year_increment), 0, year_increment),
    group = ifelse(location=="Africa","Africa","Rest of The World")
  ) |> 
  group_by(group, time)  |> 
  summarise(total_increment = sum(year_increment), .groups = 'drop') |>
  ggplot(aes(time, total_increment, color=group, fill=group, label = group)) + 
  geom_vline(xintercept=2028, alpha=0.3, linetype="dashed") +
  geom_hline(yintercept=0, alpha=0.3) +
  geom_line() +
  geom_text(data = ~filter(., time == 2078), hjust = 1.1, vjust = 1) +   
  annotate("text", x = 2028, y = 4e+04, label = "Africa is expected to take over \nthe rest of the world by 2028", color = "black", hjust=-0.05, ) +
  scale_y_continuous(
    labels = label_number(scale = 1000, scale_cut = cut_short_scale(), accuracy = 0.1),
    n.breaks = 8
  ) +
  scale_x_continuous(breaks = seq(2000, 2100, 10), limits = c(2000.5,2100)) +
  scale_fill_manual(values=c('#f8982a','#009ad9')) + 
  scale_color_manual(values=c('#f8982a','#009ad9')) + 
  theme_osaa() +
  guides(fill="none", color="none") +
  labs(
    y = "Working-age Pop. (Annual Change)*",
    title= "<span style='color:#f58220'>Africa</span> is projected to contribute more to the global labor force increase <br> than the rest of the world combined",
    subtitle= "This largely explains why Africa’s youth is expected to become the main drive of economic transformation through innovation and entrepreneurship",
    caption = paste0("****Working-age population annual change =*** increment over a period of 1 year, i.e., the difference between the population at the end versus at the beginning of the period.<br>", caption_wpp)
  )

########################## 7. Regional Dependency Ratios
comp <- function(parent="Africa", legend=NA, year=2030){
  
  df <- wpp$agg_age1 |>
    filter(loc_type_id == 3, time == year, parent_name == parent) |>
    group_by(parent_name, location, time, age_wa) |>
    summarise(comp = sum(pop_total), .groups = 'drop') |>
    group_by(parent_name, location, time) |>
    mutate(
      pop = sum(comp),
      comp_p = round(comp / pop, 2)
    ) |>
    ungroup() 
  
  index <- df |> 
    filter(age_wa == "Children (0-14)") |> 
    select(location, comp_order = comp_p) |> 
    arrange(desc(comp_order)) |> 
    mutate(index = row_number()) |> 
    select(location, index)
  
  p <- df |> 
    left_join(index, by=join_by(location)) |> 
    ggplot(aes(fct_reorder(location, index, .desc = TRUE), comp, fill= fct_relevel(age_wa, "Children (0-14)", after = Inf))) +
    geom_col(position = "fill", alpha=.8) +
    scale_fill_manual(values = osaa_pal$age_wa) +
    scale_y_continuous(labels = label_percent(scale = 100)) +
    theme_osaa() +
    theme(plot.background = element_blank()) +
    coord_flip(clip = "off") +
    labs(fill="", y="", x="", title="", subtitle=paste0(parent," (",year,")"))
  
  if (parent == "Europe") {
    p <- p + 
      geom_richtext(
        aes( x = 5, y = 1, label = "<span style = 'color:#666666'>On the opposite spectrum, **<span style = 'color:#333333'>Europe</span>** shows<br> the highest **<span style = 'color:#7e1f0c'>old dependency ratios</span>**.</span>"), 
        hjust = "right", vjust = 0, size = 5, fill = NA, label.color = NA
      )
  }
  
  if(is.na(legend)){
    return(p + guides(fill = "none"))
  } else {
    return(p + 
             theme(
               plot.title = element_markdown(margin = margin(b = 15, unit = "pt")),
               legend.position = "bottom"
              ) +
             guides(fill = guide_legend(reverse = TRUE))
        )
  }
}

africa <- comp() + labs(
  title="By the end of the decade, Africa is expected to have much higher <br>  <span style='color:#f58220'>youth dependency ratios</span> than other regions.<br>"
) 

region_dep_ratio <- africa / comp("Asia") / comp("Latin America and the Caribbean") / comp("Europe", legend="yes") &
  plot_annotation(theme = theme(plot.background = element_rect(colour='#d5d5d5')))

########################## 8. Old-age VS child dependency ratios
old_child_dep_ratios <- wpp$regions |>
  select(location, time, dep_ratio_youth_1014, dep_ratio_old_age_65) |>
  pivot_longer(dep_ratio_youth_1014:dep_ratio_old_age_65, names_to="ratio") |>
  mutate(ratio = if_else(ratio == "dep_ratio_youth_1014", "Child", "Old-age")) |>
  ggplot(aes(time, value, color = ratio, label = ratio)) +
  geom_line(linetype = "dashed") +
  geom_line(data = ~filter(., location == "Africa"), linewidth = 1.5) +
  geom_text(data = ~filter(., time == 2099 & ratio == "Child" & location != "Africa"), hjust = 1.1, vjust = 1.3) +
  geom_text(data = ~filter(., time == 2099 & ratio == "Old-age" & location != "Africa"), hjust = 1.1, vjust = -0.3) +
  geom_text(data = ~filter(., time == 2099 & ratio == "Child" & location == "Africa"), hjust = 1.1, vjust = -1.3) +
  geom_text(data = ~filter(., time == 2100 & ratio == "Old-age" & location == "Africa"), hjust = 1.1, vjust = 3.2) +
  theme_osaa() +
  scale_y_continuous(labels = label_percent(scale = 1), n.breaks = 8) +
  scale_color_manual(values = c('#f8982a', '#009ad9')) + 
  guides(color = "none", label = "none") +
  labs(
    y = "Dependency Ratios",
    title = "<span style='color:#f58220'>Africa</span> is the only region for which the dependency ratios' trends won't reverse before <br>the end of the century",
    caption = caption_wpp
  ) +
  facet_wrap(~location)

########################## 9. School-age population evolution 
school_age_data <- wpp$agg_age1 |>
  filter(loc_type_id == 3, time >= 2020, parent_name %in% c("Africa", "Asia"), age_ed != "Other") |>
  group_by(parent_name, location, time, age_ed) |>
  summarise(pop = sum(pop_total), .groups = 'drop') |>
  mutate(location = fct_inorder(location))

school_age_pop <- school_age_data |>
  ggplot(aes(time, pop, fill = location)) +
  geom_area() +
  scale_fill_manual(values = osaa_pal$subregions) +
  scale_y_continuous(
    labels = label_number(scale = 1000, scale_cut = cut_short_scale(), accuracy = 1),
    n.breaks = 6
  ) +
  theme_osaa() +
  theme(
    legend.position = "right",
    strip.text.y = element_blank(),
    plot.subtitle = element_textbox_simple(margin = ggplot2::margin(10, 0, 20, 0))
  ) +
  labs(
    fill = "", y = "", 
    subtitle = "In the next 60 years, Africa is expected to experience tremendous growth in its school age population, while Asia presents the exact opposite trend",
    title = "School Age Population Growth Projection:  **<span style='color:#f58220'>Africa</span>** VS **<span style='color:#18375f'>Asia</span>**"
  ) +
  facet_grid(parent_name ~ fct_relevel(age_ed, "Younger Prime-age Workers (25-34)", after = Inf))

########################## 10. Africa being left behing
left_behind <- wpp$countries |>
  filter(time == 2000) |>
  select(region, location, income) |>
  add_count(income, name = "n_income") |>
  count(income, n_income, region) |>
  mutate(
    perc = round((n / n_income) * 100),
    label = paste0(n, "\n", "(", perc, "%)"),
    income = factor(income, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))
  ) |>
  ggplot(aes(x = reorder_within(region, n, income), y = n, label = label, fill = region)) +
  geom_col() + 
  geom_text(vjust = -0.2) +
  scale_y_continuous(limits = c(0, 47)) +
  scale_fill_manual(values = osaa_pal$afr_highlight) +
  scale_x_reordered() +
  facet_wrap(~income, scales = "free_x") +
  theme_osaa() +
  guides(fill = "none") +
  labs(
    x = "Region",
    y = "Count of countries",
    title = "If LMICs and LICs are being left behind, so is **<span style='color:#f58220'>Africa</span>**",
    subtitle = "**85%** of African countries fall within the Low Middle Income Country (LMIC) or Low Income Country (LIC) categories.",
    fill = NULL,
    caption = "**Source:** 2024 World Bank official classification income levels"
  )

########################## 11. How Population Momentum Works (3 PLOTS GROUP)

# Plot 1
dem_momentum1 <- wpp$regions |>
  filter(location == "Africa") |>
  ggplot(aes(time, tfr, label = paste(round(tfr, 1), " TFR"))) + 
  geom_line(color = '#f58220') +
  geom_vline(xintercept = c(2000, 2060), linetype = "dashed", alpha = .3) +
  geom_point(data = ~filter(., time %in% c(2000, 2060)), aes(time, tfr)) +
  geom_text(data = ~filter(., time %in% c(2000, 2060)), aes(time, tfr), hjust = -0.2, vjust = 0) +
  scale_x_continuous(breaks = seq(2000, 2100, 20), limits = c(2000, 2100)) +
  theme_osaa() +
  labs(
    y = "TFR",
    title = "**<span style='color:#f58220'>Africa's</span>** Total Fertility Rate (TFR) will halve by 2060...",
    subtitle = "In 2060, African women in the reproductive age bracket (15-49 years) will have on average half the number of children they did in 2000",
    caption = caption_wpp
  )

# Plot 2
label_format <- label_number(
  scale = 1000, 
  scale_cut = cut_short_scale(), 
  accuracy = 1
)

age_tfr_sum <- wpp$agg_age1 |>
  filter(between(age_grp, 15, 49), location == "Africa") |>
  group_by(time) |>
  summarise(total_fem = sum(pop_female), total_births = sum(births), .groups = 'drop') |>
  mutate(labels_fem = label_format(total_fem), labels_births = label_format(total_births))

dem_momentum2 <- age_tfr_sum |>
  ggplot(aes(time, total_fem, label = labels_fem)) + 
  geom_vline(xintercept = c(2000, 2060, 2080), linetype = "dashed", alpha = .3) +
  geom_label(data = ~filter(., time %in% c(2000, 2060, 2080)), aes(time, total_fem), hjust = 0.6, vjust = -0.3, fill = 'white', label.size = NA) +
  geom_line(color = '#f58220') +
  geom_point(data = ~filter(., time %in% c(2000, 2060, 2080)), aes(time, total_fem)) +
  scale_x_continuous(breaks = seq(2000, 2100, 20), limits = c(2000, 2100)) +
  scale_y_continuous(labels = label_format, n.breaks = 8) +
  theme_osaa() +
  labs(
    y = "Reproductive Age Females",
    title = "...while the number of **<span style='color:#f58220'>African</span>** reproductive age women continues to grow...",
    caption = caption_wpp
  )

# Plot 3
dem_momentum3 <- age_tfr_sum |>
  ggplot(aes(time, total_births, label = labels_births)) + 
  geom_vline(xintercept = c(2000, 2060, 2080), linetype = "dashed", alpha = .3) +
  geom_label(data = ~filter(., time %in% c(2000, 2060, 2080)), aes(time, total_births), hjust = 0.6, vjust = -0.3, fill = 'white', label.size = NA) +
  geom_line(color = '#f58220') +
  geom_point(data = ~filter(., time %in% c(2000, 2060, 2080)), aes(time, total_births)) +
  scale_x_continuous(breaks = seq(2000, 2100, 20), limits = c(2000, 2100)) +
  scale_y_continuous(labels = label_format, n.breaks = 12) +
  theme_osaa() +
  labs(
    y = "Number of Births",
    title = "...the consequence will be that births in **<span style='color:#f58220'>Africa</span>** are expected to remain high.",
    subtitle = "And this will in turn mean that only towards the end of the 2070s, is the decrease in TFR expected to take over the effect of the increasing trend in number of reproductive age females, resulting in overall diminishing the number of births",
    caption = caption_wpp
  )

########################## 12. TFR Cumulative Plots: Regions
cum_probs_region <- wpp$countries |>
  filter(!is.na(tfr), time >= 2020) |>
  group_by(region) |>
  summarise(cum_prob_tfr = mean(tfr <= 2), .groups = 'drop') # Proportion of TFR <= 2

## Cumulative Probability Plot: determine the percentile ranking of particular values.
cum_tfr_regions <- wpp$countries |>
  filter(!is.na(tfr), time >= 2020) |>
  ggplot(aes(x = tfr, color = region)) +
  geom_vline(aes(xintercept = 2), color = "#7e1f0c", linetype = "dashed", size = .5, alpha = .7) +
  geom_line(stat = "ecdf") +
  geom_label(data = cum_probs_region, aes(x = 2, y = cum_prob_tfr, label = paste(region, ":", percent(cum_prob_tfr))), hjust = -0.05, label.size = 0, label.padding = unit(0.04, "lines")) +
  labs(
    title = "What region percentage is at or below replacement level*?",
    subtitle = "TFR Cumulative Distribution across Regions (2020-2100)",
    x = "TFR",
    y = "Cumulative Probability",
    caption = "*Replacement level is equivalent to a TFR value of 2"
  ) +
  theme_osaa() +
  scale_y_continuous(breaks = seq(0, 1, .05), labels = label_percent()) +
  scale_color_manual(values = osaa_pal$regions) +
  theme(legend.position = "none")

########################## 13. TFR Cumulative Plots: Sub-Regions
cum_probs_subregion <- wpp$countries |>
  filter(!is.na(tfr), region == "Africa", time >= 2020) |>
  group_by(region_int) |>
  summarise(cum_prob_tfr = mean(tfr <= 2), .groups = 'drop') # Proportion of TFR <= 2

cum_tfr_subregions <- wpp$countries |>
  filter(!is.na(tfr), region == "Africa", time >= 2020) |>
  ggplot(aes(x = tfr, color = region_int)) +
  geom_vline(aes(xintercept = 2), color = "#7e1f0c", linetype = "dashed", size = .5, alpha = .7) +
  geom_line(stat = "ecdf") +
  geom_label(data = cum_probs_subregion, aes(x = 2, y = cum_prob_tfr, label = paste(region_int, ":", percent(cum_prob_tfr))), hjust = -0.05, label.size = 0, label.padding = unit(0.04, "lines")) +
  labs(
    title = "What subregion percentage is at or below replacement level*?",
    subtitle = "TFR Cumulative Distribution across African subregions (2020-2100)",
    x = "TFR",
    y = "Cumulative Probability",
    caption = "*Replacement level is equivalent to a TFR value of 2"
  ) +
  theme_osaa() +
  scale_y_continuous(breaks = seq(0, 1, .05), labels = label_percent()) +
  scale_color_manual(values = osaa_pal$africa) +
  theme(legend.position = "none")

########################## 14. Scaled charts (ggcharts)
scaled_data <- wpp$countries |>
  filter(time == 2030, region == "Africa") |>
  transmute(
    region,
    tfr,
    scaled_var = scale(tfr, scale = FALSE),
    location = ifelse(scaled_var > 0,
                      paste(location, " (", round(tfr, 1), ")"),
                      paste("(", round(tfr, 1), ") ", location))
  ) |>
  add_count(region, wt = mean(tfr), name = "mean_tfr")

# Create the lollipop chart
tfr_scaled_lollipop <- scaled_data |>
  diverging_lollipop_chart(
    x = location, 
    y = scaled_var,
    lollipop_colors = c("#006400", "#b32134"),
    text_color = c("#006400", "#b32134")
  ) +
  labs(y = "", x = "") +
  geom_text(
    aes(x = 20, y = -1.5, label = paste("Region Average (2023) =", round(mean_tfr, 1)))
  )

############################
###### APPENDIX PLOTS ######
############################

########################## 14. TFR distribution boxplot

tfr_regions_boxplots <- wpp$countries |> 
  filter(!location %in% c("China", "India"))  |>
  ggplot(aes(y = tfr, fill=region, color=region, x=fct_reorder(region, tfr, mean))) +
  geom_jitter(alpha=.3, color="#E6E6E6") +
  geom_boxplot(alpha=.3) +
  theme_osaa() +
  scale_fill_manual(values = osaa_pal$regions) +
  scale_color_manual(values = osaa_pal$regions) +
  guides(fill="none", color="none") +
  labs(
    title = "TFR distribution accross regions* (2020-2100)", 
    caption = "*China and India are excluded",
    y = "TFR", 
    x = ""
  )

########################## 15. Working-age population size champions VS TFR chanpions (2 PLOTS)

# Function to prepare data for plotting
n_vars <- function(n_max=10,year=2024){
  var_order <- c("tfr", "imr", "dep_ratio_youth_1014", "net_migrations", "total_working_age", "dep_ratio_old_age_65")
  
  wpp$countries |> 
    filter(time%in%c(year)) |> 
    pivot_longer(cols = c(tfr, imr, net_migrations, total_working_age, dep_ratio_youth_1014, dep_ratio_old_age_65), 
                 names_to = "variable", values_to = "value") |> 
    mutate(variable = factor(variable, levels = var_order)) |> 
    group_by(variable)  |> 
    slice_max(order_by = value, n = n_max) |>
    ungroup() |>
    mutate(location2 = reorder_within(location, value, variable))
}

plot_champions <- function(df, var){
  df |> 
    select(-time) |> 
    left_join(wpp$countries, by=join_by(location, region)) |> 
    ggplot(aes(time, total_working_age, fill=str_wrap(location,15))) +
    geom_area(alpha=.7) +
    scale_x_continuous(breaks=seq(2000,2100,10)) +
    scale_y_continuous(labels = label_number(scale = 1000, scale_cut = cut_short_scale(), accuracy = 1)) +
    theme_osaa() +
    theme(legend.position = "right") +
    scale_fill_manual(values = osaa_pal$base) +
    labs(fill = NULL, y = "Working Age Population", subtitle = paste("Working-age population evolution of top 5", var, "in 2024"))
}

# Prepare data
vars_data <- n_vars(5)

# PLOT 1: Working-age population size champions
twa_size_champions <- vars_data |>
  filter(variable == "total_working_age") |>
  plot_champions("working population size")

# PLOT 2: TFR champions
twa_tfr_champions <- vars_data |>
  filter(variable == "tfr") |>
  plot_champions("fertility rates")

########################## 16. Working-age Distribution ridgeplot: regions 

# Helper function to relevel factors
relevel_present <- function(factor_var, levels_order) {
  present_levels <- intersect(levels_order, levels(factor_var))
  fct_relevel(factor_var, present_levels)
}

# create ridge plots
ridge_plot <- function(df, var = region) {
  order_region <- c("Oceania", "Europe", "Americas", "Africa", "Asia")
  order_region_int <- c("Southern Africa", "Middle Africa", "Northern Africa", "Western Africa", "Eastern Africa")
  
  df <- df |>
    filter(time %in% seq(2020, 2100, 20), total_working_age > 100) |>
    mutate(
      region = relevel_present(region, order_region),
      region_int = relevel_present(region_int, order_region_int)
    ) |>
    group_by({{var}}, time) |>
    add_count({{var}}, wt = total_working_age, name = "weighted_count") |>
    mutate(
      formatted_count = label_number(scale = 1000, scale_cut = cut_short_scale(), accuracy = 1)(weighted_count),
      region_year = factor(paste({{var}}, " (", formatted_count, ")"))
    )
  
  ggplot(df, aes(
    x = total_working_age, 
    y = region_year,
    fill = {{var}},
    group = interaction(time, {{var}})
  )) +
    geom_density_ridges_gradient(
      scale = 3, 
      jittered_points = TRUE,
      position = position_points_jitter(width = 0.05, height = 0),
      point_shape = 21,
      point_size = 2,
      point_fill = "white",
      alpha = 0.8,
      rel_min_height = 0.01
    ) +
    labs(y = NULL, x = "Total Working Age Population", fill = NULL) +
    facet_grid(rows = vars(time), scales = "free", switch = "y") +
    theme_osaa() + 
    scale_fill_manual(values = osaa_pal$base) +
    scale_x_log10(labels = label_number(scale = 1000, scale_cut = cut_short_scale(), accuracy = 1)) +
    guides(fill = "none")
}

# Regions' ridgelines
wap_ridge_regions <- wpp$countries |>
  filter(!location %in% c("China", "India")) |>
  ridge_plot()

########################## 17. Working-age Distribution ridgeplot: subregions

# Subregions' Ridgelines
wap_ridge_subregions <- wpp$countries |> 
  filter(!location %in% c("China", "India"))  |> 
  filter(region=="Africa") |> 
  ridge_plot(var=region_int)

########################## 18. Indicators Country Rankings
indicator_rankings <- n_vars() |> 
  ggplot(aes(value, location2, fill=region)) +
  geom_col() +
  scale_y_reordered() +
  scale_fill_manual(values = osaa_pal$regions) +
  facet_wrap(~variable, scales = "free") +
  theme_osaa() +
  labs(y=NULL, fill=NULL)

########################## 19. Top N charts (ggcharts)
topn <- function(var = tfr, years = c(2030, 2060, 2100), n = 10, fill_var = income) {
  
  filtered_data <- wpp$countries |>
    filter(time %in% years) |>
    mutate(location = str_wrap(location, 10))
  
  filtered_data |>
    bar_chart(x = location, y = {{var}}, facet = time, top_n = n, fill = {{fill_var}}) +
    geom_text(aes(label = round({{var}}, 1)), hjust = -.15) +
    scale_fill_manual(values = osaa_pal$income) +
    labs(y = "", x = "", fill = "") +
    theme(legend.position = "top")
}

topn()
