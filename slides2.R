## ----include = FALSE----------------------------------------------------------
#| label: libraries-for-participants
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(forcats)
library(colorspace)
library(patchwork)
library(broom)
library(ggbeeswarm)
library(ggmosaic)
library(nullabor)
library(gapminder)
library(ggthemes)
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::slice)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarise)


## ----include = FALSE----------------------------------------------------------
#| label: options-for-nice-slides
options(width = 200)
knitr::opts_chunk$set(
  fig.width = 3,
  fig.height = 3,
  fig.align = "center",
  dev.args = list(bg = 'transparent'),
  out.width = "100%",
  fig.retina = 3,
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  cache = FALSE
)


## ----include = FALSE----------------------------------------------------------
#| label: theme-for-nice-plots
theme_set(ggthemes::theme_gdocs(base_size = 14) +
  theme(plot.background = 
        element_rect(fill = 'transparent', colour = NA),
        axis.line.x = element_line(color = "black", 
                                   linewidth = 0.4),
        axis.line.y = element_line(color = "black", 
                                   linewidth = 0.4),
        panel.grid.major = element_line(color = "grey90"),
        axis.ticks = element_line(color = "black"),
        plot.title.position = "plot",
        plot.title = element_text(size = 14),
        panel.background  = 
          element_rect(fill = 'transparent', colour = "black"),
        legend.background = 
          element_rect(fill = 'transparent', colour = NA),
        legend.key        = 
          element_rect(fill = 'transparent', colour = NA)
  ) 
)


## -----------------------------------------------------------------------------
#| echo: false
#| eval: false
# # divergingx_hcl(palette="Zissou 1", n=10)
# # [1] "#3B99B1" "#40ABA5" "#6FB798" "#9FC095" "#C7C98A"
# # [6] "#EABE23" "#E8A419" "#E78802" "#EA6400" "#F5191C"
# # specplot(divergingx_hcl(palette="Zissou 1", n=10))


## -----------------------------------------------------------------------------
#| echo: false
plan <- tribble(~time, ~topic,
                "15", "Fixing the plot design",
                "10", "Guided exercises", 
                "20", "Styling and theming", 
                "15", "Is the pattern visible and real?", 
                "30", "Making YOUR plot better"
                )
knitr::kable(plan)


## -----------------------------------------------------------------------------
#| eval: false
#| echo: false
# # This code prepares a small subset of data
# library(learningtower)
# student_all <- load_student("all") # 3.5mill obs
# student_tv <- student_all |>
#   filter(country %in% c("CAN", "AUS", "IDN", "COL", "MEX", "NZL")) |>
#   select(year, country, math, television) |>
#   group_by(year, country, television) |>
#   summarise(math = mean(math, na.rm=TRUE)) |>
#   ungroup() |>
#   mutate(country = factor(country))
# save(student_tv, file="data/student_tv.rda")


## -----------------------------------------------------------------------------
#| label: student-tv
#| echo: false
#| fig-width: 6
#| fig-height: 6
load("data/student_tv.rda")
student_tv |>
  filter(year == 2022) |>
  filter(!is.na(television)) |>
  ggplot(aes(x=television, y=math, group=country)) +
    geom_line() +
    geom_point() +
    facet_wrap(~country, ncol=3) +
    ylab("average math") 


## -----------------------------------------------------------------------------
#| label: student-tv
#| eval: false
# load("data/student_tv.rda")
# student_tv |>
#   filter(year == 2022) |>
#   filter(!is.na(television)) |>
#   ggplot(aes(x=television, y=math, group=country)) +
#     geom_line() +
#     geom_point() +
#     facet_wrap(~country, ncol=3) +
#     ylab("average math")


## -----------------------------------------------------------------------------
#| fig-width: 6
#| fig-height: 5
#| code-line-numbers: "10"
s_tv_2012 <- student_tv |>
  filter(year == 2022) |>
  filter(!is.na(television)) 
ggplot(s_tv_2012, 
      aes(x=television, 
          y=math, 
          group=country)) +
    geom_line() +
    geom_point() +
    facet_wrap(~country, ncol=3, scale="free") +
    ylab("average math") 


## -----------------------------------------------------------------------------
#| fig-width: 6
#| fig-height: 5
#| code-line-numbers: "8,910"
slope <- function(y, x) {
  coef(lsfit(x, y))[2]
}
s_tv_trend <- s_tv_2012 |>
  mutate(tv_num = as.numeric(television)) |>
  group_by(country) |>
  summarise(s = slope(tv_num, math))
s_tv_2012 <- s_tv_2012 |>
  mutate(country = factor(country, 
    levels = s_tv_trend$country[order(s_tv_trend$s)]))
ggplot(s_tv_2012, 
      aes(x=television, 
          y=math, 
          group=country)) +
    geom_line() +
    geom_point() +
    facet_wrap(~country, ncol=3, scale="free") +
    ylab("average math") 


## -----------------------------------------------------------------------------
#| fig-width: 8
#| fig-height: 4
#| out-width: 100%
tb_aus_idn <- read_csv("data/TB_notifications_2023-08-21.csv") |>
  filter(iso3 %in% c("AUS", "IDN", "KOR")) |> 
  select(year, iso3, c_newinc) |>
  pivot_wider(names_from = iso3, values_from = c_newinc) |>
  mutate_at(vars(AUS:KOR), function(x) x/max(x, na.rm=TRUE)) |>
  pivot_longer(AUS:KOR, names_to = "iso3", 
    values_to = "rel_count")

tb_aus_idn |>
    ggplot(aes(x=year, y=rel_count, fill=iso3)) +
      geom_col(position = "dodge") +
  scale_fill_discrete_divergingx(palette = "Zissou 1") +
  xlab("") + ylab("Relative count") +
  theme(aspect.ratio = 0.5, 
        legend.title = element_blank())


## -----------------------------------------------------------------------------
#| fig-width: 5
#| fig-height: 8
#| out-width: 70%
#| code-line-numbers: "3,4"
tb_aus_idn |>
    ggplot(aes(x=year, y=rel_count)) +
      geom_point() +
      geom_smooth(se=F, colour = "#E87700") +
      xlab("") + ylab("Relative count") +
      facet_wrap(~iso3, ncol=1) +
      theme(aspect.ratio = 0.6)


## -----------------------------------------------------------------------------
#| code-fold: true
#| code-summary: "Some answers"
# 1: soften grid, resolve axis overlap
# 2: put line over points
# 3: remove repeats '000
# 4: aspect ratio=1 to read association
# 5: re-map variables


## -----------------------------------------------------------------------------
#| label: tb-tidy
#| code-summary: process data
tb <- read_csv("data/TB_notifications_2023-08-21.csv") |>
  filter(country == "Australia", year > 1996, year < 2013) |>
  select(year, contains("new_sp")) 
tb_tidy <- tb |>
  select(-new_sp, -new_sp_m04, -new_sp_m514, 
                  -new_sp_f04, -new_sp_f514) |> 
  pivot_longer(starts_with("new_sp"), 
    names_to = "sexage", 
    values_to = "count") |>
  mutate(sexage = str_remove(sexage, "new_sp_")) |>
  separate_wider_position(
    sexage,
    widths = c(sex = 1, age = 4),
    too_few = "align_start"
  ) |>
  filter(age != "u") |>
  mutate(age = fct_recode(age, "0-14" = "014",
                          "15-24" = "1524",
                          "15-24" = "1524",
                          "25-34" = "2534",
                          "35-44" = "3544",
                          "45-54" = "4554",
                          "55-64" = "5564",
                          "> 65" = "65"))


## -----------------------------------------------------------------------------
#| label: styling1
#| fig-width: 7
#| fig-height: 5
#| out-width: 70%
#| code-summary: plotting code
tb_tidy |> 
  filter(!(age %in% c("0-14", "unknown"))) |>
  ggplot(aes(x=year, 
           y=count, 
           colour=sex)) + 
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~age, ncol = 3) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  scale_x_continuous("year", 
    breaks = seq(1998, 2012, 1)) +
  theme(axis.text = element_text(size="10"),
        panel.grid.major = element_line(color="black")) 


## -----------------------------------------------------------------------------
#| label: styling2
#| fig-width: 7
#| fig-height: 5
#| out-width: 80%
#| code-summary: plotting code
tb_tidy |> 
  filter(!(age %in% c("0-14", "unknown"))) |>
  ggplot(aes(x=year, 
           y=count, 
           colour=sex)) + 
  geom_smooth(se=F) +
  geom_point() +
  facet_wrap(~age, ncol = 3) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  scale_x_continuous("year", 
    breaks = seq(1998, 2012, 2), 
    labels = c("98", "00", "02", "04", "06", "08", "10", "12")) +
  theme(axis.text = element_text(size="10",
                    color="grey80"),
        axis.title = element_text(size="10",
                    color="grey80"),
        panel.grid.major =   
           element_line(color="white"),
        panel.background =
           element_rect(fill="grey90", 
                        colour = "grey80")) 


## -----------------------------------------------------------------------------
#| label: styling3
#| fig-width: 7
#| fig-height: 5
#| out-width: 70%
#| code-summary: plotting code
gapminder |> 
  filter (year == 2007) |>
  ggplot(aes(x=lifeExp, 
             y=gdpPercap,
             label=country,
             colour=continent)) +
  geom_point() +
  scale_colour_discrete_divergingx(palette = "Zissou 1")


## -----------------------------------------------------------------------------
#| label: styling4
#| fig-width: 7
#| fig-height: 5
#| out-width: 80%
#| code-summary: plotting code
gapminder |> 
  filter (year == 2007) |>
  ggplot(aes(x=lifeExp, 
             y=gdpPercap,
             label=country)) +
  geom_point(colour = "#3B99B1") +
  scale_y_log10("gdpPercap ('000)",
                breaks = seq(0, 50000, 10000), 
                labels = seq(0, 50, 10)) +
  theme(aspect.ratio = 0.5)


## -----------------------------------------------------------------------------
#| label: styling5
#| fig-width: 7
#| fig-height: 5
#| out-width: 80%
#| code-summary: plotting code
gapminder |> 
  filter (year == 2007) |>
  ggplot(aes(x=as.numeric(country),
             y=gdpPercap,
             fill=lifeExp)) +
  geom_col() + xlab("country") +
  scale_fill_distiller(palette = "RdPu", trans="log10") +    
  scale_y_log10("gdpPercap ('000)",
                breaks = seq(0, 50000, 10000), 
                labels = seq(0, 50, 10)) +
  theme(aspect.ratio = 0.5)


## -----------------------------------------------------------------------------
#| label: page-of=text
#| fig-width: 6
#| fig-height: 3
#| out-width: 60%
#| echo: false
#| fig-cap: "Fig 1. TB incidence in Australia  1980-2021. Initially incidence dropped but it has been steadily climbing in the recent two decades. Note that, counts are not population adjusted."
tb_aus <- read_csv("data/TB_notifications_2023-08-21.csv") |>
  filter(iso3 == "AUS") |> 
  select(year, c_newinc) 

tb_aus_p <- tb_aus |>
    ggplot(aes(x=year, y=c_newinc)) +
      geom_point(colour="grey40", ) +
      geom_smooth(colour="black", 
                  se=FALSE, 
                  alpha=0.8,
                  span = 0.4) +
  xlab("year") + 
  scale_y_continuous("count ('000)", 
    breaks = seq(1000, 1600, 200), 
    labels=c("1.0", "1.2", "1.4", "1.6")) 
  
tb_aus_p + theme_grey() +
  theme(aspect.ratio = 0.5)


## -----------------------------------------------------------------------------
#| fig-width: 8
#| fig-height: 6
#| out-width: 80%
library(patchwork)
p1 <- tb_aus_p + theme_grey() +
  theme(aspect.ratio = 0.5) + ggtitle("default")
p2 <- tb_aus_p + theme_minimal() +
  theme(aspect.ratio = 0.5) + ggtitle("minimal")
p3 <- tb_aus_p + theme_tufte() +
  theme(aspect.ratio = 0.5) + ggtitle("tufte")
p4 <- tb_aus_p + theme_economist() +
  theme(aspect.ratio = 0.5) + ggtitle("economist")
p1 + p2 + p3 + p4 + plot_layout(ncol=2)


## -----------------------------------------------------------------------------
#| label: theme-for-nice-plots
#| code-fold: false
#| eval: false
# theme_set(ggthemes::theme_gdocs(base_size = 14) +
#   theme(plot.background =
#         element_rect(fill = 'transparent', colour = NA),
#         axis.line.x = element_line(color = "black",
#                                    linewidth = 0.4),
#         axis.line.y = element_line(color = "black",
#                                    linewidth = 0.4),
#         panel.grid.major = element_line(color = "grey90"),
#         axis.ticks = element_line(color = "black"),
#         plot.title.position = "plot",
#         plot.title = element_text(size = 14),
#         panel.background  =
#           element_rect(fill = 'transparent', colour = "black"),
#         legend.background =
#           element_rect(fill = 'transparent', colour = NA),
#         legend.key        =
#           element_rect(fill = 'transparent', colour = NA)
#   )
# )


## -----------------------------------------------------------------------------
#| fig-width: 7
#| fig-height: 5
tb_age <- tb_tidy |> 
  filter(!(age %in% c("0-14", "unknown"))) |>
  ggplot(aes(x = year, 
             y = count, 
             colour = age)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~sex, ncol = 2) +
  scale_x_continuous("year", 
    breaks = seq(1998, 2012, 2), 
    labels = c("98", "00", "02", "04", "06", "08", "10", "12")) +
  theme(axis.text = element_text(size="10")) 
tb_age  


## -----------------------------------------------------------------------------
#| fig-width: 7
#| fig-height: 5
tb_age +
  scale_color_discrete_divergingx(palette="Zissou 1") 


## -----------------------------------------------------------------------------
#| fig-width: 7
#| fig-height: 5
tb_age +
  scale_color_discrete_divergingx(palette="Geyser") 


## -----------------------------------------------------------------------------
#| fig-width: 7
#| fig-height: 5
tb_age +
  scale_color_discrete_sequential(palette="OrYel") 


## -----------------------------------------------------------------------------
#| fig-width: 7
#| fig-height: 5
clrs <- deutan(scales::hue_pal()(6))
tb_age + scale_colour_manual("", values = clrs)


## -----------------------------------------------------------------------------
#| fig-width: 7
#| fig-height: 5
clrs <- deutan(divergingx_hcl(6, "Zissou 1"))
tb_age +
  scale_colour_manual("", values = clrs)


## -----------------------------------------------------------------------------
#| fig-width: 7
#| fig-height: 5
clrs <- deutan(divergingx_hcl(6, "Geyser"))
tb_age +
  scale_colour_manual("", values = clrs)


## -----------------------------------------------------------------------------
#| fig-width: 7
#| fig-height: 5
clrs <- deutan(sequential_hcl(6, "OrYel"))
tb_age +
  scale_colour_manual("", values = clrs)


## -----------------------------------------------------------------------------
#| fig-width: 13
#| fig-height: 8
#| out-width: 100%
ov <- tb_tidy |>
  group_by(year) |>
  summarise(count = sum(count)) |>
  ggplot(aes(x=year, y=count)) +
    geom_col() +
    annotate("text", x=1996, y=320, label="A", size=8) +
    xlim(c(1996, 2013))
sex <- tb_tidy |>
  group_by(year, sex) |>
  summarise(count = sum(count)) |>
  ggplot(aes(x=year, weight=count, fill=sex)) +
    geom_bar(position="fill") +
    scale_fill_discrete_divergingx(palette = "TealRose",
      rev=TRUE) +
    ylab("proportion") +
    annotate("text", x=1996, y=0.95, label="B", size=8) +
    xlim(c(1996, 2013))
age <- tb_tidy |>
  group_by(year, age) |>
  summarise(count = sum(count)) |>
  ggplot(aes(x=year, weight=count, fill=age)) +
    geom_bar(position="fill") +
    scale_fill_discrete_sequential(palette = "Sunset") +
    ylab("proportion") +
    annotate("text", x=1996, y=0.95, label="C", size=8) +
    xlim(c(1996, 2013))

ov + sex/age + plot_layout(widths=c(2,1))


## -----------------------------------------------------------------------------
#| eval: false
# set.seed(1130)
# d <- tibble(x = runif(10000, -1, 1),
#             y = x + x^2 + rnorm(10000))
# p <- ggplot(d, aes(x=x, y=y)) +
#   geom_point(alpha=0.3) +
#   geom_smooth(colour="#006dae", se=F, linewidth=2)
# ggsave("images/test.pdf", p, width=1000, height=800, units="px")
# ggsave("images/test.png", p, width=1000, height=800, units="px")
# ggsave("images/test-low.png", p, width=250, height=200, units="px", dpi=75)


## -----------------------------------------------------------------------------
#| echo: false
library(tsibble)
library(naniar)
library(lubridate)
library(imputeTS)
library(purrr)
traralgon_data <- read_csv("data/traralgon_data.csv")
alphington_data <- read_csv("data/alphington_data.csv")
weather_data <- read_csv("data/weather_data.csv")

pollutant_data <- bind_rows(traralgon_data,alphington_data)
pollutant_data <- pollutant_data |>
  mutate(date_utc = round_date(date_utc, unit = "day")) |>
  group_by(location_id, location, parameter, date_utc, unit, lat, long, country) |>  
  summarise(value = mean(value, na.rm = TRUE), .groups = 'drop') 

pollutant_weather_data <- pollutant_data |> 
     mutate(date = as.Date(date_utc))|> 
     select(location, parameter, date, value) |> 
     #pivot_wider(names_from = parameter, values_from = value) |>
     left_join(weather_data |> 
     select(longitude, latitude, date, air_tmax,
            air_tmin, mslp, radiation, rainfall,
            location), by = c("location","date"))  |> 
                              ungroup()

pollutant_weather_data <- pollutant_weather_data |>
  as_tsibble(key = c("location","parameter"),
             index = "date")

gap_cnt <- pollutant_weather_data |> count_gaps()

pollutant_weather_data <- fill_gaps(pollutant_weather_data,
                                    .full=TRUE) 

gap_cnt_with_filter <- gap_cnt |> 
                            filter(.n > 2) |> 
                            bind_rows( #custom count, since the gap starts from the starting date of the data
                              tibble(
                                location = "Traralgon",
                                parameter = "so2",
                                .from = as_date("2022-04-01"),
                                .to = as_date("2022-04-20"),
                                .n = as.integer(20)  
                              )
                            )

is_excluded <- function(location, parameter, date) {
  any(gap_cnt_with_filter$location == location & 
      gap_cnt_with_filter$parameter == parameter & 
      date >= gap_cnt_with_filter$.from & 
      date <= gap_cnt_with_filter$.to)
}
pollutant_weather_data <- pollutant_weather_data |>
  mutate(exclude = pmap_lgl(list(location, parameter, date), is_excluded))

interpolate_vectorized <- function(column, exclude_condition) {
  if (sum(!is.na(column)) < 2) {
    column  # Return original if insufficient data for interpolation
  } else {
    column[!exclude_condition] <- na_interpolation(column[!exclude_condition])
    column
  }
}

pollutant_weather_data_cleaned <- pollutant_weather_data |>
  mutate(across(
    c(value:rainfall), ~ interpolate_vectorized(.x, exclude)
  )) |>
  select(-exclude)

pollutant_weather_data_cleaned <- pollutant_weather_data_cleaned |> 
  as_tibble() |> 
  pivot_wider(
    id_cols = c(location, date, longitude, latitude, air_tmax, air_tmin, mslp, radiation, rainfall),
    names_from = parameter,
    values_from = value
  )

poll_wea_for_o3 <- pollutant_weather_data_cleaned |> 
                    select(location, date, air_tmax, air_tmin, o3)


w1 <- poll_wea_for_o3 |> 
  ggplot(aes(x = air_tmin, 
             y = o3)) + 
    geom_point() + 
    geom_smooth(se = F) 

l1 <- ggplot(lineup(null_permute('air_tmin'), poll_wea_for_o3, n=12), 
       aes(air_tmin,o3)) +
  geom_point() +
  geom_smooth(se = F) +
  facet_wrap(~ .sample, ncol=4) +
  theme(axis.text = element_blank()) +
  xlab("") + ylab("")

w2 <- poll_wea_for_o3 |> 
  ggplot(aes(x = air_tmax, 
             y = o3)) + 
    geom_point() +
    geom_smooth(se = F) 

l2 <- ggplot(lineup(null_permute('air_tmax'), poll_wea_for_o3, n=12), 
       aes(air_tmax,o3)) +
  geom_point() +
  geom_smooth(se = F) +
  facet_wrap(~ .sample, ncol=4) +
  theme(axis.text = element_blank()) +
  xlab("") + ylab("")


## -----------------------------------------------------------------------------
#| echo: false
#| fig-width: 9
#| fig-height: 7
#| out-width: 80%
l1


## -----------------------------------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 3
#| out-width: 50%
w1


## -----------------------------------------------------------------------------
#| echo: false
#| fig-width: 9
#| fig-height: 7
#| out-width: 80%
l2


## -----------------------------------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 3
#| out-width: 50%
w2


## -----------------------------------------------------------------------------
#| echo: false
#| fig-width: 9
#| fig-height: 7
#| out-width: 80%
g07 <- gapminder |> 
  filter(year == 2007) 
set.seed(414)
ggplot(lineup(null_permute("lifeExp"), g07, n=12)) +
  geom_point(aes(x=lifeExp, 
                 y=gdpPercap), colour = "#3B99B1") +
  #scale_y_log10("gdpPercap ('000)",
  #              breaks = seq(0, 50000, 10000), 
  #              labels = seq(0, 50, 10)) +
  facet_wrap(~.sample, ncol=4) +
  theme(axis.text = element_blank()) +
  xlab("") + ylab("")


## -----------------------------------------------------------------------------
#| echo: false
#| fig-width: 9
#| fig-height: 7
#| out-width: 80%
set.seed(514)
ggplot(lineup(null_permute("lifeExp"), g07, n=12)) +
  geom_col(aes(x=as.numeric(country),
             y=gdpPercap,
             fill=lifeExp)) + 
  xlab("country") +
  scale_fill_distiller(palette = "RdPu", trans="log10") +    
  scale_y_log10("",
                breaks = seq(0, 50000, 10000), 
                labels = seq(0, 50, 10)) +
  facet_wrap(~.sample, ncol=4) +
  theme(axis.text = element_blank(),
        legend.position = "none") +
  xlab("") + ylab("")

