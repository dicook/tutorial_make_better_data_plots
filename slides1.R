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
#| code-summary: load these libraries to get started
library(tidyverse)
library(colorspace)
library(patchwork)
library(broom)
library(palmerpenguins)
library(ggbeeswarm)
library(vcd)
library(nullabor)
library(MASS)
library(colorspace)
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::slice)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarise)


## -----------------------------------------------------------------------------
#| echo: false
plan <- tribble(~time, ~topic,
                "5", "Outline",
                "10", "Tidy data", 
                "15", "Grammar of graphics",
                "15", "Guided exercises", 
                "15", "Cognitive principles",
                "15", "Guided exercises", 
                "15", "Identifying poor elements", 
                "30", "BREAK")
knitr::kable(plan)


## -----------------------------------------------------------------------------
#| label: tb-data
tb <- read_csv("data/TB_notifications_2023-08-21.csv") |>
  filter(country == "Australia", year > 1996, year < 2013) |>
  select(year, contains("new_sp")) 
glimpse(tb)


## -----------------------------------------------------------------------------
#| label: tb-data
#| eval: false
#| class-source: code_block_short
#| classes: code_block_short

# tb <- read_csv("data/TB_notifications_2023-08-21.csv") |>
#   filter(country == "Australia", year > 1996, year < 2013) |>
#   select(year, contains("new_sp"))
# glimpse(tb)


## -----------------------------------------------------------------------------
#| label: tb-tidy
#| echo: false
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
tb_tidy |> slice_head(n=12)


## -----------------------------------------------------------------------------
#| label: tb-tidy
#| eval: false
#| class-source: code_block_short
#| classes: code_block_short

# tb_tidy <- tb |>
#   select(-new_sp, -new_sp_m04, -new_sp_m514,
#                   -new_sp_f04, -new_sp_f514) |>
#   pivot_longer(starts_with("new_sp"),
#     names_to = "sexage",
#     values_to = "count") |>
#   mutate(sexage = str_remove(sexage, "new_sp_")) |>
#   separate_wider_position(
#     sexage,
#     widths = c(sex = 1, age = 4),
#     too_few = "align_start"
#   ) |>
#   filter(age != "u") |>
#   mutate(age = fct_recode(age, "0-14" = "014",
#                           "15-24" = "1524",
#                           "15-24" = "1524",
#                           "25-34" = "2534",
#                           "35-44" = "3544",
#                           "45-54" = "4554",
#                           "55-64" = "5564",
#                           "> 65" = "65"))
# tb_tidy |> slice_head(n=12)


## -----------------------------------------------------------------------------
#| label: tb-plots
#| echo: true
#| eval: false
#| class-source: code_block_med
#| classes: code_block_med
#| code-line-numbers: "5,6,9,10,11"
#| code-fold: false
# tb_yr <- tb_tidy |>
#   group_by(year) |>
#   summarise(count = sum(count, na.rm=TRUE))
# gg1 <- ggplot(tb_yr,
#   aes(x=year, y=count)) +
#   geom_col() +
#   ylim(c(0, 350))
# gg2 <- ggplot(tb_yr,
#   aes(x=year, y=count)) +
#   geom_point() +
#   geom_smooth(se=F) +
#   ylim(c(0, 350))
# gg1 + gg2 + plot_layout(ncol=1)


## -----------------------------------------------------------------------------
#| label: tb-plots
#| echo: false
#| fig-width: 5
#| fig-height: 9
#| out-width: 50%
tb_yr <- tb_tidy |>
  group_by(year) |>
  summarise(count = sum(count, na.rm=TRUE)) 
gg1 <- ggplot(tb_yr, 
  aes(x=year, y=count)) +
  geom_col() +
  ylim(c(0, 350))
gg2 <- ggplot(tb_yr, 
  aes(x=year, y=count)) +
  geom_point() +
  geom_smooth(se=F) +
  ylim(c(0, 350))
gg1 + gg2 + plot_layout(ncol=1)


## -----------------------------------------------------------------------------
#| label: election
#| echo: false
#| out-width: 70%
library(nullabor)
data(electoral)
polls <- electoral$polls
ggplot(polls) +
  geom_boxplot(aes(x=Democrat, 
                   y=Margin)) +
  xlab("democrat") + 
  scale_y_continuous("margin (%)", 
    breaks=seq(0, 100, 20),
    limits=c(0,100)) +
  theme(aspect.ratio = 1.2, 
        panel.grid.major.x = element_blank())


## -----------------------------------------------------------------------------
#| label: election
#| eval: false
#| code-fold: false
#| code-line-numbers: "5,6"
# library(nullabor)
# data(electoral)
# polls <- electoral$polls
# ggplot(polls) +
#   geom_boxplot(aes(x=Democrat,
#                    y=Margin)) +
#   xlab("democrat") +
#   scale_y_continuous("margin (%)",
#     breaks=seq(0, 100, 20),
#     limits=c(0,100)) +
#   theme(aspect.ratio = 1.2,
#         panel.grid.major.x = element_blank())


## -----------------------------------------------------------------------------
#| fig-width: 5
#| fig-height: 3
#| echo: false
#| label: tb-age-trend
tb_tidy |>
  filter(age %in% c("45-54", "55-64"),
         sex == "f") |>
  ggplot() + 
    geom_smooth(aes(x=year, 
                  y=count,
                  colour=age), 
              se=F, 
              method="lm") +
    scale_color_discrete_divergingx(palette="Geyser") +
    scale_x_continuous("year", 
      breaks = seq(1998, 2012, 4), 
      labels = c("98", "02", "06", "10")) +
    theme(aspect.ratio = 0.8, 
      axis.text = element_text(size="10"))


## -----------------------------------------------------------------------------
#| label: tb-age-trend
#| eval: false
#| code-fold: false
#| code-line-numbers: "5,6,7,9"
#| class-source: code_block_short
#| classes: code_block_short
# tb_tidy |>
#   filter(age %in% c("45-54", "55-64"),
#          sex == "f") |>
#   ggplot() +
#     geom_smooth(aes(x=year,
#                   y=count,
#                   colour=age),
#               se=F,
#               method="lm") +
#     scale_color_discrete_divergingx(palette="Geyser") +
#     scale_x_continuous("year",
#       breaks = seq(1998, 2012, 4),
#       labels = c("98", "02", "06", "10")) +
#     theme(aspect.ratio = 0.8,
#       axis.text = element_text(size="10"))


## -----------------------------------------------------------------------------
#| fig-width: 5
#| fig-height: 3
#| label: tb-age-sex-count-2012
#| echo: false
#| out-width: 80%
tb_tidy |>
  filter(year == 2012) |>
  ggplot() + 
  geom_bar(aes(x=age, 
               weight=count,
               fill=sex),
           alpha=0.8) +
  scale_fill_discrete_divergingx(palette="Geyser") +
  theme_bw() +
  theme(aspect.ratio = 0.8, 
    axis.text = element_text(size="10"))


## -----------------------------------------------------------------------------
#| label: tb-age-sex-count-2012
#| eval: false
#| code-line-numbers: "4,5,6"
#| code-fold: false
# tb_tidy |>
#   filter(year == 2012) |>
#   ggplot() +
#   geom_bar(aes(x=age,
#                weight=count,
#                fill=sex),
#            alpha=0.8) +
#   scale_fill_discrete_divergingx(palette="Geyser") +
#   theme_bw() +
#   theme(aspect.ratio = 0.8,
#     axis.text = element_text(size="10"))


## -----------------------------------------------------------------------------
#| label: tb-age-sex-prop-2012
#| echo: false
#| fig-width: 5
#| fig-height: 3
#| out-width: 80%
tb_tidy |>
  filter(year == 2012) |>
  ggplot() + 
  geom_bar(aes(x=age, 
               weight=count,
               fill=sex),
           position="fill", alpha=0.8) +
  scale_fill_discrete_divergingx(palette="Geyser") +
  ylab("proportion") +
  theme_bw() +
  theme(aspect.ratio = 0.8, 
    axis.text = element_text(size="10"))


## -----------------------------------------------------------------------------
#| label: tb-age-sex-prop-2012
#| eval: false
#| code-line-numbers: "4,5,6,7"
#| code-fold: false
# tb_tidy |>
#   filter(year == 2012) |>
#   ggplot() +
#   geom_bar(aes(x=age,
#                weight=count,
#                fill=sex),
#            position="fill", alpha=0.8) +
#   scale_fill_discrete_divergingx(palette="Geyser") +
#   ylab("proportion") +
#   theme_bw() +
#   theme(aspect.ratio = 0.8,
#     axis.text = element_text(size="10"))


## -----------------------------------------------------------------------------
#| echo: false
tb_bad <- tb_tidy |>
  pivot_wider(names_from = "sex", values_from = "count")
tb_bad |>
  slice_head(n=10)


## -----------------------------------------------------------------------------
#| eval: false
#| code-fold: false
#| code-line-numbers: "3,4"
# tb_bad |>
#   ggplot() +
#     geom_point(aes(x=year, y=m), colour = "#A39000") +
#     geom_point(aes(x=year, y=f), colour = "#93B3FE")


## -----------------------------------------------------------------------------
#| echo: false
tb_tidy |>
  slice_head(n=10)


## -----------------------------------------------------------------------------
#| eval: false
#| code-fold: false
#| code-line-numbers: "3,4,5"
# tb_tidy |>
#   ggplot() +
#     geom_point(aes(x=year,
#                    y=count,
#                    colour=sex))


## -----------------------------------------------------------------------------
#| label: wdi
#| echo: false
wdi <- read_xlsx("data/P_Data_Extract_From_World_Development_Indicators.xlsx")
glimpse(wdi, width=60)


## -----------------------------------------------------------------------------
#| label: wdi-tidy
#| code-summary: tidying data
wdi_country <- wdi |>
  select(`Country Name`, `Country Code`) |>
  distinct()
wdi_indicator <- wdi |>
  select(`Series Name`, `Series Code`) |>
  distinct()
wdi_tidy <- wdi |>
  select(`Country Code`, `Series Code`, 
    `2004 [YR2004]`:`2022 [YR2022]`) |>
  pivot_longer(cols=`2004 [YR2004]`:`2022 [YR2022]`, 
    names_to="year", values_to="value") |>
  rename(country = `Country Code`,
         indicator = `Series Code`) |>
  mutate(value = as.numeric(value)) |>
  mutate(year = str_sub(year, 1, 4)) |>
  mutate(year = as.numeric(year))


## -----------------------------------------------------------------------------
#| label: wdi-access-to-clean-fuel
#| code-fold: false
#| eval: false
#| code-line-numbers: "4,5,6"
# wdi_tidy |>
#   filter(indicator == "EG.CFT.ACCS.ZS") |>
#   ggplot() +
#     geom_line(aes(x=year,
#                   y=value,
#                   group=country),
#               alpha = 0.5) +
#     xlab("") + ylab("Access to clean fuel") +
#     theme_minimal()


## -----------------------------------------------------------------------------
#| label: wdi-access-to-clean-fuel
#| echo: false
#| fig-width: 4
#| fig-height: 6
#| out-width: 70%
wdi_tidy |>
  filter(indicator == "EG.CFT.ACCS.ZS") |>
  ggplot() +
    geom_line(aes(x=year, 
                  y=value, 
                  group=country),
              alpha = 0.5) +
    xlab("") + ylab("Access to clean fuel") +
    theme_minimal()


## -----------------------------------------------------------------------------
#| label: tb-age-sex-prop-2012
#| echo: false
#| fig-width: 5
#| fig-height: 3
#| out-width: 80%
tb_tidy |>
  filter(year == 2012) |>
  ggplot() + 
  geom_bar(aes(x=age, 
               weight=count,
               fill=sex),
           position="fill", alpha=0.8) +
  scale_fill_discrete_divergingx(palette="Geyser") +
  ylab("proportion") +
  theme_bw() +
  theme(aspect.ratio = 0.8, 
    axis.text = element_text(size="10"))


## -----------------------------------------------------------------------------
#| label: tb-age-sex-mosaic-2012
#| eval: false
#| code-fold: false
#| code-line-numbers: "4,5,6"
# tb_tidy |>
#   filter(year == 2012) |>
#   ggplot() +
#   geom_mosaic(aes(x=age,
#                  weight=count,
#                  fill=sex)) +
#   scale_fill_discrete_divergingx(palette="Geyser") +
#   scale_y_continuous("proportion", breaks=seq(0,1,0.25)) +
#   #theme_bw() +
#   theme(aspect.ratio = 0.6,
#     axis.text = element_text(size="10"))


## -----------------------------------------------------------------------------
#| label: tb-age-sex-mosaic-2012
#| echo: false
#| fig-width: 5
#| fig-height: 3
#| out-width: 100%
tb_tidy |>
  filter(year == 2012) |>
  ggplot() + 
  geom_mosaic(aes(x=age, 
                 weight=count,
                 fill=sex)) +
  scale_fill_discrete_divergingx(palette="Geyser") +
  scale_y_continuous("proportion", breaks=seq(0,1,0.25)) +
  #theme_bw() +
  theme(aspect.ratio = 0.6, 
    axis.text = element_text(size="10"))


## -----------------------------------------------------------------------------
#| echo: false
vis_spacing <- 'style="padding-left:20px;"'
vis_spacing1 <- 'style="padding-left:10px;"'


## -----------------------------------------------------------------------------
#| label: proximity1
#| fig-width: 7
#| fig-height: 5
#| echo: false
tb_tidy |> 
  filter(!(age %in% c("0-14", "unknown"))) |>
  ggplot(aes(x=year, 
           y=count, 
           colour=sex)) + 
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~age, ncol = 3) +
  scale_color_discrete_divergingx(palette="Geyser") +
  scale_x_continuous("year", 
    breaks = seq(1998, 2012, 2), 
    labels = c("98", "00", "02", "04", "06", "08", "10", "12")) +
  theme(axis.text = element_text(size="10")) +
  ggtitle("Arrangement A")


## -----------------------------------------------------------------------------
#| label: proximity2
#| fig-width: 7
#| fig-height: 5
#| echo: false
tb_tidy |> 
  filter(!(age %in% c("0-14", "unknown"))) |>
  ggplot(aes(x = year, 
             y = count, 
             colour = age)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~sex, ncol = 2) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  scale_x_continuous("year", 
    breaks = seq(1998, 2012, 2), 
    labels = c("98", "00", "02", "04", "06", "08", "10", "12")) +
  theme(axis.text = element_text(size="10")) +
  ggtitle("Arrangement B")


## -----------------------------------------------------------------------------
#| label: cblind1
#| fig-width: 5
#| fig-height: 3.5
#| echo: false
tb_tidy |>
  filter(age %in% c("45-54", "55-64"),
         sex == "f") |>
  ggplot(mapping=aes(x=year, 
                 y=count)) + 
  geom_point() +
  geom_smooth(aes(colour=age), se=F, method="lm") +
  facet_wrap(~age, ncol = 2) +
  scale_color_discrete_divergingx(palette="Geyser") +
  scale_x_continuous("year", 
    breaks = seq(1998, 2012, 4), 
    labels = c("98", "02", "06", "10")) +
  theme(legend.position="none",
        axis.text = element_text(size="10"))
  


## -----------------------------------------------------------------------------
#| label: cblind2
#| fig-width: 3
#| fig-height: 3
#| out-width: 60%
#| echo: false
tb_tidy |>
  filter(age %in% c("45-54", "55-64"),
         sex == "f") |>
  ggplot(mapping=aes(x=year, 
                 y=count)) + 
  geom_smooth(aes(colour=age), se=F, method="lm") +
  scale_color_discrete_divergingx(palette="Geyser") +
  scale_x_continuous("year", 
    breaks = seq(1998, 2012, 4), 
    labels = c("98", "02", "06", "10")) +
  theme(legend.position="none",
        axis.text = element_text(size="10"))
  


## -----------------------------------------------------------------------------
#| label: olives-data
#| include: false
data(olives, package = "classifly")
df2 <- olives |>
  mutate(Region = factor(Region, labels = c("South", "Sardinia", "North")))


## -----------------------------------------------------------------------------
#| label: color-olives
#| echo: false
#| fig-width: 8
#| fig-height: 5
#| out-width: 80%
ggplot(olives, aes(palmitoleic, palmitic, color = Area)) +
  geom_point() +
  scale_color_discrete_divergingx(palette="Zissou 1") 


## -----------------------------------------------------------------------------
#| label: no-shadow
#| echo: false
#| fig-width: 7
#| fig-height: 7
#| out-width: 60%
ggplot(olives, aes(palmitoleic, palmitic, color = Area)) +
  geom_point() +
  facet_wrap(~Area) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  guides(color = FALSE) 


## -----------------------------------------------------------------------------
#| label: shadow
#| echo: false
#| fig-width: 7
#| fig-height: 7
#| out-width: 60%
ggplot(olives, aes(palmitoleic, palmitic)) +
  geom_point(data = dplyr::select(olives, -Area), color = "gray80") +
  geom_point(aes(color = Area), size=2) +
  facet_wrap(~Area) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  guides(color = FALSE)


## -----------------------------------------------------------------------------
#| label: shape
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 80%
set.seed(209)
df <- data.frame(
  x=runif(100), 
  y=runif(100), 
  cl=sample(c(rep("A", 1), rep("B", 99))))
ggplot(data=df, aes(x, y, shape=cl)) + 
  geom_point(size=3, alpha=0.8) +
  theme(legend.position="None", aspect.ratio=1)


## -----------------------------------------------------------------------------
#| label: colour
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 80%
set.seed(454)
df <- data.frame(
  x=runif(100), 
  y=runif(100), 
  cl=sample(c(rep("A", 1), rep("B", 99))))
ggplot(data=df, aes(x, y, colour=cl)) + 
  geom_point(size=3, alpha=0.8) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  theme(legend.position="None", aspect.ratio=1)


## -----------------------------------------------------------------------------
#| echo: false
#| fig-width: 6
#| fig-height: 1
V1 = tibble(x = 1:7, 
            native = factor(c("quoll", "emu", "roo", 
            "bilby", "quokka", "dingo", "numbat")))
ggplot(V1, aes(x=x, y=1, fill=native)) +
  geom_tile() +
  geom_text(aes(x=x, y=1, label=native)) +
  ggtitle("qualitative") + 
  theme_minimal() +
  theme(legend.position = "none", 
        panel.background =
                    element_rect(fill = 'transparent', colour = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        #axis.line = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"))


## -----------------------------------------------------------------------------
#| echo: false
#| fig-width: 6
#| fig-height: 1
V2 = tibble(x = 1:7, 
            fill = 1:7)
ggplot(V2, aes(x=x, y=1, fill=fill)) +
  geom_tile() +
  geom_text(aes(x=x, y=1, label=fill)) +
  ggtitle("sequential: emphasise high") + 
  scale_fill_continuous_sequential(palette = "PinkYl") +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.background =
                    element_rect(fill = 'transparent', colour = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        #axis.line = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"))


## -----------------------------------------------------------------------------
#| echo: false
#| fig-width: 6
#| fig-height: 1
V3 = tibble(x = 1:7, 
            fill = -3:3)
ggplot(V3, aes(x=x, y=1, fill=fill)) +
  geom_tile() +
  geom_text(aes(x=x, y=1, label=fill)) +
  ggtitle("diverging: emphasise high and low") + 
  scale_fill_continuous_divergingx(palette = "ArmyRose") +
  theme_minimal() +
  theme(legend.position = "none", 
        panel.background =
                    element_rect(fill = 'transparent', colour = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        #axis.line = element_line(colour = "white"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"))


## -----------------------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 5
library(fable)
library(tsibble)
tourism_melb <- tourism |>
  filter(Region == "Melbourne")
fit <- tourism_melb |>
  model(
    ets = ETS(Trips ~ trend("A"))
  )
fc <- fit |>
  forecast(h = "5 years")
fc |>
  filter(Purpose == "Business") |>
  autoplot(tourism_melb) +
  ggtitle("Melbourne Business Trips") +
  theme(aspect.ratio = 0.5)


## -----------------------------------------------------------------------------
#| fig-width: 5.5
#| fig-height: 5
#| out-width: 70%
library(vital)
library(viridis)
am <- aus_mortality |> 
  filter(State == "Victoria", 
         Sex != "total", 
         Year < 1980, 
         Age < 90) 

ggplot(am, aes(x=Age, y=Mortality, colour=Year, group=Year)) + 
    geom_line() +
    facet_wrap(~Sex, ncol=1) +
    scale_color_gradientn(colours = rainbow(10)) +
    scale_y_log10() + 
    theme(aspect.ratio = 0.5)


## -----------------------------------------------------------------------------
#| fig-width: 5.5
#| fig-height: 5
#| out-width: 70%
ggplot(am, aes(x=Age, y=Mortality, colour=Year, group=Year)) + 
    geom_line() +
    facet_wrap(~Sex, ncol=1) +
    scale_colour_gradientn(colors = viridis_pal(option = "turbo")(10)[10:1]) +
    scale_y_log10() + 
    theme(aspect.ratio = 0.5)


## -----------------------------------------------------------------------------
#| fig-width: 5.5
#| fig-height: 5
#| out-width: 70%

ggplot(am, aes(x=Age, y=Mortality, colour=Year, group=Year)) + 
    geom_line() +
    facet_wrap(~Sex, ncol=1) +
    scale_color_gradientn(colours = deutan(rainbow(10))) +
    scale_y_log10() + 
    theme(aspect.ratio = 0.5)


## -----------------------------------------------------------------------------
#| fig-width: 5.5
#| fig-height: 5
#| out-width: 70%
ggplot(am, aes(x=Age, y=Mortality, colour=Year, group=Year)) + 
    geom_line() +
    facet_wrap(~Sex, ncol=1) +
    scale_colour_gradientn(colors = deutan(viridis_pal(option = "turbo")(10)[10:1])) +
    scale_y_log10() + 
    theme(aspect.ratio = 0.5)


## -----------------------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 6
#| out-width: 80%
ggplot(as_tibble(Titanic), aes(x=interaction(Sex, Age),
                               y=interaction(Class, Survived), 
                               fill=n)) +
  geom_tile() +
  xlab("Sex, Age") +
  ylab("Class, Survived") +
  scale_fill_continuous_sequential(palette = "Terrain")


## -----------------------------------------------------------------------------
#| fig-width: 8
#| fig-height: 6
#| out-width: 80%
#| code-line-numbers: "8,9,10"
ggplot(as_tibble(Titanic), 
       aes(x=interaction(Sex, Age),
           y=interaction(Class, Survived), 
           fill=n)) +
  geom_tile() +
  xlab("Sex, Age") +
  ylab("Class, Survived") +
  scale_fill_continuous_sequential(
    palette = "Terrain", 
    trans="sqrt")


## -----------------------------------------------------------------------------
#| echo: false
#| eval: false
# # remotes::install_github("kevinwang09/learningtower")
# library(learningtower)
# student_data_2018 <- load_student(2018)
# student_means <- student_data_2018 |>
#   group_by(country) |>
#   summarise(math = mean(math, na.rm=TRUE),
#             read = mean(read, na.rm=TRUE),
#             science = mean(science, na.rm=TRUE))
# save(student_data_2018,
#   file="data/student_data_2018.rda")
# save(student_means, file="data/student_means.rda")
# 
# # Compute differences and bootstrap
# student2018_stats <- student_data_2018 %>%
#   group_by(country) %>%
#   summarise(mathgap=mean(math[gender=="male"],
#                            na.rm=TRUE)-
#                     mean(math[gender=="female"],
#                            na.rm=TRUE),
#             wmathgap=weighted.mean(
#                     math[gender=="male"],
#                       w=stu_wgt[gender=="male"],
#                         na.rm=T)-
#                      weighted.mean(
#                     math[gender=="female"],
#                       w=stu_wgt[gender=="female"],
#                         na.rm=T),
#             readgap=mean(read[gender=="male"],
#                            na.rm=TRUE)-
#                     mean(read[gender=="female"],
#                            na.rm=TRUE),
#             wreadgap=weighted.mean(
#                     read[gender=="male"],
#                       w=stu_wgt[gender=="male"],
#                         na.rm=T)-
#                      weighted.mean(
#                     read[gender=="female"],
#                       w=stu_wgt[gender=="female"],
#                         na.rm=T))
# save(student2018_stats, file="data/student2018_stats.rda")
# 
# library(boot)
# cimathfn <- function(d, i) {
#   x <- d[i,]
#   if (nrow(x) == 0) {
#     ci <- 0
#   }
#   else {
#     ci <- weighted.mean(x$math[x$gender=="male"],
#        w=x$stu_wgt[x$gender=="male"], na.rm=T)-
#      weighted.mean(x$math[x$gender=="female"],
#         w=x$stu_wgt[x$gender=="female"], na.rm=T)
#   }
#   ci
# }
# cireadfn <- function(d, i) {
#   x <- d[i,]
#   if (nrow(x) == 0) {
#     ci <- 0
#   }
#   else {
#     ci <- weighted.mean(x$read[x$gender=="male"],
#        w=x$stu_wgt[x$gender=="male"], na.rm=T)-
#      weighted.mean(x$read[x$gender=="female"],
#         w=x$stu_wgt[x$gender=="female"], na.rm=T)
#   }
#   ci
# }
# bootmathfn <- function(d) {
#   if (nrow(d) == 0) {
#     ci <- c(0, 0)
#   }
#   else {
#     r <- boot(d, statistic=cimathfn, R=200)
#     l <- sort(r$t)[5]
#     u <- sort(r$t)[195]
#     ci <- c(l, u)
#   }
#   return(ci)
# }
# bootreadfn <- function(d) {
#   if (nrow(d) == 0) {
#     ci <- c(0, 0)
#   }
#   else {
#     r <- boot(d, statistic=cireadfn, R=200)
#     l <- sort(r$t)[5]
#     u <- sort(r$t)[195]
#     ci <- c(l, u)
#   }
#   return(ci)
# }
# math_results <- student_data_2018 %>%
#   split(.$country) %>%
#   purrr::map(bootmathfn)
# cnt <- names(math_results)
# math_results_tb <- tibble(country = rep(cnt, rep(2, length(cnt))),
#             ci = rep(c("l", "u"), length(cnt)),
#             value=unlist(math_results))
# math_results_tb <- math_results_tb |>
#   pivot_wider(names_from = ci, values_from = value) |>
#   filter(!(l == 0 & u == 0))
# read_results <- student_data_2018 %>%
#   split(.$country) %>%
#   purrr::map(bootreadfn)
# cnt <- names(read_results)
# read_results_tb <- tibble(country = rep(cnt, rep(2, length(cnt))),
#             ci = rep(c("l", "u"), length(cnt)),
#             value=unlist(read_results))
# read_results_tb <- read_results_tb |>
#   pivot_wider(names_from = ci, values_from = value) |>
#   filter(!(l == 0 & u == 0))
# save(math_results_tb,
#   file="data/math_results_tb.rda")
# save(read_results_tb,
#   file="data/read_results_tb.rda")


## -----------------------------------------------------------------------------
#| fig-height: 6
#| fig-width: 4
#| out-width: 70%
load("data/student_means.rda")
student_means_sub <- student_means |>
  filter(country %in% c("SGP", "KOR", "POL", "DEU", "NOR", "IRL", "GBR", "IDN", "AUS", "NZL", "USA", "TUR", "PHL", "MAR", "URY", "CHL", "COL", "CAN"))
ggplot(student_means_sub, aes(x=country, y=math)) + 
  geom_point(colour="#8ACE00", size=4) + 
  coord_flip() +
  xlab("") +
  theme(aspect.ratio = 2)


## -----------------------------------------------------------------------------
#| fig-height: 6
#| fig-width: 4
#| out-width: 70%
ggplot(student_means_sub, aes(x=country, y=math)) + 
  geom_point(colour="#8ACE00", size=4) + 
  coord_flip() +
  xlab("") +
  ylim(c(0, 1000)) +
  theme(aspect.ratio = 2)


## -----------------------------------------------------------------------------
#| fig-height: 6
#| fig-width: 4
#| out-width: 70%
#| code-line-numbers: "2,5"
ggplot(student_means_sub, 
       aes(x=fct_reorder(country, math), 
           y=math)) + 
  geom_point(colour="#8ACE00", size=4) + 
  coord_flip() +
  xlab("") +
  ylim(c(0, 1000)) +
  theme(aspect.ratio = 2)


## -----------------------------------------------------------------------------
#| label: trade-data
#| include: false
#| echo: false
data(EastIndiesTrade, package = "GDAdata")
skimr::skim(EastIndiesTrade)


## -----------------------------------------------------------------------------
#| label: trade-plot1
#| echo: false
#| fig-height: 4
#| fig-width: 6
#| out-width: 80%
ggplot(EastIndiesTrade, aes(Year, Exports)) +
  geom_line(color = "#008A25", size = 2) +
  geom_line(aes(Year, Imports), color = "#e6005c", size = 2) +
  geom_ribbon(aes(ymin = Exports, ymax = Imports), fill = "gray") +
  labs(y = "<span style='color:#008A25'>Export</span>/<span style='color:#e6005c'>Import</span>") +
  theme(aspect.ratio=0.7, axis.title.y = ggtext::element_markdown())


## -----------------------------------------------------------------------------
#| label: trade-plot2
#| echo: false
#| fig-height: 4
#| fig-width: 6
#| out-width: 80%
ggplot(EastIndiesTrade, aes(Year, Imports - Exports)) +
  geom_line(size = 2) +
  theme(aspect.ratio=0.7)


## -----------------------------------------------------------------------------
#| fig-width: 10
#| fig-height: 4
#| code-fold: true
#| label: anorexia-calculate
data(anorexia, package="MASS")
ggplot(data=anorexia, 
  aes(x=Prewt, 
      y=Postwt, 
	    colour=Treat)) + 
  coord_equal() +
  xlim(c(70, 110)) + 
  ylim(c(70, 110)) +
  xlab("Pre-treatment weight (lbs)") +  
  ylab("Post-treatment weight (lbs)") +
  geom_abline(intercept=0, slope=1,  
    colour="grey80", linewidth=1.25) + 
  geom_density2d() + 
  geom_point(size=3) +
  facet_grid(.~Treat) +
  theme(legend.position = "none")


## -----------------------------------------------------------------------------
#| fig-width: 10
#| fig-height: 4
#| code-fold: true
#| label: anorexia-reorganise
#| code-line-numbers: "3"
ggplot(data=anorexia, 
  aes(x=Prewt, colour=Treat,
    y=(Postwt-Prewt)/Prewt*100)) + 
  xlab("Pre-treatment weight (lbs)") +  
  ylab("Percent increase in weight") +
  geom_hline(yintercept=0, linewidth=1.25, 
    colour="grey80") + 
  geom_point(size=3) +   
  facet_grid(.~Treat) +
  theme(aspect.ratio=1, legend.position = "none")


## -----------------------------------------------------------------------------
#| fig-width: 4
#| fig-height: 7
#| out-width: 100%
#| echo: false
#| label: anorexia-wrong-aspect-ratio
ggplot(data=anorexia, 
 aes(x=Prewt, y=Postwt, 
	colour=Treat)) + 
 xlim(c(70, 110)) + ylim(c(70, 110)) +
 xlab("Pre-treatment weight (lbs)") +  
 ylab("Post-treatment weight (lbs)") +
 geom_abline(intercept=0, slope=1,  
   colour="grey80", linewidth=1.25) + 
 geom_density2d() + 
 geom_point(size=3) +
 facet_wrap(~Treat, ncol=1) +
 theme(legend.position = "none",
       aspect.ratio = 0.5) #exaggerated


## -----------------------------------------------------------------------------
#| label: aspect-ratio
#| fig-width: 6
#| fig-height: 8
#| out-width: 80%
#| echo: false
tb_tidy |> 
  filter(!(age %in% c("0-14", "unknown"))) |>
  ggplot(aes(x=year, 
           y=count, 
           colour=sex)) + 
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~age, ncol = 1) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  scale_x_continuous("year", 
    breaks = seq(1998, 2012, 2), 
    labels = c("98", "00", "02", "04", "06", "08", "10", "12")) +
  theme(axis.text = element_text(size="10")) +
  ggtitle("Wrong aspect ratio")


## -----------------------------------------------------------------------------
#| label: proximity1
#| fig-width: 7
#| fig-height: 5
#| echo: false
tb_tidy |> 
  filter(!(age %in% c("0-14", "unknown"))) |>
  ggplot(aes(x=year, 
           y=count, 
           colour=sex)) + 
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~age, ncol = 3) +
  scale_color_discrete_divergingx(palette="Geyser") +
  scale_x_continuous("year", 
    breaks = seq(1998, 2012, 2), 
    labels = c("98", "00", "02", "04", "06", "08", "10", "12")) +
  theme(axis.text = element_text(size="10")) +
  ggtitle("Arrangement A")


## -----------------------------------------------------------------------------
#| label: election
#| echo: false
#| out-width: 70%
library(nullabor)
data(electoral)
polls <- electoral$polls
ggplot(polls) +
  geom_boxplot(aes(x=Democrat, 
                   y=Margin)) +
  xlab("democrat") + 
  scale_y_continuous("margin (%)", 
    breaks=seq(0, 100, 20),
    limits=c(0,100)) +
  theme(aspect.ratio = 1.2, 
        panel.grid.major.x = element_blank())


## -----------------------------------------------------------------------------
#| label: election
#| eval: false
#| code-fold: false
# library(nullabor)
# data(electoral)
# polls <- electoral$polls
# ggplot(polls) +
#   geom_boxplot(aes(x=Democrat,
#                    y=Margin)) +
#   xlab("democrat") +
#   scale_y_continuous("margin (%)",
#     breaks=seq(0, 100, 20),
#     limits=c(0,100)) +
#   theme(aspect.ratio = 1.2,
#         panel.grid.major.x = element_blank())

