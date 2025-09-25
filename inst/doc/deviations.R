## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  fig.width = 7, fig.height = 5,
  collapse = TRUE,
  comment = "#>"
)
library(dplyr)
library(ggplot2)
library(NHSRplotthedots)
library(NHSRdatasets)

## ----fig.cap="An SPC chart labelled 'Data' on the y-axis, with dates from 22 March 2021 to 8 April 2021, at 1-day intervals, on the x-axis. From left to right the line shows four grey common cause points followed by a single blue special cause improvement point followed by a further 12 grey common cause points. A caption reports the upper process limit as 4.6, the mean as 1.94 and the lower process limit as -0.72."----
data <- c(1, 2, 1, 2, 10, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1)
date <- seq(as.Date("2021-03-22"), by = 1, length.out = 18)
df <- tibble::tibble(data, date)

# screen_outliers = TRUE by default
spc_data <- ptd_spc(df, value_field = data, date_field = date)
spc_data %>%
  plot() +
  labs(
    caption = paste(
      "UPL = ", round(spc_data$upl[1], 2),
      ", Mean = ", round(spc_data$mean_col[1], 2),
      ", LPL = ", round(spc_data$lpl[1], 2)
    )
  )

## ----fig.cap="An SPC chart labelled 'Data' on the y-axis, with dates from 22 March 2021 to 8 April 2021, at 1-day intervals, on the x-axis. From left to right the line shows four grey common cause points followed by a single blue special cause improvement point followed by a further 12 grey common cause points. A caption reports the upper process limit as 6.95, the mean as 1.94 and the lower process limit as -3.06."----
# setting screen_outliers = FALSE produces the same output as Excel
spc_data <- ptd_spc(df, value_field = data, date_field = date, screen_outliers = FALSE)
spc_data %>%
  plot() +
  labs(
    caption = paste(
      "UPL = ", round(spc_data$upl[1], 2),
      ", Mean = ", round(spc_data$mean_col[1], 2),
      ", LPL = ", round(spc_data$lpl[1], 2)
    )
  )

## ----fig.cap="An SPC chart labelled 'Attendances' on the y-axis, with dates from 1 April 2016 to 1 March 2019, at 1-month intervals, on the x-axis. The dates are presented at 45 degrees to the x-axis. The line chart has three sections, each with twelve grey common cause points on the process line. Each section has its own upper process limit, mean, and lower process limit lines, that are not connected to the lines of another section. The process line is also split into three sections. A caption states 'Some trial limits created by groups of fewer than 12 points exist. These will become more reliable as more data is added.'"----
spc_data <- ae_attendances %>%
  group_by(period) %>%
  summarise(across(attendances, sum)) %>%
  ptd_spc(attendances, period, rebase = as.Date(c("2017-04-01", "2018-04-01")))

plot(spc_data, break_lines = "both")

## ----fig.cap="An SPC chart labelled 'Attendances' on the y-axis, with dates from 1 April 2016 to 1 March 2019, at 1-month intervals, on the x-axis. The line chart has three sections, each with twelve grey common cause points on the process line. Each section has its own upper process limit, mean, and lower process limit lines, that are not connected to the lines of another section. The process line is unbroken. A caption states 'Some trial limits created by groups of fewer than 12 points exist. These will become more reliable as more data is added.'"----
plot(spc_data, break_lines = "limits")

## ----fig.cap="An SPC chart labelled 'Attendances' on the y-axis, with dates from 1 April 2016 to 1 March 2019, at 1-month intervals, on the x-axis. The line chart has three sections, each with twelve grey common cause points on the process line. Each section has its own upper process limit, mean, and lower process limits, but the lines for these are unbroken between sections. The process line is however broken between sections. A caption states 'Some trial limits created by groups of fewer than 12 points exist. These will become more reliable as more data is added.'"----
plot(spc_data, break_lines = "process")

## ----fig.cap="An SPC chart labelled 'Attendances' on the y-axis, with dates from 1 April 2016 to 1 March 2019, at 1-month intervals, on the x-axis. The line chart has three sections, each with twelve grey common cause points on the process line. Each section has its own upper process limit, mean, and lower process limits, but the lines for these are unbroken between sections. The process line is also unbroken between the sections. A caption states 'Some trial limits created by groups of fewer than 12 points exist. These will become more reliable as more data is added.'"----
plot(spc_data, break_lines = "none")

## ----fig.cap="An SPC chart labelled 'Attendances' on the y-axis, with dates from 1 April 2016 to 1 March 2019, at 1-month intervals, on the x-axis. The dates are presented at 90 degrees to the x-axis. The line chart has 36 grey common cause points on the process line. There are dashed lines for the upper and lower process limits and a solid line for the mean."----
ae_attendances %>%
  group_by(period) %>%
  summarise(across(attendances, sum)) %>%
  ptd_spc(attendances, period) %>%
  plot(theme_override = theme(axis.text.x = element_text(angle = 90)))

