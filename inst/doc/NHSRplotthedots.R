## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
knitr::opts_chunk$set(
  fig.width = 7, fig.height = 5,
  collapse = TRUE,
  comment = "#>"
)

## ----dataset, message=FALSE, warning=FALSE------------------------------------
library(NHSRplotthedots)
library(NHSRdatasets)
library(dplyr)
library(ggplot2)
library(scales)

data("ae_attendances")

ae_attendances %>%
  filter(org_code == "RRK", type == 1) %>%
  ggplot(aes(x = period, y = breaches)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("4-hour target breaches", labels = comma) +
  scale_x_date("Date") +
  labs(title = "Example plot of A&E breaches for organsiation: 'RRK'") +
  theme_minimal()

## ----stableperiod-------------------------------------------------------------
stable_set <- ae_attendances %>%
  filter(
    org_code == "RRK",
    type == 1,
    period < as.Date("2018-04-01")
  )

ptd_spc(
  stable_set,
  value_field = breaches,
  date_field = period,
  improvement_direction = "decrease"
)

## ----changepoint--------------------------------------------------------------
change_set <- ae_attendances %>%
  filter(org_code == "RRK", type == 1)

ptd_spc(change_set,
  value_field = breaches,
  date_field = period,
  improvement_direction = "decrease",
  rebase = ptd_rebase(as.Date("2018-07-01"))
)

## ----facetvignette------------------------------------------------------------
facet_set <- ae_attendances %>%
  filter(
    org_code %in% c("RRK", "RJC", "RJ7", "R1K", "R1H", "RQM"),
    type == 1,
    period < as.Date("2018-04-01")
  )

ptd_spc(
  facet_set,
  value_field = breaches,
  date_field = period,
  facet_field = org_code,
  improvement_direction = "decrease"
) %>%
  plot(fixed_y_axis_multiple = FALSE, x_axis_breaks = "3 months")

## ----facetvignette2-----------------------------------------------------------
facet_set <- ae_attendances %>%
  filter(
    org_code %in% c("RRK", "RJC", "RJ7", "R1K", "R1H", "RQM"),
    type == 1,
    period < as.Date("2018-04-01")
  )

ptd_spc(
  facet_set,
  value_field = breaches,
  date_field = period,
  facet_field = org_code,
  improvement_direction = "decrease"
) %>%
  plot(
    fixed_y_axis_multiple = FALSE,
    x_axis_breaks = "3 months",
    point_size = 2,
    y_axis_label = "Number of 4-hour A&E target breaches"
  )

## ----facetvignette3-----------------------------------------------------------
facet_set <- ae_attendances %>%
  filter(
    org_code %in% c("RRK", "RJC", "RJ7", "R1K", "R1H", "RQM"),
    type == 1,
    period < as.Date("2018-04-01")
  )

a <- ptd_spc(
  facet_set,
  value_field = breaches,
  date_field = period,
  facet_field = org_code,
  improvement_direction = "decrease"
) %>%
  plot(
    fixed_y_axis_multiple = FALSE,
    x_axis_breaks = "3 months",
    point_size = 2,
    y_axis_label = "Number of 4-hour A&E target breaches"
  )

a + theme(axis.text.x = element_text(size = 6, angle = 45))

## ----plotly-------------------------------------------------------------------
ptd_spc(
  facet_set,
  value_field = breaches,
  date_field = period,
  facet_field = org_code,
  improvement_direction = "decrease"
) %>%
  ptd_create_plotly(
    fixed_y_axis_multiple = FALSE,
    x_axis_breaks = "3 months",
    point_size = 2,
    y_axis_label = "Number of 4-hour A&E target breaches",
    icons_position = "none"
  )

