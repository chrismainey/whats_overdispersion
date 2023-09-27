library(readr)
library(tidyverse)
library(FunnelPlotR)
library(scales)

readmissions <- read_csv("../whats_overdispersion/I02040 Compendium Readmissions Dataset (Main) 2022(10).csv")

unique(readmissions$`Period of coverage`)
unique(readmissions$Level)

sub<- readmissions %>%
  filter(`Period of coverage` == "01/04/2021 to 31/03/2022" &
           substring(Level,1,1) == "R" &
           `Sex Breakdown` == "Persons") %>%
  mutate(Numerator = as.numeric(ifelse(Numerator == "*", floor(Expected), Numerator)))

# No limits
sub %>%
  group_by(Level, Trust=`Level description`) %>%
  summarise(Numerator = sum(Numerator),
            Expected = sum(Expected)) %>%
  # ggplot(aes(y=Numerator/Expected, x = Expected))+
  # geom_point() +
  # geom_hline(yintercept = 1, colour = "red")+
  # scale_y_continuous("Indirectly Standardised Readmission Ratio")+
  # scale_x_continuous(labels = comma)+
  funnel_plot(Numerator,Expected, Trust, data_type = "SR"
              , draw_adjusted = FALSE
              , draw_unadjusted = FALSE
              , title = "Readmissions 2021/22")

# standard limits
sub %>%
  group_by(Level, Trust=`Level description`) %>%
  summarise(Numerator = sum(Numerator),
            Expected = sum(Expected)) %>%
  # ggplot(aes(y=Numerator/Expected, x = Expected))+
  # geom_point() +
  # geom_hline(yintercept = 1, colour = "red")+
  # scale_y_continuous("Indirectly Standardised Readmission Ratio")+
  # scale_x_continuous(labels = comma)+
  funnel_plot(Numerator,Expected, Trust, data_type = "SR", draw_unadjusted = TRUE
              ,draw_adjusted = FALSE
              , title = "Readmissions 2021/22")


# od limits
sub %>%
  group_by(Level, Trust=`Level description`) %>%
  summarise(Numerator = sum(Numerator),
            Expected = sum(Expected)) %>%
  # ggplot(aes(y=Numerator/Expected, x = Expected))+
  # geom_point() +
  # geom_hline(yintercept = 1, colour = "red")+
  # scale_y_continuous("Indirectly Standardised Readmission Ratio")+
  # scale_x_continuous(labels = comma)+
  funnel_plot(Numerator,Expected, Trust, data_type = "SR", draw_adjusted = TRUE
              , title = "Readmissions 2021/22", max.overlaps = 50)


