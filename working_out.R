library(NHSRdatasets)
library(tidyverse)
library(broom)

data("ae_attendances")

unique(ae_attendances$period)

ae_sub <- filter(ae_attendances, type ==1 & period == "2018-04-01")

ps_1 <- glm(breaches ~ 1 + offset(log(attendances)), data =ae_sub, family="poisson")

ae_sub$preds<- predict(ps_1, type="response")
#ae_attendances$avg_breaches<- predict(ps_1, type="response")

tidy(ps_1, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)


ae_sub %>%
  ggplot(aes(y=breaches/attendances, x=attendances))+
  geom_point()


library(FunnelPlotR)


funnel_plot(ae_sub$breaches,ae_sub$attendances, group = ae_sub$org_code, data_type = "PR"
            , draw_adjusted = FALSE, draw_unadjusted = TRUE, label= NA)

ae_attendances %>%
  filter(period == "2018-04-01" & type==1) %>%
  ggplot() +
  geom_density(aes(x = breaches))

  ggplot()+
  geom_density(aes(x=x)
               , data = data.frame(x=dpois(x = seq(max(ae_attendances$breaches))
                         , lambda = median(ae_attendances$breaches))))

z<- data.frame(x=dpois(x = seq(2000), lambda = 1614),
               y = seq(2000))
?qpois
ppois(q= seq(0,1,0.005), lambda = 5)


dpois(seq(20), lambda = 5)
