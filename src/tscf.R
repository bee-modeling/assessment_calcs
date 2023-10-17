library(ggplot2)
library(tidyr)

logkow <- seq(-3,7,0.1)
#briggs 1982
briggs1982 <- 0.784 * exp((-(logkow - 1.78)^2/ 2.44 ))
mystery <- -0.0648 * logkow^2 * 0.241 * logkow + 0.5822
mystery_quadratic <- -0.0648 * logkow^2 + 0.241 * logkow + 0.5822
dettenmeier_sigmoidal <- 11/(11+2.6^logkow)
dettenmeier_linear <- -0.15 * logkow + 0.87
tscf_df <- data.frame(logkow=logkow,
          briggs1982=briggs1982,
          mystery = mystery,
          dettenmeier_sigmoidal=dettenmeier_sigmoidal,
          dettenmeier_linear=dettenmeier_linear)
summary(tscf_df)

tscf_df_long <- tscf_df %>% pivot_longer(cols=c("briggs1982", "mystery", 
                                                "dettenmeier_sigmoidal",
                                                "dettenmeier_linear"), 
                                         names_to="source",
                                         values_to="tscf")
tscf_df_long$tscf
tscf_df_long$tscf <- replace(tscf_df_long$tscf, tscf_df_long$tscf<0.038, 0.038)
tscf_df_long$tscf <- replace(tscf_df_long$tscf, tscf_df_long$tscf>0.8, 0.8) #EU focus guidance

ggplot(data=tscf_df_long, aes(x=logkow, y=tscf, color=source)) +
  geom_line() +
  theme_classic()

       