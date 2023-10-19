library(ggplot2)
library(tidyr)

logkow <- seq(-3,7,0.1)
#briggs 1982
briggs1982 <- 0.784 * exp((-(logkow - 1.78)^2/ 2.44 ))
hsu1991    <- 0.7   * exp((-(logkow - 3.07)^2/ 2.78))
#mystery <- -0.0648 * logkow^2 * 0.241 * logkow + 0.5822
epa2014_quadratic <- -0.0648 * logkow^2 + 0.241 * logkow + 0.5822
dettenmaier_sigmoidal <- 11/(11+2.6^logkow)
dettenmaier_linear <- -0.15 * logkow + 0.87
tscf_df <- data.frame(logkow=logkow,
          briggs1982=briggs1982,
          epa2014_quadratic = epa2014_quadratic,
          dettenmaier_sigmoidal=dettenmaier_sigmoidal,
          dettenmaier_linear=dettenmaier_linear, 
          hsu1991=hsu1991)
summary(tscf_df)

tscf_df_long <- tscf_df %>% pivot_longer(cols=c("briggs1982", "epa2014_quadratic", 
                                                "dettenmaier_sigmoidal",
                                                "dettenmaier_linear", "hsu1991"), 
                                         names_to="source",
                                         values_to="tscf")
tscf_df_long$tscf
tscf_df_long$tscf <- replace(tscf_df_long$tscf, tscf_df_long$tscf<0.038, 0.038)
tscf_df_long$tscf <- replace(tscf_df_long$tscf, tscf_df_long$tscf>0.8, 0.8) #EU focus guidance

ggplot(data=tscf_df_long, aes(x=logkow, y=tscf, color=source)) +
  geom_line() +
  theme_classic()

       