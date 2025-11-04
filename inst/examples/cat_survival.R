library(ggsurvfit)
library(catplot)

x <- survfit2(Surv(time, status) ~ sex, data = df_lung)

x |> cat_survival()

x |> cat_survival(add_confidence_interval = TRUE)
