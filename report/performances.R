library(ggplot2)
library(ggthemes)
library(plotly)



err <- c(0.11, 0.66, 2.89, 0.93, 1.70, 0.71, 0.09, 0.51, 2.45, 0.41, 0.71, 0.36, 0.12, 0.59, 2.16, 0.46, 0.34, 0.42, 
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mod <- c(rep("mod1_ar", 6), rep("mod2_ar", 6), rep("ucm1", 6), rep("ucm2", 6), rep("lstm1", 6), rep("lstm2", 6))
step <- c("1-step", "24-steps", "168-steps","1-step", "24-steps", "168-steps","1-step", "24-steps", "168-steps","1-step", "24-steps", "168-steps",
          "1-step", "24-steps", "168-steps","1-step", "24-steps", "168-steps","1-step", "24-steps", "168-steps","1-step", "24-steps", "168-steps",
          "1-step", "24-steps", "168-steps","1-step", "24-steps", "168-steps","1-step", "24-steps", "168-steps","1-step", "24-steps", "168-steps")
modality <- c(rep("iterated", 3), rep("punctual", 3), rep("iterated", 3), rep("punctual", 3), rep("iterated", 3), rep("punctual", 3),
              rep("iterated", 3), rep("punctual", 3),rep("iterated", 3), rep("punctual", 3),rep("iterated", 3), rep("punctual", 3))

df <- as.data.frame(cbind(err, mod, step, modality))
df$err <- as.numeric(df$err)
df$step <- ordered(df$step, levels = c("1-step", "24-steps", "168-steps"))
str(df)

p <- plot_ly(df, color = I("darkred"),
             transforms = list(
               list(type = "filter",
                    target = mod,
                    operation = '=',
                    value = unique(df$mod)[1]),
               list(type = "filter",
                    target = modality, 
                    operation = "=",
                    value = unique(df$modality)[1]))) %>% 
  add_bars(y = err, x = ~step, width = 0.2) %>%
  layout(updatemenus = list(
                        list(
                          type = 'dropdown',
                          y = 0.8,
                          active = 0,
                          buttons = list(
                            list(method = "restyle",
                                 args = list("transforms[0].value", unique(df$mod)[1]),
                                 label = unique(df$mod)[1]),
                            list(method = "restyle",
                                 args = list("transforms[0].value", unique(df$mod)[2]),
                                 label = unique(df$mod)[2]),
                            list(method = "restyle",
                                 args = list("transforms[0].value", unique(df$mod)[3]),
                                 label = unique(df$mod)[3]),
                            list(method = "restyle",
                                 args = list("transforms[0].value", unique(df$mod)[4]),
                                 label = unique(df$mod)[4]),
                            list(method = "restyle",
                                 args = list("transforms[0].value", unique(df$mod)[5]),
                                 label = unique(df$mod)[5]),
                            list(method = "restyle",
                                 args = list("transforms[0].value", unique(df$mod)[6]),
                                 label = unique(df$mod)[6]))),
                        list(type = 'dropdown',
                          y = 0.7,
                          active = 0,
                          buttons = list(
                            list(method = "restyle",
                                 args = list("transforms[1].value", unique(df$modality)[1]),
                                 label = unique(df$modality)[1]),
                            list(method = "restyle",
                                 args = list("transforms[1].value", unique(df$modality)[2]),
                                 label = unique(df$modality)[2])))))
p

