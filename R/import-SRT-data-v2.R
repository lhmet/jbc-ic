easypackages::libraries(c("tidyverse", "modelr", "broom", "ggfortify"))

# dados -----------------------------------------------------------------------
tab2 <- rio::import("../data/data-table2-Hubbard2012-Cap1-BookPractConcepQC.txt") %>%
  as_tibble()
# tab2 <- rename(tab2, "target" = x)
names(tab2) <- gsub("\\'", "lin", names(tab2))
names(tab2) <- gsub("\\^", "", names(tab2))
names(tab2) <- gsub("\\/", "_", names(tab2))

tab3 <- readr::read_table("data/data-table3-Hubbard2012-Cap1-BookPractConcepQC.txt",
  col_names = TRUE
) %>%
  setNames(., gsub("\\'", "lin", names(.))) %>%
  setNames(., gsub("\\^", "", names(.))) %>%
  setNames(., gsub("\\/", "_", names(.))) %>%
  setNames(., gsub("\\(", "", names(.))) %>%
  setNames(., gsub("\\)", "", names(.))) %>%
  setNames(., gsub("-", "_", names(.))) %>%
  setNames(., tolower(names(.)))

tab3 <- tab2
# tab3_long <- select(tab2, days:y4) %>%
tab3_long <- select(tab3, days:y4) %>%
  gather(y, tempf, -c(x, days))

# dados aninhados por estação 
by_station <- tab3_long %>%
  group_by(y) %>%
  nest()

by_station$data[[1]]
by_station$data[[2]]

# função para o modelo de regressão
reg_model <- function(df) {
  lm(x ~ tempf, data = df)
}

# calc regressão
by_station <- by_station %>%
  mutate(model = map(data, reg_model))
by_station

# verificando os slopes
by_station %>%
  # filter(y == "y1") %>% select(model) %>% `[[`(1)
  # filter(y == "y2") %>% select(model) %>% `[[`(1)
  # filter(y == "y3") %>% select(model) %>% `[[`(1)
  filter(y == "y4") %>%
  select(model) %>%
  `[[`(1)

# adicionando previsoes / ajustes
by_station <- by_station %>%
  mutate(
    # x'
    pred = map2(data, model, add_predictions),
    # x - x'
    resid = map2(data, model, add_residuals)
  )

by_station$model

by_station$pred[[1]]
by_station$pred[[2]]
by_station$resid[[1]]

previstos <- unnest(by_station, pred)
previstos


select(tab3, x, y1, xlin1) %>%
  # mutate(Si = sqrt(sum((xlin1-x)^2)/(nrow(tab3) - 1))) %>%
  mutate(Si = sqrt(sum((xlin1 - x) ^ 2, na.rm = TRUE) / (sum(!is.na(x)) - 1))) %>%
  # mutate(Si = sqrt(sum((xlin4-x)^2)/(nrow(tab2) - 1))) %>%
  select(Si) %>%
  distinct()


select(tab3, x, y1, xlin1) %>%
  mutate(Si = sqrt((sum((xlin1 - x) ^ 2, na.rm = TRUE)) / (nrow(tab3) - 1))) %>%
  distinct(Si)
#
# with(tab2, sigma(lm(x ~ y1)))
#


by_station %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance)

glance <- by_station %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = TRUE)

# forçando vals de sigma do livro para verificação
# tab2
glance$sigma_book <- c(1.349, 0.954, 0.706, 0.694)
# tab3


by_station_norm <- by_station %>%
  select(y, pred) %>%
  # rename("xi" = pred) %>%
  # inner_join(., select(glance, y, sigma_book)) %>%
  inner_join(., select(glance, y, sigma)) %>%
  unnest(pred) %>%
  # para melhor comparação com o artigo
  rename("xi" = pred) %>%
  # unnest(xi) %>%
  # pred_norm: coluna 11 da tab2 (x'1/s1'^2)
  # mutate(xi_norm = xi/sigma_book^2) #%>%
  mutate(xi_norm = xi / sigma ^ 2) # %>%
# select(-(sigma_book:days), -tempf, -x, -pred)

# constante: sum(1/Si^2), linha 36, coluna 11, da tab2
# inv_sum_si2 <- sum(1/glance$sigma_book^2)
inv_sum_si2 <- sum(1 / glance$sigma ^ 2)
# num d estações vizinhas
N <- length(glance$sigma)
# constante: s', linha 36, coluna 13, da tab2
s_lin <- sqrt(N / inv_sum_si2)
s_lin


resultado <- select(by_station_norm, y, days, xi_norm) %>%
  mutate(
    days = as.Date(days, "%m/%d/%Y"),
    # para não confundir a cabeça
    y = gsub("y", "xi_norm", y)
  ) %>%
  spread(y, xi_norm) %>%
  mutate(x_est = (xi_norm1 + xi_norm2 + xi_norm3 + xi_norm4) / inv_sum_si2)

# define f = 3
# (x < x_est - f*s_lin | x > x_est + f*s_lin)


# f = 3; ? N = 15?; n(dias) = 24?
# ver cmo obter distâncias e estações dentro de um dado raio
# ver QC 1b


# reproduzindo resultados------------------------------------------------------
# standard error of the estimates
# para a regressão com cada estação
si <- c(1.349, 0.954, 0.706, 0.694)
# sum(1/si^2)
sum(1 / si ^ 2)

# s'
N <- length(si)
s_lin <- sqrt(N / sum(1 / si ^ 2))
s_lin

# x'
X_est <- round(
  rowSums(plyr::ldply(
    1:nrow(tab2),
    function(i) {
      select(
        tab2,
        xlin1:xlin4
      )[i, ] /
        si ^ 2
    }
  )) / sum(1 / si ^ 2), 1
)
X_est

bias <- X_est - tab2$x

round(mean(X_est - tab2$target), 2)
round(mean(tab2$target - X_est), 2)






# http://statisticsbyjim.com/regression/standard-error-regression-vs-r-squared/



# gráfico de dispersão
ggplot(tab2, aes(x = y1, y = target)) +
  geom_point() +
  geom_smooth(method = lm) +
  geom_abline(slope = 1, intercept = 0, size = 0.1, colour = "red")

# reg linear
reg <- with(tab2, lm(target ~ y1))
# resumo estatísticoda red
summary(reg)
sigma(reg)
# coeficientes da regressão
tidy_reg <- tidy(reg)
tidy_reg

# valores ajustados e resíduos
prev_reg <- augment(reg)
prev_reg


# resumo do modelo
# sigma é o std. error:
# The standard error of the regression provides the absolute measure of the
# typical distance that the data points fall from the regression line.
# S is in the units of the dependent variable.
# This statistic indicates how far the data points are from the regression line
# on average.
sumario <- glance(reg)
# antepenúltima equação em http://onlinestatbook.com/2/regression/accuracy.html
# The only difference is that the denominator is N-2 rather than N.
# The reason N-2 is used rather than N-1 is that two parameters
# (the slope and the intercept) were estimated in order to estimate the sum
# of squares.
Si <- with(prev_reg, round(sqrt(sum((.fitted - target) ^ 2) /
  (length(target) - sumario[["df"]])), 5))

apply(prev_reg, 2, function(x) sum(1 / (x ^ 2)))


with(prev_reg, round(sqrt(sum((.fitted - target) ^ 2) /
  (length(target))), 5))


# You want lower values of S because it signifies that the distances between the
# data points and the fitted values are smaller.
# S is also valid for both linear and nonlinear regression models.
# This fact is convenient if you need to compare the fit between both types
# of models.


comp <- select(tab2, target, y1, xlin1, xlin1_s1lin2) %>%
  mutate(.,
    xlin1_r = prev_reg$.fitted,
    # inf  = inferido
    xlin_inf = tab2$xlin1 / 1.349 ^ 2,
    xlin1_s1lin2_inf = round(xlin1 / prev_reg$.sigma ^ 2, 4)
  )

xi <- prev_reg$.fitted


lado_direito_eq2 <- mean(prev_reg$.sigma ^ -2)
((lado_direito_eq2) ^ -1) ^ 2


sqrt(sum(xi ^ 2 / prev_reg$.sigma ^ 2)) / sqrt((sum(1 / prev_reg$.sigma ^ 2)))
sqrt(sum(xi ^ 2 / prev_reg$.resid ^ 2)) / sqrt((sum(1 / prev_reg$.resid ^ 2)))
(sum(xi ^ 2 / prev_reg$.resid ^ 2)) ^ 0.5 * ((sum(1 / prev_reg$.resid ^ 2))) ^ -0.5
(sum(xi ^ 2 / prev_reg$.std.resid ^ 2)) ^ 0.5 * ((sum(1 / prev_reg$.std.resid ^ 2))) ^ -0.5

Si <- round(summary(reg)[[6]] * summary(reg)[[8]], 3)

# Inferindo por ordem inversa o valor de s1'^2
round(sqrt((tab2$xlin1_s1lin2 / tab2$xlin1) ^ -1), 3)[1]
round(summary(reg)[[6]], 3)
round(with(prev_reg, sqrt(mean((target - .fitted) ^ 2))), 3)
with(tab2, hydroGOF::rmse(xlin1, target))

# (tab2$xlin1/tab2$xlin1_s1lin2)^-1



rio::export(tab2, file = "tab2.csv")