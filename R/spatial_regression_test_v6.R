# Modelo de regressão
reg_model <- function(df) {
  lm(x ~ y, data = df, na.action = na.exclude)
}

spatial_regression_test <- function(DAILY_DATA_WIDE, R2_THRESHOLD = 0.5, f = 3) {

  # DAILY_DATA_WIDE = data_int_wide_sel
  stopifnot(
    any(str_detect(names(DAILY_DATA_WIDE), "A[0-9]{3}")),
    "x" %in% names(DAILY_DATA_WIDE),
    all(c("target", "int") %in% names(DAILY_DATA_WIDE))
  )
  # Ordem das EMAs vizinhas mais próximas à EMA alvo
  yn_order <-
    DAILY_DATA_WIDE %>%
    dplyr::select(-c(target, int, date, x)) %>%
    names()
  
  # Aplicando a regressão entre a EMA de referência e cada EMA vizinha
  tab_by_station <-
    DAILY_DATA_WIDE %>%
    select(-c(target, int)) %>%
    # id: identificador das EMAs, y: valores da variável nas EMAs vizinhas
    gather(site, y, -c(x, date)) %>%
    group_by(site) %>%
    nest() %>%
    mutate(model = map(data, reg_model)) %>%
    mutate(
      pred = map2(data, model, add_predictions), # x'
      resid = map2(data, model, add_residuals)
    ) %>% # x - x'
    ungroup()
  # Valores previstos ou estimados de temperatura das EMAs vizinhas via regressão linear simples
  tab_previstos <- unnest(tab_by_station, pred) %>% ungroup()
  # Tabela anterior em formato wide para visualização similar a do artigo de referência do SRT
  tab_previstos_wide <-
    tab_previstos %>%
    select(date, site, pred) %>%
    # mutate(id = as.numeric(as.factor(id))) %>%
    spread(site, pred) %>%
    select(date, yn_order) %>%
    setNames(c("date", paste0("xlin_", names(.)[-1])))
  # Qualidade do ajuste das regressões
  tab_glance <-
    tab_by_station %>%
    mutate(glance = map(model, broom::glance)) %>%
    unnest(glance, .drop = TRUE) %>%
    arrange(sigma)

  # lidando com casos de EMAs sem r2 > R2_THRESHOLD
  if (any(tab_glance$r.squared >= R2_THRESHOLD)) {
    tab_glance_sel <-
      tab_glance %>%
      filter(r.squared >= R2_THRESHOLD)
  } else {
    # pega a EMA com r2 menos pior
    tab_glance_sel <-
      tab_glance %>%
      arrange(desc(r.squared)) %>%
      slice(1)
  }
  # N <- nrow(tab_glance_sel)
  # Relação entre o $\sigma$ das estimativas e o coeficiente $R^2$
  # plot_target_01 <- plot_01_fun(tab_glance_sel = tab_glance_sel)
  # Termos necessários para equação 5 de Hubbard et al. 2012
  inv_sum_si2 <- sum(1 / tab_glance_sel$sigma^2) # lado direito da eq. 5
  tab_N <- length(tab_glance_sel$sigma) # num d estações vizinhas - numerador do lado esquerdo da eq. 5
  slin <- sqrt(tab_N / inv_sum_si2) # constante: s', linha 36, coluna 13, da tab2, obtida isolando s' na eq. 5
  # Ordem das EMAs vizinhas mais próximas à EMA alvo que foram selecionadas
  yn_order_sel <- yn_order[yn_order %in% tab_glance_sel$site]

  # yn_order
  # tab_glance_sel$id[-1]
  # yn_order %in% tab_glance_sel$id[-1]
  # Vamos filtrar os dados gerados anteriormente mantendo somente as EMAs selecionadas.
  data_wide_sel <-
    DAILY_DATA_WIDE %>%
    select(c("date", "x", yn_order_sel))
  tab_by_station_sel <-
    tab_by_station %>%
    filter(site %in% yn_order_sel)
  tab_previstos_sel <-
    tab_previstos %>%
    filter(site %in% yn_order_sel)
  tab_previstos_wide_sel <-
    tab_previstos_wide %>%
    select(date, paste0("xlin_", yn_order_sel))
  # Normalização das temperaturas previstas para as EMAs vizinhas. Numerador da equação 4 de Hubbard et al. 2012
  tab_by_station_norm_sel <-
    tab_by_station_sel %>%
    select(site, pred) %>%
    inner_join(., select(tab_glance, site, sigma), by = "site") %>%
    unnest(pred) %>%
    rename("xlin" = pred) %>%
    mutate(xlin_norm = xlin / sigma^2) # numerador da eq. 4
  # Colocando temperaturas previstas normalizadas no formato wide
  tab_by_station_norm_wide_sel <-
    tab_by_station_norm_sel %>%
    select(site, date, xlin_norm) %>%
    # mutate(id = as.numeric(as.factor(id))) %>% # ao invés dos códigos das EMAs utilizaremos um num sequencial
    spread(site, xlin_norm) %>%
    setNames(c("date", paste0("xlin_norm_", names(.)[-1]))) %>%
    select(date, paste0("xlin_norm_", yn_order_sel))
  # Tabela com a a estimativa final da temperatura para a EMA alvo, obtida pela média ponderada do erro padrão das regressões com as EMAs vizinhas
  tab_resultado <-
    tab_by_station_norm_wide_sel %>%
    mutate(
      # x01 = tab_resultado_1
      x_est = rowSums(select(., -1)) / inv_sum_si2,
      #      x_est_2 = tab_resultado_ok,
      #      x_est_3 = tab_resultado_1,
      n_valid = rowSums(!is.na(select(., -1)), na.rm = TRUE)
    ) %>%
    right_join(tab_previstos_wide_sel, ., by = "date") %>%
    right_join(data_wide_sel, ., by = "date") %>%
    mutate(vies = x - x_est)
  # vies2 = x - x_est_2,
  # vies3 = x - x_est_3)
  # Verificação data_gof e r2
  data_gof <- hydroGOF::gof(sim = tab_resultado$x_est, obs = tab_resultado$x)
  r2 <- data_gof[rownames(data_gof) == "R2"]
  # Verificação de que a observação caia no intervalo de confiança da estimativa pela regressão linear
  tab_susp <-
    tab_resultado %>%
    mutate(
      sigma_lin = slin,
      r2 = r2,
      lower = x_est - f * slin,
      upper = x_est + f * slin,
      #      lower = x_est_2 - f*slin, upper = x_est_2 + f*slin,
      suspect = !(x >= lower & x <= upper)
    ) %>%
    #    select(date, x, x_est, lower, upper, suspect, x_est_2)
    select(date, x, x_est, sigma_lin, r2, lower, upper, suspect)
  # Gráfico da série temporal das observações, estimativas e IC.
  # plot_target_02 <- plot_02_fun(tab_susp = tab_susp)
  # Gráfico de dispersão das observações e estimativas
  #formula <- y ~ x
  # plot_target_03 <- plot_03_fun(tab_susp = tab_susp, formula = formula)
  # Lista com os resultados que serão retornados
  # rmf <- list()
  # rmf[[1]] <- tab_resultado
  # rmf[[2]] <- tab_susp
  # rmf[[3]] <- data_gof
  # rmf[[4]] <- plot_target_01
  # rmf[[5]] <- plot_target_02
  # rmf[[6]] <- plot_target_03
  # return(rmf)
  return(tab_susp)
}

# Relação entre o sigma das estimativas e o coeficiente R^2
plot_01_fun <- function(tab_glance_sel) {
  p01 <-
    tab_glance_sel %>%
    ggplot2::ggplot(aes(x = adj.r.squared, y = sigma)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::labs(x = expression(R^2), y = expression(sigma ~ ~(degree ~ C))) +
    ggplot2::theme_bw()
  return(p01)
}

# Gráfico da série temporal das observações, estimativas e IC
plot_02_fun <- function(tab_susp) {
  p02 <-
    tab_susp %>%
    ggplot2::ggplot(aes(x = date, y = x)) +
    ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    ggplot2::geom_point(colour = "red") +
    #    geom_line(aes(x = date, y = x_est_2)) +
    ggplot2::geom_line(aes(x = date, y = x_est)) +
    ggplot2::theme_bw()
  return(p02)
}

# Gráfico de dispersão das observações e estimativa
plot_03_fun <- function(tab_susp, formula) {
  p03 <-
    tab_susp %>%
    #    ggplot(aes(x = x, y = x_est_2)) +
    ggplot(aes(x = x, y = x_est)) +
    geom_point(colour = "red") +
    coord_equal() +
    geom_smooth(method = "lm", formula = formula) +
    ggpmisc::stat_poly_eq(aes(
      label = paste(..eq.label.., ..adj.rr.label.., sep = "~~")
    ),
    formula = formula,
    parse = TRUE
    ) +
    geom_abline(slope = 1, intercept = 0) +
    theme_bw()
  return(p03)
}