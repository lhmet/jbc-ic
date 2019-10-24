spatial_regression_test <- function(DAYLE_DATA_WIDE, HAVE_VALID_DATA, R2_THRESHOLD, f) {
  if (HAVE_VALID_DATA) {
    # Modelo de regressão
    reg_model <- function(df) {lm(x ~ y, data = df, na.action = na.exclude)}
    # Aplicando a regressão entre a EMA de referência e cada EMA vizinha
    tab_by_station <-
      DAYLE_DATA_WIDE %>%
      select(-c(ema, int)) %>%
      # id: identificador das EMAs, y: valores da variável nas EMAs vizinhas
      gather(id, y, -c(x, date)) %>%
      group_by(id) %>%
      nest() %>%
      mutate(model = map(data, reg_model)) %>%
      mutate(
        pred = map2(data, model, add_predictions),  # x'
        resid = map2(data, model, add_residuals)) %>% # x - x'
      ungroup()
    # Valores previstos ou estimados de temperatura das EMAs vizinhas via regressão linear simples
    tab_previstos <- unnest(tab_by_station, pred) %>% ungroup()
    # Tabela anterior em formato wide para visualização similar a do artigo de referência do SRT
    tab_previstos_wide <- 
      tab_previstos %>% 
      select(date, id, pred) %>% 
      mutate(id = as.numeric(as.factor(id))) %>%
      spread(id, pred) %>%
      setNames(c("date", paste0("xlin", names(.)[-1])))
    # Qualidade do ajuste das regressões
    tab_glance <-
      tab_by_station %>%
      mutate(glance = map(model, broom::glance)) %>% 
      unnest(glance, .drop = TRUE) %>%
      arrange(sigma)
    tab_glance_sel <-
      tab_glance %>%
      filter(adj.r.squared > R2_THRESHOLD)
    N <- nrow(tab_glance_sel)
    # Relação entre o $\sigma$ das estimativas e o coeficiente $R^2$
    plot_target_01 <- plot_01_fun(tab_glance_sel = tab_glance_sel)
    # Termos necessários para equação 5 de Hubbard et al. 2012
    inv_sum_si2 <- sum(1/tab_glance_sel$sigma^2) # lado direito da eq. 5
    tab_N <- length(tab_glance_sel$sigma) # num d estações vizinhas - numerador do lado esquerdo da eq. 5
    slin <- sqrt(tab_N/inv_sum_si2) # constante: s', linha 36, coluna 13, da tab2, obtida isolando s' na eq. 5
    # Vamos filtrar os dados gerados anteriormente mantendo somente as EMAs selecionadas.
    data_wide_sel <-
      #    data_wide_target %>%
      DAYLE_DATA_WIDE %>%
      select(c("date", "x", tab_glance_sel$id))
    tab_by_station_sel <-
      tab_by_station %>%
      filter(id %in% tab_glance_sel$id)
    tab_previstos_sel <-
      tab_previstos %>%
      filter(id %in% tab_glance_sel$id)
    tab_previstos_wide_sel <- 
      tab_previstos_sel %>% 
      select(date, id, pred) %>% 
      mutate(id = as.numeric(as.factor(id))) %>%
      spread(id, pred) %>%
      setNames(c("date", paste0("xlin", names(.)[-1])))
    # Normalização das temperaturas previstas para as EMAs vizinhas. Numerador da equação 4 de Hubbard et al. 2012
    tab_by_station_norm_sel <-
      tab_by_station_sel %>%
      select(id, pred) %>%
      inner_join(., select(tab_glance, id, sigma), by = "id") %>%
      unnest(pred) %>%
      rename("xlin" = pred) %>%
      mutate(xlin_norm = xlin/sigma^2) # numerador da eq. 4
    # Colocando temperaturas previstas normalizadas no formato wide
    tab_by_station_norm_wide_sel <-
      tab_by_station_norm_sel %>%
      select(id, date, xlin_norm) %>%
      mutate(id = as.numeric(as.factor(id))) %>% # ao invés dos códigos das EMAs utilizaremos um num sequencial
      spread(id, xlin_norm) %>%
      setNames(c("date", paste0("xlin_norm", names(.)[-1])))
    # Tabela com a a estimativa final da temperatura para a EMA alvo, obtida pela média ponderada do erro padrão das regressões com as EMAs vizinhas
    tab_resultado <-
      tab_by_station_norm_wide_sel %>%
      mutate(
        #x01 = tab_resultado_1
        x_est = rowSums(select(., -1)) / inv_sum_si2,
        #      x_est_2 = tab_resultado_ok,
        #      x_est_3 = tab_resultado_1,
        n_valid = rowSums(!is.na(select(., -1)), na.rm = TRUE)) %>%
      right_join(tab_previstos_wide_sel, ., by = 'date') %>%
      right_join(data_wide_sel, ., by = 'date') %>%
      mutate(
        vies = x - x_est)
    #      vies2 = x - x_est_2,
    #      vies3 = x - x_est_3)
    # Verificação Viés e r2
    vies <- hydroGOF::gof(tab_resultado$x_est, tab_resultado$x)
    r2 <- vies[rownames(vies) == "R2"]
    # Verificação de que a observação caia no intervalo de confiança da estimativa pela regressão linear
    tab_susp <- 
      tab_resultado %>%
      mutate(
        sigma_lin = slin,
        r2 = r2,
        lower = x_est - f*slin,
        upper = x_est + f*slin,
        #      lower = x_est_2 - f*slin, upper = x_est_2 + f*slin,
        suspect = !(x >= lower &  x <= upper)
      ) %>%
      #    select(date, x, x_est, lower, upper, suspect, x_est_2)
      select(date, x, x_est, sigma_lin, r2, lower, upper, suspect)
    # Gráfico da série temporal das observações, estimativas e IC.
    plot_target_02 <- plot_02_fun(tab_susp = tab_susp)
    # Gráfico de dispersão das observações e estimativas
    formula <- y ~ x
    plot_target_03 <- plot_03_fun(tab_susp = tab_susp, formula = formula)
    # Lista com os resultados que serão retornados
    rmf <- list()
    rmf[[1]] <- tab_resultado
    rmf[[2]] <- tab_susp
    rmf[[3]] <- vies
    rmf[[4]] <- plot_target_01
    rmf[[5]] <- plot_target_02
    rmf[[6]] <- plot_target_03
  }
  else {
    rmf <- "Teste não aplicado, devido ao fato da EMA alvo não possuir dados válidos para comparar com os dados das EMAs vizinhas."
  }
  return(rmf) }

# Relação entre o sigma das estimativas e o coeficiente R^2
plot_01_fun <- function(tab_glance_sel) {
  p01 <- 
    tab_glance_sel %>%
    ggplot2::ggplot(aes(x = adj.r.squared, y = sigma)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::labs(x = expression(R^2), y = expression(sigma~~(degree~C))) +
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
      label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~")),
      formula = formula, 
      parse = TRUE) +
    geom_abline(slope = 1, intercept = 0) +
    theme_bw()
  return(p03)
  }



