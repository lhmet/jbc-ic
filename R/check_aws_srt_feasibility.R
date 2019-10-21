# Verifica a viabilidade de aplicação do TRE para EMA alvo
check_aws_srt_feasibility <- function(
                                      .disp_intervals_all = disp_intervals_sel,
                                      .aws_target = "A882",
                                      .perc_ref = PERC_REF,
                                      .neighborhood = neighbors,
                                      .m = M) {
  stopifnot(all(c("int", "site", "perc_valid") %in% names(.disp_intervals_all)))
  stopifnot(is.character(.aws_target), is.numeric(.perc_ref), is.numeric(.m))


  # neighbors stations codes
  neighbors_target <- stations_neighbors(
    neighbors = .neighborhood,
    aws_id = .aws_target,
    M = Inf
  )

  check_disp_int <- .disp_intervals_all %>%
    dplyr::select(int, site, perc_valid) %>%
    # para cada itervalo
    dplyr::group_by(int) %>%
    # ordena os sites pela dist a EMA target
    dplyr::mutate(site = ordered(site, levels = neighbors_target)) %>%
    # imprime seguido esta ordem
    dplyr::arrange(int, site) %>%
    # modifica a perc_valid da EMA target se a perc_val dela eh menor que parametro
    # limiar de disponibilidade
    dplyr::mutate(perc_valid = ifelse((!perc_valid >= .perc_ref) & (site == .aws_target), NA, perc_valid)) %>%
    # seleciona EMAs vizinhas com perc_val acima da .perc_ref, mantendo a perc_valid da EMA target
    dplyr::filter(perc_valid >= .perc_ref | is.na(perc_valid)) %>%
    # seleciona as M+1 EMAs vizinhas aprovadas no criterio de disponibilidade
    dplyr::slice(1:(.m + 1)) %>% # spread(site, perc_valid)
    # adiciona o codigo da EMA target
    dplyr::mutate(target = .aws_target) %>%
    # move a EMA target para primeira coluna
    arrange_vars(c("target" = 1)) %>%
    dplyr::ungroup() %>%
    # agrupa por intervalo e EMA alvo para aninhar os dados (sites e perc_valid)
    dplyr::group_by(target, int) %>%
    tidyr::nest(site, perc_valid)

  # function to check that the target aws have avaialable data
  check_target_aws <- function(x) !is.na(x[1])

  check_disp_int <- dplyr::mutate(check_disp_int,
    # inclui coluna indicativa da disponibilidade de dados na target
    is_available = purrr::map_lgl(
      data,
      ~check_target_aws(.$perc_valid)
    ),
    # lista das M EMAs vizinhas com disponibilidade verificada
    data = map(
      data,
      ~as.character(.$site[-1])
    )
  ) %>%
    rename("neighbors_checked" = data)

  return(check_disp_int)
}



check_srt_feasibility <- function(aws_targets,
                                  disp_intervals_all,
                                  perc_ref,
                                  neighborhood,
                                  m) {
  purrr::map_df(aws_targets, ~check_aws_srt_feasibility(
    .aws_target = .x,
    .disp_intervals_all = disp_intervals_all,
    .perc_ref = perc_ref,
    .neighborhood = neighborhood,
    .m = m
  ))
}