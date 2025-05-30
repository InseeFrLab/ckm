#' Visualize probability distributions from multiple scenarios
#'
#' This function creates visualizations of probability distributions
#' resulting from different parameter combinations of the Cell Key Method.
#' It generates bar plots showing the probability mass function for each scenario.
#'
#' @inheritParams create_transition_matrix
#' @param DV data.frame. Alternative way to specify D and V parameters as a data frame
#'   with columns "D" and "V". If provided, D and V parameters are ignored
#' @param precision integer. Precision level for probability calculations (default: 5)
#'
#' @return A cowplot object containing multiple ggplot bar charts showing
#'   probability distributions for each parameter combination
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Using separate D and V vectors
#' visualize_distribution(D = c(11, 15), V = c(10, 30))
#'
#' # Using a data frame
#' params_df <- data.frame(D = c(11, 15), V = c(10, 30))
#' visualize_distribution(DV = params_df)
#' }
#' @import ggplot2
#' @importFrom cowplot plot_grid
#' @importFrom purrr map
#' @importFrom purrr pmap
#' @importFrom purrr list_c
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
visualize_distribution <- function(D=NULL, V=NULL, DV=NULL, precision=5){

  # Validate parameters
  assertthat::assert_that(
    is.numeric(precision) && precision > 0 && precision %% 1 == 0,
    msg = "Precision must be a positive integer."
  )
  assertthat::assert_that(
    is.null(D) || (is.numeric(D) && all(D > 0) && all(D %% 1 == 0)),
    msg = "D must be a strictly positive integer vector."
  )
  assertthat::assert_that(
    is.null(V) || (is.numeric(V) && all(V > 0)),
    msg = "V must be a strictly positive numeric vector."
  )
  if(is.null(DV)){

    # js has no influence on large value distribution
    if(is.null(D) || is.null(V)){
      stop("Either provide D and V vectors or a data frame DV with columns D and V.")
    }
    if(!is.numeric(D) || !is.numeric(V)){
      stop("D and V must be numeric vectors.")
    }
    if(any(D <= 0) || any(V <= 0)){
      stop("D and V must be strictly positive.")
    }

    nV <- length(V)
    nD <- length(D)
    ncol <- nV

    parametres <- expand.grid(
      D = D,
      V = V,
      stringsAsFactors = FALSE,
      KEEP.OUT.ATTRS = FALSE
    ) %>%
      as.data.frame() %>%
      arrange(D)

  }else{

    if(!is.data.frame(DV) | !all(names(DV) == c("D","V"))){
      stop("DV must be a data frame with columns D and V in this order.")
    }else{
      parametres <- DV
      nV <- length(unique(parametres$V))
      nD <- length(unique(parametres$D))
      ncol <- if(nrow(parametres) <= 3) nrow(parametres) else ceiling(sqrt(nrow(parametres)))
    }
  }

  limit_x <- c(-1,1)*(max(parametres$D)+1)

  pmat_list <- pmap(
    parametres,
    \(D, V){
      mat <- create_transition_matrix(D, V, js = 0)
      mat_extract <- mat@pTable %>% filter(i == max(i)) %>%
        select(i,v,p)
    }
  )

  p_max <- map(pmat_list, \(mat) max(mat$p)) |> list_c() |> max()
  prob_AD5 <- map(pmat_list, \(mat) mat[abs(v) <= precision, sum(p)]) |> list_c()

  parametres <- parametres %>%
    mutate(prob5 = prob_AD5)

  p_list <- map(
    pmat_list,
    \(mat){

      mat %>%
        ggplot(aes(x=v,y=p)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_x_continuous(limits = limit_x, expand = c(0,0)) +
        scale_y_continuous(limits = c(0,round(p_max+0.01, digits = 2)), expand = c(0,0)) +
        labs(x="Deviation",y="pij") +
        ggtitle("", subtitle = ) +
        theme_light()
    }
  )

  lab_list <- pmap(parametres, \(D, V, prob5) paste0("D=",D, " - V=", V, " - P(|i-j|<=", precision, ")=", round(prob5,3)))
  cowplot::plot_grid(plotlist = p_list, labels = lab_list, ncol = ncol, label_size = 10, hjust = -0.1)

}
