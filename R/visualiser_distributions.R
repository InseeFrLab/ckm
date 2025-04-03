#' Visualisation de distributions de probabilités
#' issues de plusieurs scénarios
#'
#' @inheritParams creer_matrice_transition
#' @param precision \code{integer}
#'
#' @return cowplot object
#' @export
#'
#' @examples
#' visualiser_distribution(
#'   D = c(11, 15),
#'   V = c(10, 30)
#' )
#' visualiser_distribution(
#'   DV = data.frame(D = c(11, 15), V = c(10, 30))
#' )
#' @import ggplot2
#' @importFrom cowplot plot_grid
#' @importFrom purrr map
#' @importFrom purrr pmap
#' @importFrom purrr list_c
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
visualiser_distribution <- function(D=NULL,V=NULL,DV=NULL,precision = 5){

  if(is.null(DV)){

    # js n'a pas d'influence sur la distrib grandes valeurs
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
      stop("DV doit être un dataframe avec les colonnes D et V dans cet ordre.")
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
      mat <- creer_matrice_transition(D, V, js = 0)
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
        labs(x="Déviation",y="pij") +
        ggtitle("", subtitle = ) +
        theme_light()
    }
  )

  lab_list <- pmap(parametres, \(D, V, prob5) paste0("D=",D, " - V=", V, " - P(|i-j|<=", precision, ")=", round(prob5,3)))
  cowplot::plot_grid(plotlist = p_list, labels = lab_list, ncol = ncol, label_size = 10, hjust = -0.1)

}
