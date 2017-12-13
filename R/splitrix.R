#' @title Split RIX
#' @description Split RIX (CC0XXxCC0XX) into dam and sire
#'
#' @param df
#'
#' @return df
#'
#' @examples
#'
#' @export

rixsplit <- function(df) {
  if(any(!("RIX" %in% colnames(df)))) stop('RIX is not a column name in the dataset')
  df$RIX <- as.character(df$RIX)
  RIX_sep <- data.frame(do.call("rbind", strsplit(df$RIX,"x")))
  colnames(RIX_sep)[1:2] <- c("dam","sire")
  df <- cbind(RIX_sep,df)
  return(df)
}
