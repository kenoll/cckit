#' @title Fill in reference calls 
#'
#' @description Replaced reference calls (- or ~, keyed in as NA) with actual reference allele
#'
#' @param df data frame of Sanger output
#'
#' @return Data frame with reference allele calls
#'
#' @examples replaceref(merge_output)
#'
#' @export

replaceref <- function(df){
  df$X129S1_SvImJ[is.na(df$X129S1_SvImJ)] <- df$Ref[is.na(df$X129S1_SvImJ)]
  df$A_J[is.na(df$A_J)] <- df$Ref[is.na(df$A_J)]
  df$CAST_EiJ[is.na(df$CAST_EiJ)] <- df$Ref[is.na(df$CAST_EiJ)]
  df$NOD_ShiLtJ[is.na(df$NOD_ShiLtJ)] <- df$Ref[is.na(df$NOD_ShiLtJ)]
  df$NZO_HlLtJ[is.na(df$NZO_HlLtJ)] <- df$Ref[is.na(df$NZO_HlLtJ)]
  df$PWK_PhJ[is.na(df$PWK_PhJ)] <- df$Ref[is.na(df$PWK_PhJ)]
  df$WSB_EiJ[is.na(df$WSB_EiJ)] <- df$Ref[is.na(df$WSB_EiJ)]
  
  return(df)
}

#' @title Remove asterices
#'
#' @description Remove * from allele calls
#'
#' @param df data frame of Sanger output
#'
#' @return Data frame with reference allele calls
#'
#' @examples replaceast(merge_output)
#'
#' @export

replaceast <- function(df){
  df$X129S1_SvImJ <- df$X129S1_SvImJ %>% str_replace("\\*", '')
  df$A_J <- df$A_J %>% str_replace("\\*", '')
  df$CAST_EiJ <- df$CAST_EiJ %>% str_replace("\\*", '')
  df$NOD_ShiLtJ <- df$NOD_ShiLtJ %>% str_replace("\\*", '')
  df$NZO_HlLtJ <- df$NZO_HlLtJ %>% str_replace("\\*", '')
  df$PWK_PhJ <- df$PWK_PhJ %>% str_replace("\\*", '')
  df$WSB_EiJ <- df$WSB_EiJ %>% str_replace("\\*", '')
  
  return(df)
}
