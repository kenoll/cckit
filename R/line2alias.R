#' @title Alias 2 Line
#' @description Convert from old CC nomenclature to CC0XX line designations
#'
#' @param df  Input data frame which contains a column with old CC alias names
#' @param strain.col Character, name of column containing strains to be changed. Not required if is.rix=T.
#' @param is.rix Logical, is the data from inbred crosses (RIX)? Defaults to FALSE.
#'
#' @return Input data frame with CC line converted to new nomenclature
#'
#' @examples
#' @export

alias2line <- function(df,strain.col,is.rix=F){

  if (is.rix == F) {

    df[strain.col] <- as.character(df[[strain.col]])
    colnames(CC_names_a2l)[1] <- strain.col
    df <- merge(df,CC_names_a2l[1:2],by=strain.col,all.x=T)
    df[strain.col] <- df["CCLine"]
    df <- df[-length(df)]
    return(df)

  } else if (is.rix ==T) {

      alias2line.rixbase <- function(df){
        df$dam <- as.character(df$dam)
        df$sire <- as.character(df$sire)

        colnames(CC_names_a2l)[1] <- "sire"
        df <- merge(df,CC_names_a2l[1:2],by="sire",all.x=T)
        df["sire"] <- df["CCLine"]
        df <- df[-length(df)]

        colnames(CC_names_a2l)[1]="dam"
        df=merge(df,CC_names_a2l[1:2],by="dam",all.x=T)
        df["dam"]=df["CCLine"]
        df=df[-length(df)]

        df["RIX"]=paste(df$dam,df$sire,sep="x")
        return(df)
      }

      if (any("RIX" %in% colnames(df)) && any(!("dam" %in% colnames(df)))) {
        df <- splitrix(df)
        df <- alias2line.rixbase(df)
        df <- df[-(1:2)]
        } else if (any("dam" %in% colnames(df))){
          df <- alias2line.rixbase(df)
        }
        return(df)
  }
}

#' @title Line 2 Alias
#' @description Convert from new CC0XX line designations to old CC nomenclature
#'
#' @param df Input data frame which contains a column with new CC line designations
#' @param strain.col Character, name of column containing strains to be changed- not required if is.rix=T.
#' @param is.rix Logical, is the data from inbred crosses (RIX)?
#'
#' @return Input data frame with CC line designation converted to old alias nomenclature
#'
#' @examples
#' @export

line2alias <- function(df,strain.col,is.rix=F){
  if (is.rix == F) {

    if(any(grepl("[[:punct:]]",df[[strain.col]]))){
      df[[strain.col]] <- gsub("[[:punct:]].*","",df[[strain.col]])}

    df[strain.col] <- as.character(df[[strain.col]])
    colnames(CC_names_l2a)[1] <- strain.col
    df <- merge(df,CC_names_l2a[1:2],by=strain.col,all.x=T)
    df[strain.col] <- df["Alias"]
    df <- df[-length(df)]
    return(df)

  } else if (is.rix ==T ) {

    line2alias.rixbase <- function(df){

      df$dam <- as.character(df$dam)
      df$sire <- as.character(df$sire)

      colnames(CC_names_l2a)[1] <- "sire"
      df <- merge(df,CC_names_l2a[1:2],by="sire",all.x=T)
      df["sire"] <- df["Alias"]
      df <- df[-length(df)]

      colnames(CC_names_l2a)[1] <- "dam"
      df <- merge(df,CC_names_l2a[1:2],by="dam",all.x=T)
      df["dam"] <- df["Alias"]
      df <- df[-length(df)]

      df["RIX"] <- paste(df$dam,df$sire,sep="x")
      return(df)
    }

    if(any("RIX" %in% colnames(df)) && any(!("dam" %in% colnames(df)))) {
      df <- splitrix(df)
      df <- line2alias.rixbase(df)
      df <- df[-(1:2)]
    } else if(any("dam" %in% colnames(df))){
      df <- line2alias.rixbase(df)
    }
    return(df)
  }
}


