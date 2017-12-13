CC_names_a2l <- read.csv("~/Dropbox/Heise/CC/CC_names_a2l.csv")
CC_names_a2l[1:2] <-  sapply(CC_names_a2l[1:2], as.character)

CC_names_l2a <- read.csv("~/Dropbox/Heise/CC/CC_names_l2a.csv")
CC_names_l2a[1:2] <- sapply(CC_names_l2a[1:2], as.character)

#' @title Alias 2 Line
#'
#' @description Old CC Alias to New CC Line
#'
#' @param df
#'
#' @return df
#'
#' @examples
#'
#' @export

alias2line <- function(df){

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

    alias2line.base <- function(df,mice){
      df[mice] <- as.character(df[[mice]])
      colnames(CC_names_a2l)[1] <- mice
      df <- merge(df,CC_names_a2l[1:2],by=mice,all.x=T)
      df[mice] <- df["CCLine"]
      df <- df[-length(df)]
    }

  if(any("RIX" %in% colnames(df)) && any(!("dam" %in% colnames(df)))) {
    df <- rixsplit(df)
    df <- alias2line.rixbase(df)
    df <- df[-(1:2)]
    } else if(any("dam" %in% colnames(df))){
        df <- alias2line.rixbase(df)
        } else if(any("Strain" %in% colnames(df))){
            df <- alias2line.base(df,"Strain")
        } else if(any("strain" %in% colnames(df))){
          df <- alias2line.base(df,"strain")
        } else if(any("Line" %in% colnames(df))){
          df <- alias2line.base(df,"Line")
        } else if(any("line" %in% colnames(df))){
          df <- alias2line.base(df,"line")
        }
  return(df)
}

#' @title Line 2 Alias
#'
#' @description New CC Line to Old CC Alias
#'
#' @param df
#'
#' @return df
#'
#' @examples
#'
#' @export


line2alias <- function(df){

  line2aliasbase <- function(df){

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
    df <- rixsplit(df)
    df <- line2aliasbase(df)
    df <- df[-(1:2)]
  } else {
    if(any("dam" %in% colnames(df))){
      df <- line2aliasbase(df)
    }
  }
  return(df)
}
