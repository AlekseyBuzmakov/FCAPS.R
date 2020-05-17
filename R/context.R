
#' Converting data to binary table
#'
#' The function binarize data in requested number of quantiles
#' taken into account that some quantiles can have a lot of observations
#'
#' @param x is the data.frame that contains the data
#' @param nQuantiles the number of quantiles for binarization
#'
#' @return the new data.frame with binary values
#'
#' @export
#'
#' @examples
#'
contextToBinData = function(x,nQuantiles = 3) {
  result=NULL
  nms=NULL
  for(col.name in names(x)) {
    col=x[[col.name]]
    if(!is.factor(col) && length(unique(col)) <= 20) {
      is.ord = is.numeric(col)
      col = factor(col,ordered = is.ord)
    }
    if(is.character(col)) {
      next
    }
    if(is.logical(col)) {
      result=cbind(result,col)
      nms=c(nms,col.name)
    }
    if(is.factor(col) && !is.ordered(col)) {
      for(lvl in levels(col)){
        result=cbind(result,col==lvl)
        nms=c(nms,paste0(col.name, "=",lvl))
      }
    }
    if(is.factor(col) && is.ordered(col)  ) {
      for(lvl in levels(col)){
        if(lvl !=rev(levels(col))[1]) {
          result=cbind(result,col<=lvl)
          nms=c(nms,paste0(col.name, " <= ",lvl))
        }
        if(lvl != levels(col)[1]) {
          result=cbind(result,col>=lvl)
          nms=c(nms,paste0(col.name, " >= ",lvl))
        }
      }
    }
    if(is.numeric(col)){
      if( is.na(nQuantiles) || nQuantiles <= 1 ) {
        breaks=sort(unique(col))
        breaks=(breaks[-length(breaks)]+breaks[-1])/2.0
      } else {
        breaks = unique(quantile(col,probs = seq(1/nQuantiles,to=1 - 1/nQuantiles,length.out = nQuantiles-1)))
      }
      for(i in breaks){
        if( i != max(breaks)){
          result=cbind(result,col <= i )
          nms=c(nms,paste0(col.name, "<=",i))
        }
        if( i != breaks[1]) {
          result=cbind(result,col > i )
          nms=c(nms,paste0(col.name, ">",i))
        }
      }
    }
  }
  colnames(result)=nms
  result[is.na(result)]=F
  rownames(result) = rownames(x)
  return(result)
}

#' A function that writes a logical table as a formal context
#'
#' @param db is the dataset to code as a formal context
#' @param file -- is the file name to write the data to
#'
#' @export
#'
#' @examples
#'
toFCAPSJson = function(db, file) {
  # And write the header
  cat(file=file, paste0(
    '[{"ObjNames":',jsonlite::toJSON(rownames(db)),',\n"Params":{"AttrNames":',
    jsonlite::toJSON(colnames(db)),
    '}},{"Data":[\n',
    # and the body of the file
    paste0(apply(db,1,FUN=function(x){return(paste0('{"Inds":',jsonlite::toJSON(which(x)-1),'}'))}),collapse = ",\n"),
    ']}]'
  ))
}
