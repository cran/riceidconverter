#' @name RiceIDConvert
#' @author Xiang LI <ynaulx@@gmail.com>
#' 
#' @title Convert Biological ID from One to Another for Oryza Sativa
#' @description 
#' \code{RiceIDConverter} Convert One Biological ID to Another for Oryza Sativa, such As SYMBOL to TRANSCRIPTID.
#' 
#' @param myID A vector of entrez  id
#' @param fromType Keytype of input  id
#' @param toType Keytype of output  id
#
#' @examples 
#' convert_id <- RiceIDConvert('Os01g0100500','RAP',toType = 'MSU')
#' convert_id <- RiceIDConvert(myID = 'Os01g0100500',
#'                             fromType = 'RAP',
#'                             toType = 'SYMBOL')
#
#' @export 
#' 
#' @return Return a vector or a datafram
RiceIDConvert <- function(myID,fromType,toType){
  
  myid <- as.data.frame(myID)
  colnames(myid) <- fromType
  
  RiceID_selected <- dplyr::select(riceiddb,c(fromType,toType))
  res <- merge(myid,RiceID_selected, by = fromType, all.x = T)
  return(res)
}
