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
#' @param drop A logical vector
#
#' @examples 
#' convert_id <- RiceIDConvert('Os01g0100500','RAP',toType = 'MSU',FALSE)
#' convert_id <- RiceIDConvert(myID = 'Os01g0100500',
#'                             fromType = 'RAP',
#'                             toType = 'SYMBOL',
#'                             drop = FALSE)
#
#' @export 
#' 
#' @return Return a vector or a datafram
RiceIDConvert <- function(myID,fromType,toType,drop){
  RiceID_selected <- dplyr::select(riceiddb,fromType,toType)
  ID_result <- RiceID_selected[which(RiceID_selected == 'Os01g0100500'),]
  ID_result <- ID_result[-(1:nrow(ID_result)),-(1:ncol(ID_result))]

  for (gene in myID) {
    if (gene %in% RiceID_selected[,1]) {
      ID_result_temp <- RiceID_selected[which(RiceID_selected == gene),]
      #rownames(ID_result_temp) <- paste(gene,seq(1,nrow(ID_result_temp)),sep = '_')
      colnames(ID_result_temp) <- c(fromType,toType)
    }else{
      ID_result_temp <- as.data.frame(t(data.frame(temp = c(gene,rep('No.ID',(length(toType)))))))
      # rownames(ID_result_temp) <- gene
      colnames(ID_result_temp) <- c(fromType,toType)
    }
    ID_result <- rbind(ID_result,ID_result_temp)
    if (drop == TRUE | drop == T) {
      ID_result <- dplyr::filter(ID_result,ID_result[,2] != 'No.ID')
    }else{
      ID_result <- ID_result
    }
  }
  if (length(myID) == 1 & nrow(ID_result) < 1) {
    print('--> No ID can be mapped....')
  }else{
    return(unique(ID_result))
  }
  
}
