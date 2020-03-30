#' Extract medication inforamtion from clinical notes
#'
#' This function is an interface to the \code{medExtractR} function within the \code{medExtractR} 
#' package, and allows drug dosing information to be extracted from free text sources, 
#' e.g., clinical notes.
#' 
#' 
#'
#' @param note_fn File name(s) for the text files containing the clinical notes. Can be 
#' a character string for an individual note, or a vector or list of file names for 
#' multiple notes.
#' @param drugnames Vector of drug names for which dosing information should be extracted. 
#' Can include various forms (e.g., generic, brand name) as well as abbreviations.
#' @param drgunit Unit of the drug being extracted, e.g., 'mg'
#' @param windowlength Length of the search window (in characters) around the drug name in 
#' which to search for dosing entities
#' @param max_edit_dist Maximum edit distance allowed when attempting to extract \code{drugnames}. 
#' Allows for capturing misspelled drug name information. 
#' @param ... Additional arguments to \code{medExtractR}
#'
#' @return A data.frame with the extracted dosing information
#' @export
#'
extractMed <- function(note_fn, drugnames, drgunit,
                        windowlength, max_edit_dist = 0, ...){

  if(!(class(note_fn) %in% c("character", "list"))){
    stop("`notefn` must be of class 'character' or 'list'")
  }
  
  # note_fn is a single note
  if((class(note_fn)[1] == "character") & (length(note_fn) == 1)){
    
    gdx <- getDose(note_fn,
                   drug_names = drugnames,
                   unit=drgunit,
                   window_length=windowlength,
                   max_dist=max_edit_dist, ...)  
    
  }else{ 
    # note_fn contains multiple notes
    
    # If supplied as a vector, convert to list
    if(class(note_fn) == "character"){
      note_fn <- as.list(note_fn)
    }
    
    # Run module on all notes
    gdx <- do.call(rbind, lapply(note_fn, function(x){
      getDose(x,
              drug_names = drugnames,
              unit=drgunit,
              window_length=windowlength,
              max_dist=max_edit_dist, ...) 
    }))
    
  }

  return(gdx)
}
