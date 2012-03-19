is.multi.transiogram <-
function(object) {
  if(!is(object, "multi.transiogram")) return(FALSE)
  if(!prod(c("lags", "type", "Tmat") %in% names(object))) return(FALSE)
  if(length(names(object)) != 3) return(FALSE)
  if(!is.array(object$Tmat)) return(FALSE)
  if(!is.numeric(object$lags)) return(FALSE)
  if(!is.character(object$type)) return(FALSE)
  if(object$type != "Empirical" && object$type != "Theoretical") return(FALSE)
  return(TRUE)
}

