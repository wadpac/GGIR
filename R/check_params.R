check_params = function(params_sleep) {
  
  check_class = function(category, params, parname, parclass) {
    if (!parname %in% names(params)) {
      stop(paste0("\nParameter ", parname," is missing in object "))
    } else {
      x = params[[parname]]
    }
    if (parclass == "numeric") {
      if (!is.numeric(x)) {
        stop(paste0("\n", category, " argument ", parname, " is not ", parclass))
      }
    }
    if (parclass == "boolean") {
      if (!is.logical(x)) {
        stop(paste0("\n", category, " argument ", parname, " is not ", parclass))
      }
    }
    if (parclass == "character") {
      if (!is.character(x)) {
        stop(paste0("\n", category, " argument ", parname, " is not ", parclass))
      }
    }
  }
  if (length(params_sleep) > 0) {
    check_class("Sleep", params = params_sleep, parname = "anglethreshold", parclass = "numeric")
    check_class("Sleep", params = params_sleep, parname = "timethreshold", parclass = "numeric")
    check_class("Sleep", params = params_sleep, parname = "ignorenonwear", parclass = "boolean")
    check_class("Sleep", params = params_sleep, parname = "constrain2range", parclass = "boolean")
    check_class("Sleep", params = params_sleep, parname = "HASPT.algo", parclass = "character")
    check_class("Sleep", params = params_sleep, parname = "HASIB.algo", parclass = "character")
    check_class("Sleep", params = params_sleep, parname = "Sadeh_axis", parclass = "character")
    check_class("Sleep", params = params_sleep, parname = "longitudinal_axis", parclass = "numeric")
    check_class("Sleep", params = params_sleep, parname = "HASPT.ignore.invalid", parclass = "boolean")
  }
}