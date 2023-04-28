ShellDoc2Vignette <- function(argument = "mode") {  
  if (!is.character(argument)) argument <- deparse(substitute(argument))
  .utils <- asNamespace("utils")
  .tools <- asNamespace("tools")
  helpfile = .utils$.getHelpFile(help("GGIR"))
  
  
  hs <- utils::capture.output(switch("Rd", Rd = .tools$prepare_Rd(helpfile)))
  
  # substitute curly braces to avoid problems later on
  hs = gsub(pattern = "\\{|\\}", replacement = "__", x = hs)
  
  
  # argument to look for with Rd format
  lookfor = paste0("\\item__",argument, "____")
  
  # basic arguments
  start_args = grep("__\\arguments__", hs, fixed = TRUE)
  end_args = grep("__\\details__", x = hs, fixed = TRUE)
  basic_args = hs[start_args:(end_args - 1)]
  
  # Arguments in parameters
  start_details = grep("__\\details__", hs, fixed = TRUE)
  end_details = grep("__\\examples__", x = hs, fixed = TRUE)
  parameters = hs[start_details:(end_details - 1)]
  
  tmp = grep(lookfor, basic_args); if (length(tmp) > 1) tmp = tmp[1]
  tmp2 = grep(lookfor, parameters); if (length(tmp2) > 1) tmp2 = tmp2[1]
  
  # Find the argument definition --------------------------------------------
  if (length(tmp) > 0) { # the argument is within the basic arguments of GGIR()
    definitions = grep("\\item", basic_args, fixed = TRUE)
    def0 = tmp +  1
    def1 = (definitions[which(definitions == tmp) + 1]) - 1
    
    def = paste(basic_args[def0:def1], collapse = " ")
  } else { # argument is in details (parameter objects)
    definitions = grep("\\item_", parameters, fixed = TRUE)
    def0 = tmp2
    def1 = (definitions[which(definitions == tmp2) + 1]) - 1
    
    if (def0 == max(definitions)) { # last parameter in GGIR.Rd
      def1_tmp = grep("__\\\\", parameters[(def0 + 1):length(parameters)])
      def1 = def0 + def1_tmp[1] - 1
    }
    
    if (is.na(def1)) {
      def1_tmp = grep("__", parameters)
      def1_tmp = which(diff(def1_tmp) > 1)
      def1 = def1_tmp[length(def1_tmp)]
    }
    
    def = paste(parameters[def0:def1], collapse = " ")
  }
  
  
  # Cleaning the definition text --------------------------------------------
  # clean argument name from definition
  def = gsub(pattern = lookfor, replacement = "", x = def, fixed = TRUE)
  
  # clean links in def
  def = gsub(pattern = "\\\\link", replacement = "", x = def)
  
  # clean \\code in def (used to mention arguments in Rd)
  def = gsub(pattern = "\\\\code", replacement = "", x = def)
  
  # clean multiple spaces in def
  def = gsub(pattern = "^ *|(?<= ) | *$", replacement = "", x = def, perl = TRUE)
  
  # clean double quotes in def
  # def = gsub(pattern = "\"", replacement = "'", x = def)
  
  # clean double underscore at the end
  def = gsub(pattern = "__", replacement = "", x = def)
  
  # clean next subsection if it is there
  if (grepl("\\\\subsection", def)) {
    stop = gregexpr("\\\\subsection", def)[[1]][1]
    def = substr(x = def, start = 1, stop = stop - 1)
  }
  
  
  # return ------------------------------------------------------------------
  return(def)
}