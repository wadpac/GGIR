# GGIR used to depend on CRAN package GENEAread as developed by Joss Langford and Zhou Fang.
# Now GENEAread is depricated the essential parts of the code has been copied to GGIR to ensure
# ongoing functionality.


#reformat.time <-
#function (t, format = "julian") 

#turns a time vector of form "2012-04-27 08:45:18:500" or "27/4/2010 08:45:18" or "08:45:22" into either the raw second, second classed as posixct, or days since epoch
#only posix deals with the time zone. The other methods work with the same time zone as the start of recording.
#now for easier dependencies we use this as a wrapper for strptime
#t1 is used to disambiguate, show be epoch seconds of first obs
parse.time <-
function (t="", format = c("seconds", "days", "POSIX"), tzone = 0, start = NULL, startmidnight = NULL) 
{
dow = NULL
format = match.arg(format)
millisec = rep(0, length(t))
offset = 0
t1= t[1]
informat = ""

#do we have time in here?
switch(length(strsplit(t1, split = ":")[[1]]), "1" = {
informat = ""
}, "2" = {
informat = "%H:%M"
}, "3" = {
informat = "%H:%M:%S"
if (length(strsplit(t1, split = "\\.")[[1]])>1){
millisec = sapply( strsplit(t, split="\\."), function(t) as.numeric( paste("0.", t[2], sep = "")))
t = sapply( strsplit(t, split="\\."), function(t) t[1])
}
}, "4" = {
informat = "%H:%M:%S"
millisec = sapply( strsplit(t, split=":"), function(t) as.numeric( paste("0.", t[4], sep = "")))
t = sapply( strsplit(t, split=":"), function(t) paste(t[1:3], collapse = ":"))
})

#do we have date?
#strip whitespace/time
t1 = strsplit(t1, split=" ")[[1]]
t1 = t1[min(which(t1 != ""))]
#mode yyyy-mm-dd
if (length(strsplit(t1, split = "-")[[1]]) > 1){
	informat = paste("%Y-%m-%d", informat, sep = " ")
} else if (length(strsplit(t1, split = "/")[[1]]) > 1){
#mode d/m/y
	if (nchar(strsplit(t1, split = "/")[[1]][3])<4){
		informat = paste("%d/%m/%y", informat, sep = " ")
	} else {
		informat = paste("%d/%m/%Y", informat, sep = " ")
	}
} else if (length(strsplit(t1, split = ":")[[1]]) > 1) {
#nothing found, first non-open field is time
#send us back to 1970
	offset = as.numeric(strptime("00:00", format = "%H:%M", tz = "UTC"))
} else {
#add some days?
	t = lapply(strsplit(t, split=" "), function(t) (t[t != ""]))
	if (!suppressWarnings(is.na(as.numeric(t[[1]][1])))){
		t1 = sapply(t, function(x) as.numeric(x[ 1]))
		t = sapply(t, function(t) t[ 2])
	} else {
		dow = sapply(t, function(x) x[1])
		t = sapply(t, function(t) t[ 2])
		t1 = 0
	}
		offset = as.numeric(strptime("00:00", format = "%H:%M", tz = "UTC")) - t1 * 24*60*60 
}

if (informat != ""){
	t = as.POSIXct(strptime(t, format = informat), tz = "UTC") 
} else {
	t = as.POSIXct(strptime("00:00", format = "%H:%M", tz = "UTC"))
}

t= t+ millisec - offset

if ((!is.null(start)) || (!is.null(startmidnight))){
	if (is.null(startmidnight)) startmidnight = floor(start/(60*60*24)) * 60*60*24
	if (is.null(start)) start = startmidnight
#resolve ambiguity
	if (is.null(dow)){
		if (t[1] < startmidnight) t= t + startmidnight
		if (t[1] < start) t= t + ceiling((start-as.numeric(t[1]))/(60*60*24)) * 60*60*24
	} else {
		#day of the week processing
		#get DOW for midnight on start
		startmidnight= as.POSIXct( floor(start/(60*60*24)) * 60*60*24 , origin = "1970-1-1", tz = "UTC")
		next_week <- as.Date(startmidnight) + 1:7
		dow = substr(tolower(dow), 1, min(nchar(dow)))
		startmidnight = as.numeric(startmidnight)
		t = t + startmidnight + (match(dow, substr(tolower(weekdays(next_week)), 1, min(nchar(dow)))) - 0) * 24*60*60
#TODO

	}
}

if (format == "seconds"){
	t = as.numeric(t)
} else if (format == "POSIX"){
	t = c(t - tzone*60*60)
} else {
	t = as.numeric(t) / (60*60*24)
}

return(t)
}
