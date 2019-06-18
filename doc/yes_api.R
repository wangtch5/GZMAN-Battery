# TODO: Add comment
# 
# Author: Ben Kornfeld
###############################################################################

###############################################################################
# Require Necessary Packages
###############################################################################
suppressMessages({
	if (!require("httr")) {
		install.packages("httr", repos = "http://cran.us.r-project.org")
	}
	require("httr")
	if (!require("dplyr")) {
		install.packages("dplyr", repos = "http://cran.us.r-project.org")
	}
	require("dplyr")
	if (!require("lubridate")) {
		install.packages("lubridate", repos = "http://cran.us.r-project.org")
	}
	require("lubridate")
	if (!require("dtplyr")) {
		install.packages("dtplyr", repos = "http://cran.us.r-project.org")
	}
	require("dtplyr")
	if (!require("RCurl")) {
		install.packages("RCurl", repos = "http://cran.us.r-project.org")
	}
	require("RCurl")
})

###############################################################################
# Define Table of "ORIG" Variables' Current Versions
###############################################################################
# orig.datatypes.file = paste0(dirname(parent.frame(2)$ofile),
# 		"/Driver Files/yes_orig_datatypes.csv")
# 
# # Load orig.fields.df of original tables
# orig.datatypes.df <<- read.csv(orig.datatypes.file, stringsAsFactors = FALSE)

###############################################################################
# yes.timeseries Function for Pulling Time Series from YES Energy
###############################################################################
yes.timeseries <- function(items, options = list(), username =
	c("bwu@guzmanenergy.com", "mkguzman@guzmanenergy.com"),
	password = c("Guzman12345", "7seesaws"), fill.orig = FALSE, objs.fixed = FALSE) {
	# items		 	=	a list containing at least one of, or both of:
	#					1. "items.list", a vector of character strings in the
	#						format "datatype:object" to be pulled
	#					AND/OR
	#					2. BOTH "datatypes", a vector of character strings of
	#						datatypes AND "objects", a vector of character
	#						strings of objects, along with optionally
	#						"combine", a character string which specifies how
	#						datatypes and objects are to be combined. If
	#						combine is not provided (or is provided and equals
	#						"product") then all possible datatype-object
	#						combinations will be pulled. If combine = "pair"
	# 						then for all i in 1:length(datatypes), datatype
	#						i will be pulled for object i, and the length of
	#						datatypes and objects must be the same
	# options		=	a list containing options to be used when pulling the
	#						data. Possible options include:
	#					1. "start.date", start date for the timeseries. Default
	#						is 30 days ago. Input as "today", or in MM/DD/YYYY,
	#						numeric or date formats
	#					2. "end.date", end date for the timeseries. Default is
	#						today. Input as "Today", or in MM/DD/YYYY, numeric
	#						or date formats.
	#					3. "agg.level", the length of time between consecutive
	#						samples in the timeseries. Possible values are
	#						5min, hour, day, week, month, year, or raw (which
	#						is the native data periodicity). Defaults to day
	#					4.	"stat", the stat returned if multiple samples exist
	#						within the agg.level specified. Can return AVG, MIN
	#						or MAX. Defaults to AVG
	#					5. "hours", (INCOMPATIBLE WITH PEAKTYPE) a comma
	#						separated list of hours for which to return data.
	#						Defaults to all hours
	#					6. "days", (INCOMPATIBLE WITH PEAKTYPE) a comma
	#						separated list of days of the week (full or
	#						abbreviated) for which to return data. Defaults
	#     					to all days.
	#					7. "peak.type", (INCOMPATIBLE WITH HOURS/DAYS) set
	#						whether to include peak, offpeak or all data.
	#						Possible values are ONPEAK, OFFPEAK, WDPEAK, WEPEAK
	#						or ALL. Defaults to ALL
	#					8. "months", a comma separated list of months (full or
	#						abbreviated) for which to return data. Defaults to
	#						all months
	#					9. "time.zone", the timezone to use. GMT, C_T, M_T,
	#						E_T, and, P_T are valid times where _ is filled
	#						with S or P
	# username		=	the username, or a vector of such, used for YES Energy
	# password		=	the password, or a vector of such, the same length as
	#					the username vector, used for YES Energy
	# fill.orig		=	a logical scalar indicating whether or not more recent
	#					data than is available for an "ORIG" forecast field
	#					for YES (which are only available in past) should
	#					be replaced with current forecasts where possible.
	# objs.fixed	=	a logical scalar indicating whether or not the names
	#					of objects input into the function (which may have an
	#					ISO appended after a semi-colon) should be preserved
	#					in column names (TRUE) or have the ISO removed (FALSE).
	
	# Create the Base URL and Max Length
	url.base =
		"https://services.yesenergy.com/PS/rest/timeseries/multiple.csv?items="
	url.base.length = nchar(url.base)
	url.max.length = 2000
	
	# Set Validation Arrays
	option.types = c("start.date", "end.date", "agg.level", "stat", "hours",
		"days", "peak.type", "months", "time.zone")
	agg.level.types = c("5min", "hour", "day", "week", "month", "year", "raw")
	stat.types = c("avg", "min", "max")
	hours.types = 1:24
	days.types = list(monday = c("monday", "mon"), tuesday = c("tuesday",
		"tues", "tue"), wednesday = c("wednseday", "wed"), thursday =
		c("thursday", "thurs", "thu"), friday = c("friday", "fri"), saturday =
		c("saturday", "sat"), sunday = c("sunday", "sun"))
	peak.type.types = c("onpeak", "offpeak", "wdpeak", "wepeak")
	months.types = list(january = c("january", "jan"), february =
		c("february", "feb"), march = c("march", "mar"), april = c("april",
		"apr"), may = "may", june = c("june", "jun"), july = c("july", "jul"),
		august = c("august", "aug"), september = c("september", "sept"),
		october = c("october", "oct"), november = c("november", "nov"),
		december = c("december", "dec"))
	time.zone.types = c("gmt", "est", "ept", "cst", "cpt", "mst", "mpt",
		"pst", "ppt")
	
	# Validate Items Provided 
	tryCatch({
		has.items.list = !is.null(items$items.list)
		has.datatypes = !is.null(items$datatypes)
		has.objects = !is.null(items$objects)
		stopifnot(has.items.list || (has.datatypes && has.objects))
	},
		error = function(e)
			stop(paste0("Either items$datatypes AND items$objects or/and ",
			"items$list must be specified"), call. = FALSE)
	)
	if (has.items.list) {
		tryCatch({
			stopifnot(is.character(items$items.list) &&
				all(grepl("^.{1,}:.{1,}$", items$items.list)))
		},
			error = function(e)
				stop(paste0("Each item in items$items.list must be of the ",
				"form 'datatype:object'"), call. = FALSE)
		)
	}
	
	# Add relevant datatype-object combinations to "items.list$items"
	if (length(items$datatypes) > 0 && length(items$objects) > 0) {
		
		# Check how to combine datatypes and objects
		if (is.null(items$combine) || tolower(items$combine) == "product") {
			
			# If combine implies a product, add every combination to items.list
			for (i in 1:length(items$objects)) {
				for (j in 1:length(items$datatypes)) {
					items$items.list = rbind(items$items.list,
						paste0(items$datatypes[j], ":", items$objects[i]))
				}
			}
			
		} else {
			
			# Otherwise, test that "combine" = "pair" and lengths are as needed
			tryCatch({
				stopifnot(tolower(items$combine) == "pair" &&
					length(items$datatypes) == length(items$objects))
				items$items.list = rbind(items$items.list,
					paste0(items$datatypes, ":", items$objects))
			},
				error = function(e) stop(paste0("\"combine\" must be equal ",
					"to \"pair\" or \"product\", and if \"combine\" = ",
					"\"pair\", items$datatypes and items$objects must be the ",
					"same length"), call. = FALSE)
			)
		}
	}
	
	# Validate All Options Provided and Convert/Create Where Necessary and
	# Append to a "Tail" of Options for the URL
	url.tail = ""
	
	tryCatch({
		stopifnot(all(is.element(names(options), option.types)))
	},
		error = function(e)
			stop("Unrecognized option specified")
	)
	if (!is.null(options$start.date)) {
		tryCatch({
			start.date.numeric = is.numeric(options$start.date)
			start.date.datefmt = class(options$start.date) == "Date"
			start.date.string = !start.date.numeric && !start.date.datefmt &&
			!is.na(as.Date(options$start.date, format = "%m/%d/%Y")) &&
				as.Date(options$start.date, format = "%m/%d/%Y") > 0
			stopifnot(length(options$start.date) == 1 && (start.date.numeric ||
				start.date.datefmt || start.date.string))
					
			if (start.date.numeric) {
				options$start.date = format(as.Date(options$start.date, origin
					= as.Date("1/1/1970", "%m/%d/%Y")), "%m/%d/%Y")
			} else if (start.date.datefmt) {
				options$start.date = format(options$start.date, "%m/%d/%Y")
			}
		},
			error = function(e)
				stop(paste0("start.date must be a number, date, or string in ",
				"format %m/%d/%Y for a date > 1/1/1970"), call. = FALSE)
		)
	} else {
		options$start.date = format(Sys.Date() - 29, "%m/%d/%Y")
	}
	url.tail = paste0(url.tail, '&startdate=', options$start.date)
	if (!is.null(options$end.date)) {
		tryCatch({
			end.date.numeric = is.numeric(options$end.date)
			end.date.datefmt = class(options$end.date) == "Date"
			end.date.string = !end.date.numeric && !end.date.datefmt &&
				!is.na(as.Date(options$end.date, format = "%m/%d/%Y")) &&
				as.Date(options$end.date, format = "%m/%d/%Y") > 0
			stopifnot(length(options$end.date) == 1 &&
				(end.date.numeric || end.date.datefmt || end.date.string))
					
			if (end.date.numeric) {
				options$end.date = format(as.Date(options$end.date, origin
					= as.Date("1/1/1970", "%m/%d/%Y")), "%m/%d/%Y")
			} else if (end.date.datefmt) {
				options$end.date = format(options$end.date, "%m/%d/%Y")
			}
		},
			error = function(e)
				stop(paste0("end.date must be a number, date, or string in ",
				"format %m/%d/%Y for a date > 1/1/1970"), call. = FALSE)
		)
	} else {
		options$end.date = format(Sys.Date(), "%m/%d/%Y")
	}
	url.tail = paste0(url.tail, '&enddate=', options$end.date)
	if (!is.null(options$agg.level)) {
		tryCatch({
			options$agg.level = tolower(options$agg.level)
			stopifnot(length(options$agg.level) == 1 &&
				is.element(options$agg.level, agg.level.types))
		},
			error = function(e)
				stop(paste0("agg.level must be one of 5min, hour, day, week, ",
				"month , year, or raw"), call. = FALSE)
		)
	} else {
		options$agg.level = "day"
	}
	url.tail = paste0(url.tail, '&agglevel=', options$agg.level)
	if (!is.null(options$stat)) {
		tryCatch({
			options$stat = tolower(options$stat)
			stopifnot(length(options$stat) == 1 &&
				is.element(options$stat, stat.types))
		},
			error = function(e)
				stop("stat must be one of avg, min, max", call. = FALSE)
		)
	} else {
		options$stat = "avg"
	}
	agg.func = c("mean", "min", "max")[is.element(c("avg", "min", "max"),
		options$stat)]
	url.tail = paste0(url.tail, '&stat=', options$stat)
	if (!is.null(options$hours)) {
		tryCatch({
			options$hours = tolower(gsub(", ", ",", options$hours))
			hours.length = nchar(options$hours)
			hours.all = options$hours == "all"
			hours.list = strsplit(options$hours, ",")
			stopifnot(hours.length > 0 && (hours.all ||
				all(is.element(hours.list, hours.types))))
			if (hours.all) {
				hours.list = 1:24
			}
			url.tail = paste0(url.tail, '&hours=', options$hours)
		},
			error = function(e)
				stop(paste0("hours must be the either 'all' or a comma or ",
				"comma-space separated list of unique numbers from 1-24"),
				call. = FALSE)
		)
	} else {
		hours.list = 1:24
	}
	if (!is.null(options$days)) {
		tryCatch({
			options$days = tolower(gsub(", ", ",", options$days))
			days.length = nchar(options$days)
			days.all = options$days == "all"
			days.list = strsplit(options$days, ",")
			days.match.total = 0
			for (I in 1:length(names(days.types))) {
				days.match.this = sum(is.element(days.list,
					days.types[[1]][I]))
				days.match.total = days.match.total + days.match.this
				stopifnot(days.match.this <= 1)
				days.list[is.element(days.list, days.types[[1]][I])] =
					names(days.types)[I]
			}
			stopifnot(days.length > 0 && (days.all || days.match.total ==
				length(days.list)))
			url.tail = paste0(url.tail, '&days=', options$days)
		},
			error = function(e)
				stop(paste0("days must be either 'all' or a comma or ",
					"comma-space separated list of unique days of the week or ",
					"abbreviated days of the week"), call. = FALSE)
		)
	} else {
		days.list = names(days.types)
	}
	if (!is.null(options$peak.type)) {
		tryCatch({
			options$peak.type = tolower(options$peak.type)
			stopifnot(is.null(options$hours) && is.null(options$days) &&
				length(options$peak.type) == 1 && (options$peak.type == "all"
				|| is.element(options$peak.type, peak.type.types)))
			if (options$peak.type == "onpeak") {
				hours.list = 7:22
			} else if (options$peak.type == "offpeak") {
				hour.list = c(1:6, 23:24)
			} else if (options$peak.type == "wdpeak") {
				hours.list = 7:22
				days.list = c("monday", "tuesday", "wednesday", "thursday",
					"friday")
			} else if (options$peak.type == "wepeak") {
				hours.list = 7:22
				days.list = c("saturday", "sunday")
			}
			url.tail = paste0(url.tail, '&peaktype=', options$peak.type)
		},
			error = function(e)
				stop(paste0("peak.type must be one of onpeak, offpeak, ",
				"wdpeak, wepeak, or all, and cannot be specified if hours ",
				"or days is"), call. = FALSE)
		)
	}
	hours.list = 1:24
	if (!is.null(options$months)) {
		tryCatch({
			options$months = tolower(gsub(", ", ",", options$months))
			months.length = nchar(options$months)
			months.all = options$months == "all"
			months.list = strsplit(options$months, ",")
			months.match.total = 0
			for (I in 1:length(names(months.types))) {
				months.match.this = sum(is.element(months.list,
					months.types[[I]]))
				months.match.total = months.match.total + months.match.this
				stopifnot(months.match.this <= 1)
				months.list[is.element(months.list, months.types[[1]][I])] =
					names(months.types)[I]
			}
			stopifnot(months.length > 0 && (months.all || months.match.total ==
				length(months.list)))
			url.tail = paste0(url.tail, '&months=', options$months)
		},
			error = function(e)
				stop(paste0("months must be either 'all' or a comma or ",
					"comma-space separated list of unique months of the year or ",
					"abbreviated months of the year"), call. = FALSE)
		)
	} else {
		months.list = c("january", "february", "march", "april", "may", "june",
			"july", "august", "september", "october", "november", "december")
	}
	if (!is.null(options$time.zone)) {
		tryCatch({
			options$time.zone = tolower(options$time.zone)
			stopifnot(length(options$time.zone) == 1 &&
				is.element(options$time.zone, time.zone.types))
			url.tail = paste0(url.tail, '&timezone=', options$time.zone)
		},
			error = function(e)
				stop(paste0("time.zone must be one of gmt, est, ept, cst, ",
				"cpt, mst, mpt, pst, ppt"), call. = FALSE)
		)
	}
	url.tail.length = nchar(url.tail)
	
	# Validate username and password
	tryCatch({
		stopifnot(is.character(username) && is.character(password) &&
			length(username) == length(password) && length(username) >= 1)		
	},
		error = function(e)
			stop(paste0("username and password must be character vectors of ",
			"equal length (length may be 1 - scalar)"), call. = FALSE)
	)
	
	# Create all datetimes
	start.date.num = as.POSIXlt(options$start.date, format = "%m/%d/%Y",
		tz = "GMT")
	end.date.num = as.POSIXlt(options$end.date, format = "%m/%d/%Y",
		tz = "GMT")
	if (options$agg.level == "5min") {
		posix.interval = 5 * 60
		start.date.num = start.date.num + posix.interval
		end.date.num = end.date.num + 24 * 60 * 60
		all.datetimes = seq(start.date.num, end.date.num, by =
			posix.interval)
		all.datetimes = format(all.datetimes, "%m/%d/%Y %H:%M:%S")
	} else if (options$agg.level == "hour") {
		posix.interval = 60 * 60
		start.date.num = start.date.num + posix.interval
		end.date.num = end.date.num + 24 * 60 * 60
		all.datetimes = seq(start.date.num, end.date.num, by =
			posix.interval)
		all.datetimes = format(all.datetimes, "%m/%d/%Y %H:%M:%S")
	} else if (options$agg.level == "day") {
		posix.interval = 24 * 60 * 60
		all.datetimes = seq(start.date.num, end.date.num, by =
			posix.interval)
		all.datetimes = format(all.datetimes, "%m/%d/%Y %H:%M:%S")
	} else if (options$agg.level == "week") {
		posix.interval = 7 * 24 * 60 * 60
		start.date.num = start.date.num - 24 * 60 * 60 *
			(as.numeric(format(start.date.num, "%u")) - 1)
		end.date.num = end.date.num - 24 * 60 * 60 *
			(as.numeric(format(end.date.num, "%u")) - 1)
		all.datetimes = seq(start.date.num, end.date.num, by =
			posix.interval)
		all.datetimes = format(all.datetimes, "%m/%d/%Y %H:%M:%S")
	} else if (options$agg.level == "month") {
		start.year = as.numeric(format(start.date.num, "%Y"))
		start.month = as.numeric(format(start.date.num, "%m"))
		end.year = as.numeric(format(end.date.num, "%Y"))
		end.month = as.numeric(format(end.date.num, "%m"))
		total.months.seq = seq(start.year * 12 + start.month,
			end.year * 12 + end.month, by = 1) - 1
		months.seq = (total.months.seq %% 12) + 1
		years.seq =  floor(total.months.seq / 12)
		all.datetimes = ISOdate(years.seq, months.seq, 1, 0, 0, 0)
		all.datetimes = format(all.datetimes, "%m/%d/%Y %H:%M:%S")
	} else if (options$agg.level == "raw") {
		start.year = as.numeric(format(start.date.num, "%Y"))
		end.year = as.numeric(format(end.date.num, "%Y"))
		all.datetimes = ISOdate(0[0], 1, 1, 1, 1, 1)
		all.datetimes = format(all.datetimes, "%m/%d/%Y %H:%M:%S")
	}
	
	# Pre-allocate matrix to hold results
	yes.data.list = vector('list', length(items$items.list))
	for (i in 1:length(yes.data.list)) {
		yes.data.list[[i]] = rep(NA, length(all.datetimes))
	}
	
	# Determine Loop Size for Next Step
	if (options$agg.level == "5min" && end.date.num -
		start.date.num > 10) {
		loop.num = 10
	} else if (options$agg.level == "hour" && end.date.num -
		start.date.num > 365) {
		loop.num = 20
	} else {
		loop.num = 75
	}
	
	# Loop Over Items to Ensure the URL Does not Get Excessively Long
	i = 1
	user.num = ceiling(runif(1) * length(username))
	col.names = c()
	while (i <= length(items$items.list)) {
		
		# Message the user what item # the API is on
		message(paste0(i, " out of ", length(items$items.list)))
		j = i
		
		# Add All Items to the Url then Trim Trailing Comma and Add Tail
		url = url.base
		i.last = i
		while (nchar(url) + nchar(items$items.list[i]) <= url.max.length -
			url.tail.length - url.base.length && i - i.last < loop.num &&
			i <= length(items$items.list)) {
			
			url = paste0(url, gsub("&", "%26", items$items.list[i]), ",")
			i = i + 1
		}
		url = paste0(substr(url, 1, nchar(url) - 1), url.tail)
		url = gsub(" ", "%20", url)
		print("b")
		# Pull the Table from the API
		message ("Hitting Yes...")
		tmp.start.time = Sys.time()
	
		yes.text = content(GET(url, authenticate(user = username[user.num],
			password = password[user.num])), as = "text")
		
		temp.yes.data = read.csv(text = yes.text,
			check.names = FALSE, stringsAsFactors = FALSE)
		tmp.end.time = Sys.time()
		message(paste("Total Elapsed Time:", format(.POSIXct(difftime(tmp.end.time,
			tmp.start.time, units="secs"), tz="GMT"), "%H:%M:%S")))
		
		# Aggregate by Datetimes for Daylight Savings
		message("Aggregating...")
		tmp.start.time = Sys.time()
		
		# Drop Columns That Are Irrelevant
		temp.yes.data = temp.yes.data[, !grepl(x = names(temp.yes.data),
			pattern = "^(HOURENDING|MARKETDAY|PEAKTYPE|MONTH|YEAR)$")]
		
		tryCatch({
			if (nrow(temp.yes.data) > 0) {
				yes.item.column.names = names(temp.yes.data)[
					names(temp.yes.data) != "DATETIME"]
				temp.yes.data = aggregate(temp.yes.data[, names(temp.yes.data)
					!= "DATETIME"], by = list(DATETIME = temp.yes.data$DATETIME
					), FUN = agg.func, na.action = na.pass, na.rm = TRUE)
				names(temp.yes.data)[names(temp.yes.data) != "DATETIME"] =
					yes.item.column.names
				for (k in 2:ncol(temp.yes.data)) {
					temp.yes.data[is.nan(temp.yes.data[, k]), k] = NA
				}
			} else {
				temp.yes.data$DATETIME = as.character(temp.yes.data$DATETIME)
			}
		},
			error = function(e) {
				stop(temp.yes.data, call. = FALSE)
		})
		
		tmp.end.time = Sys.time()
		message (paste("Total Elapsed Time:", format(.POSIXct(difftime(
			tmp.end.time, tmp.start.time, units="secs"), tz="GMT"),
			"%H:%M:%S")))
		
		# Merge With Existing Data
		# Use join with datetimes as first argument to maintain order of
		# datetimes
		temp.yes.data = full_join(data.frame(DATETIME = all.datetimes,
			stringsAsFactors = FALSE), temp.yes.data, by = "DATETIME")
		tmp.col.names = names(temp.yes.data)[names(temp.yes.data) !=
			"DATETIME"]
		
		# We save all datetimes if agg.level is raw
		if (options$agg.level == "raw") {
			for (k in 1:length(tmp.col.names)) {
				yes.data.list[[j + k - 1]] = temp.yes.data[c("DATETIME",
					tmp.col.names[k])]
			}
		} else {
			for (k in 1:length(tmp.col.names)) {
				yes.data.list[[j + k - 1]] = temp.yes.data[tmp.col.names[k]]
			}		
		}
		
		gc(reset = T)
		col.names = c(col.names, tmp.col.names)
		
		message (paste0(memory.size(F), " Mb / ", memory.size(T), " Mb"))
		
		# Iterate the username
		user.num = (user.num %% length(username)) + 1
	}
	
	# Make yes.data.list into the yes.data data-frame we all know and love
	if (options$agg.level != "raw") {
		yes.data = as.data.frame(yes.data.list)
		names(yes.data) = col.names
		yes.data$DATETIME = all.datetimes
		
		# We do this differently if agg.level is raw due to the lack of a standard
		# all.datetimes vector
	} else {
		merge.func = function(...) join(..., by = "DATETIME", type = "full")
		yes.data = Reduce(merge.func, yes.data.list, data.frame(DATETIME =
			all.datetimes, stringsAsFactors = FALSE))
		yes.data = yes.data[order(yes.data$DATETIME), ]
	}
	# re-arrange columns to make DATETIME first
	# AXG 2015.11.14 It's unclear if this is necessary but this was how the format
	# before, so I'm not sure if something else may have depended on this format
	yes.data = yes.data[c("DATETIME", col.names)]
	
	# Change Datetime to Correct Format and Case and Keep Only Relevant Times
	yes.data$date = as.Date(substring(yes.data$DATETIME, 1, 10),
		format = "%m/%d/%Y")
	yes.data$hour = substring(yes.data$DATETIME, 12, 13)
	yes.data$minutes = substring(yes.data$DATETIME, 15, 16)
	yes.data$seconds = substring(yes.data$DATETIME, 18, 19)
	
	if (nrow(yes.data) > 0) {
		if (is.element(options$agg.level, c("raw", "5min", "hour"))) {
			
			yes.data$hour[yes.data$minutes != "00" | yes.data$seconds !=
				"00"] = sprintf("%2.2i", as.numeric(yes.data$hour[
				yes.data$minutes != "00" | yes.data$seconds != "00"]) + 1)
			yes.data$date[yes.data$hour == "00" & yes.data$minutes == "00" &
				yes.data$seconds == "00"] = yes.data$date[yes.data$hour ==
				"00" & yes.data$minutes == "00" & yes.data$seconds == "00"] - 1
			yes.data$hour[yes.data$hour == "00" & yes.data$minutes == "00"
				& yes.data$seconds == "00"] = "24"
			
			yes.data = yes.data[is.element(as.numeric(yes.data$hour),
				hours.list) & is.element(tolower(weekdays(yes.data$date)),
				days.list) & is.element(tolower(format(yes.data$date,
				format = "%B")), months.list), ]
			
			if (options$agg.level == "raw") {
				row.names(yes.data) = paste0(format(yes.data$date,
					"%Y-%m-%d HE"), yes.data$hour, " M", yes.data$minutes, 
					" S", yes.data$seconds)
			} else if (options$agg.level == "5min") {
				row.names(yes.data) = paste0(format(yes.data$date,
					"%Y-%m-%d HE"), yes.data$hour, " M", yes.data$minutes)
			} else if (options$agg.level == "hour") {
				row.names(yes.data) = paste0(format(yes.data$date,
					"%Y-%m-%d HE"), yes.data$hour)
			}
			
		} else if (is.element(options$agg.level, c("day", "week", "month"))) {
			
			if (is.element(options$agg.level, c("day", "week"))) {
				yes.data = yes.data[is.element(tolower(weekdays(
					yes.data$date)), days.list), ]
			}
			
			yes.data = yes.data[is.element(tolower(format(yes.data$date,
				format = "%B")), months.list), ]
			row.names(yes.data) = format(yes.data$date, "%Y-%m-%d")			
		}
	}
	
	yes.data$DATETIME = NULL
	yes.data$date = NULL
	yes.data$hour = NULL
	yes.data$minutes = NULL
	yes.data$seconds = NULL
	
	# Initialize lists to hold all datatypes and objects
	datatypes = rep("", length(items$items.list))
	obj.vec = rep("", length(items$items.list))
	
	# Rename the Other Columns in Line with Convention
	for (i in 1:length(items$items.list)) {
		
		# Find the datatype and object
		datatype = gsub("^([^:]+):[^:]+[:]?.*$", "\\1",
			items$items.list[i])
		object = gsub("^[^:]+:([^:]+)[:]?.*$", "\\1", items$items.list[i])
		object.fixed = gsub("^[^:]+:([^:]+[:]?.*)$", "\\1",
			items$items.list[i])
		
		# Find the index of the match
		object.regex = gsub("\\(", "\\\\(", gsub("\\)", "\\\\)", object))
		pattern = paste0("^", object.regex, "( \\(NODE\\))? \\(", datatype, "\\)$")
		if (!objs.fixed) {
			names.match = grepl(pattern, names(yes.data), ignore.case = T)
			names(yes.data)[names.match] = paste0(datatype, ".", object)
		} else {
			names.match = grepl(pattern, names(yes.data), ignore.case = T)
			names(yes.data)[names.match] =	paste0(datatype, ".", object.fixed)
		}
		
		# Store object and datatype in vectors
		datatypes[names.match] = datatype
		obj.vec[names.match] = object.fixed
	}
	
	# Check if we should fill in the "orig" data
	if (fill.orig) {
		
		# Replace orig.datatypes with current alternative (find orig data)
		orig.data.idx = match(datatypes,orig.datatypes.df$orig.datatype)
		replace.idx = which(orig.data.idx & is.na(yes.data[nrow(yes.data), ]))
		orig.data.idx = orig.data.idx[replace.idx]
		
		# Check if anything needs replacement
		if (as.Date(options$end.date, format = "%m/%d/%Y") >= Sys.Date() &
			length(replace.idx) > 0) {
			
			# Get the new datatypes to get
			new.data = orig.datatypes.df$current.datatype[orig.data.idx]
			obj.vec = obj.vec[replace.idx]
			
			# Call the replacements
			cur.options = options
			cur.options$start.date = format(Sys.Date(), format = "%m/%d/%Y")
			cur.items = list(items.list = paste0(new.data, ":", obj.vec))
			cur.yes.data = suppressMessages({
				yes.timeseries(cur.items, cur.options, username = username,
				password = password, objs.fixed = objs.fixed)})
			
			# Replace the NA segments of the yes.data with the current data
			min.rep.row = match(rownames(cur.yes.data)[1], rownames(yes.data))
			for (i in 1:length(replace.idx)) {
				
				# Find where the replacement should start
				first.rep.row = tail(which(!is.na(yes.data[, replace.idx[i]])),
					1) + 1
				
				# Perform the replacement if this is >= min.rep.row
				if (first.rep.row >= min.rep.row) {
					
					# Find the datatype and object
					datatype = new.data[i]
					if (!objs.fixed) {
						object = gsub("^[^:]+:([^:]+)[:]{0,1}.*$", "\\1",
							cur.items$items.list[i])
					} else { 
						object = gsub("^[^:]+:([^:]+[:]{0,1}.*)$", "\\1",
							cur.items$items.list[i])
					}
					object.regex = gsub("\\(", "\\\\(", gsub("\\)", "\\\\)",
						object))
					pattern = paste0("^", datatype, "\\.", object.regex, "$")
					names.match = which(grepl(pattern, names(cur.yes.data),
						ignore.case = T))[1]
					
					# Isolate the replacement rows
					rep.rows = rownames(yes.data)[first.rep.row:nrow(yes.data)]
					rep.row.idx = match(rep.rows, rownames(cur.yes.data))
					yes.data[first.rep.row:nrow(yes.data), replace.idx[i]] =
						cur.yes.data[rep.row.idx, names.match]
				}
			}
		}
	}
	
	# Return the Table
	return(yes.data)
}

###############################################################################
# yes.datatype Function for Search for Datatypes Available in YES Energy
###############################################################################
yes.datatype <- function(iso = "", object = "", search = "", username =
	"data2@guzmanenergy.com", password = "Brookdale1") {
	# iso			=	the iso, as a character string, for which datatypes
	#					available for it are to be returned
	# object		=	the object (INCOMPATIBLE WITH SEARCH), as a character
	#					string, for which datatypes available for it are to be
	#					returned
	# search		=	a search term (INCOMPATIBLE WITH OBJECT), as a
	#					character string, which will be used to narrow the
	#					datatypes returned based on matching the search
	# username		=	the username used to sign into YES Energy
	# password		=	the password used to sign into YES Energy
	
	url = "https://services.yesenergy.com/PS/rest/timeseries/datatypes.csv?"
	
	# Validate Inputs, Append to the URL and Remove Trailing &
	if (nchar(iso) > 0) {
		tryCatch({
			stopifnot(length(object) == 1 && is.character(object))
		},
			error = function(e)
				stop("iso must be a single character string", call. = FALSE)
		)
		url = paste0(url, "iso=", iso, "&")	
	}
	if (nchar(object) > 0) {
		has.object = TRUE
		tryCatch({
			stopifnot(length(object) == 1 && is.character(object))
		},
			error = function(e)
				stop("object must be a single character string", call. = FALSE)
		)
		url = paste0(url, "object=", object, "&")		
	} else {
		has.object = FALSE
	}
	if (nchar(search) > 0) {
		has.search = TRUE
		tryCatch({
			stopifnot(length(object) == 1 && is.character(object))
		},
			error = function(e)
				stop("search must be a single character string", call. = FALSE)
		)
		url = paste0(url, "search=", search, "&")	
	} else {
		has.search = FALSE
	}
	tryCatch({
		stopifnot(!has.object || !has.search)
	},
		error = function(e)
			stop(paste0("Cannot specify search and object for the same ",
			"datatype query"), call. = FALSE)
	)
	substr(url, 1, nchar(url) - 1)
	
	# Validate username and password
	tryCatch({
		stopifnot(is.character(username) && is.character(password) &&
			length(username) == 1 && length(password) == 1)		
	},
		error = function(e)
			stop("username and password must be character scalars",
			call. = FALSE)
	)
	
	# Pull Data from YES and Modify It To Replace Commas and Newlines Intended
	# To Be Part of Text With Other Characters
	yes.text = getURL(url, userpwd = paste0(username, ":", password),
		ssl.verifypeer = FALSE)
	yes.text = gsub(",[ abcdefghijklmnopqrstuvwxyz0123456789]{1}", "~ ",
		yes.text)
	yes.text = gsub("\"", "", yes.text)
	yes.text =
		gsub("\r\n(?!Fuel|Generation|Load|Prices|Transmission|Weather)", " ",
		yes.text, perl = TRUE)
	yes.text =
		gsub("\n(?!Fuel|Generation|Load|Prices|Transmission|Weather)", " ",
		yes.text, perl = TRUE)
	yes.text =
		gsub("\r(?!\n|Fuel|Generation|Load|Prices|Transmission|Weather)", " ",
		yes.text, perl = TRUE)
	
	# Parse YES Data
	yes.data = read.table(text = yes.text, header = TRUE, sep = ",",
		comment.char = "", quote = "")
	yes.data[, ] = gsub("~ ", ", ", as.matrix(yes.data[, ]))
	
	# Sort YES Data
	yes.data = yes.data[order(yes.data$CATEGORY, yes.data$SUBCATEGORY,
		yes.data$DATATYPE), ]
	row.names(yes.data) = 1:nrow(yes.data)
	
	# Return the Table
	return(yes.data)
	
}

###############################################################################
# yes.object Function for Search for Objects Available in YES Energy
###############################################################################
yes.object <- function(iso = "", username = "bwu@guzmanenergy.com",
	password = "Guzman12345") {
	# object		=	the datatype, as a character string, for which objects
	#					with it as an available datatype are to bereturned
	# iso			=	the iso, as a character string, for which objects in it
	#					available for the datatype specified are to be returned
	# username		=	the username used to sign into YES Energy
	# password		=	the password used to sign into YES Energy
	
	url = "https://services.yesenergy.com/PS/rest/objects/pricenodes/"
	
	# Validate Inputs, Append to the URL
	if (nchar(iso) > 0) {
		tryCatch({
					stopifnot(length(iso) == 1 && is.character(iso))
				},
				error = function(e)
					stop("iso must be a single character string", call. = FALSE)
		)
		url = paste0(url, iso,".csv")	
	}
	
	# Validate username and password
	tryCatch({
		stopifnot(is.character(username) && is.character(password) &&
			length(username) == 1 && length(password) == 1)		
	},
		error = function(e)
			stop("username and password must be character scalars",
			call. = FALSE)
	)
	
	message(url)
	# Pull Data from YES and Modify It To Replace Commas and Newlines Intended
	# To Be Part of Text With Other Characters
	yes.data = read.table(text = getURL(url, userpwd = paste0(username, ":",
		password), ssl.verifypeer = FALSE), header = TRUE, sep = ",",
		comment.char = "", quote = "")
	
	# Return the Table
	return(yes.data)
}

###############################################################################
# yes.object Function for Search for Fuel Type of Price Node in YES Energy
###############################################################################
yes.object.gen.units <- function(iso = "", username = "bwu@guzmanenergy.com",
		password = "Guzman12345") {
	# object		=	the datatype, as a character string, for which objects
	#					with it as an available datatype are to bereturned
	# iso			=	the iso, as a character string, for which objects in it
	#					available for the datatype specified are to be returned
	# username		=	the username used to sign into YES Energy
	# password		=	the password used to sign into YES Energy
	
	url = "https://services.yesenergy.com/PS/rest/objects/genunits/"
	
	# Validate Inputs, Append to the URL
	if (nchar(iso) > 0) {
		tryCatch({
					stopifnot(length(iso) == 1 && is.character(iso))
				},
				error = function(e)
					stop("iso must be a single character string", call. = FALSE)
		)
		url = paste0(url, iso,".csv")	
	}
	
	# Validate username and password
	tryCatch({
				stopifnot(is.character(username) && is.character(password) &&
								length(username) == 1 && length(password) == 1)		
			},
			error = function(e)
				stop("username and password must be character scalars",
						call. = FALSE)
	)
	
#	message(url)
	# Pull Data from YES and Modify It To Replace Commas and Newlines Intended
	# To Be Part of Text With Other Characters
	
	yes.text <- content(GET(url, authenticate(user = username,
							password = password)), as = "text")
	yes.data <- read.table(text = yes.text, header = TRUE, sep = ",",
			check.names = FALSE, stringsAsFactors = FALSE, fill=NA)
	
	# Return the Table
	return(yes.data)
}

###############################################################################
# long.hourly.yes.data Function to Return a yes.timeseries in Long Format
###############################################################################
long.hourly.yes.data <- function(yes.ts) {
	
	# Save as a data frame with an added date and he column
	yes.data = yes.ts
	date.hes = rownames(yes.data)
	yes.data$date = as.Date(substring(date.hes, 1, 10))
	yes.data$he = as.numeric(substring(date.hes, 14, 15))
	
	# Transform the data into long form
	yes.df = melt(yes.data, id.vars = c("date", "he"))
	yes.df$variable = as.character(yes.df$variable)
	
	# Split into node and datatype
	yes.df$node = str_split_fixed(yes.df$variable, "\\.", 2)[, 2]
	yes.df$var = str_split_fixed(yes.df$variable, "\\.", 2)[, 1]
	yes.df$variable = NULL
	
	# Cast the data and return it
	yes.df = dcast(data = yes.df, formula = node + date + he ~ var,
		value.var = "value")
	return(yes.df)
}

###############################################################################
# yes.convert.datetime Function to Transfor Raw YES Datetimes to Desired Format
###############################################################################
yes.convert.datetime <- function(datetimes, agg.level) {
	
	# Break datetime into substituent parts
	datetimes = as.character(datetimes)
	dates = as.Date(substr(datetimes, 1, 10), format = "%m/%d/%Y")
	hours = substr(datetimes, 12, 13)
	minutes = substr(datetimes, 15, 16)
	seconds = substr(datetimes, 18, 19)
	
	# Only modify if length > 0
	if (length(datetimes) == 0) return(datetimes)

	# The format depends on the agg level
	if (agg.level %in% c("raw", "5min", "hour")) {
		
		idx = minutes != "00" | seconds != "00"
		hours[idx] = sprintf("%2.2i", as.numeric(hours[idx]) + 1)
		idx = hours == "00" & minutes == "00" & seconds == "00"
		dates[idx] = dates[idx] - 1
		hours[hours == "00" & minutes == "00" & seconds == "00"] = "24"
		
		# Format depending on agg.level
		dates = format(dates, "%Y-%m-%d HE")
		if (agg.level == "raw") {
			datetimes = paste0(dates, hours,
				" M", minutes, " S", seconds)
		} else if (agg.level == "5min") {
			datetimes = paste0(dates, hours, " M", minutes)
		} else if (agg.level == "hour") {
			datetimes = paste0(dates, hours)
		}
		
	} else if (agg.level %in% c("day", "week", "month", "year")) {
		
		# Save new datetimes
		datetimes = format(dates, "%Y-%m-%d")
	}
	
	# Return datetimes
	return(datetimes)
}

###############################################################################
# yes.fc.vintage.ts Function to Return YES Forecast Vintage Results
###############################################################################
yes.fc.vintage.ts <- function(items, options = list(),
	username = c("data1@guzmanenergy.com", "data2@guzmanenergy.com"),
	password = rep("Brookdale1", 2), file.name = NULL, extra.saves = NULL) {
	# items		 	=	a list containing at least one of, or both of:
	#					1.	"items.list", a vector of character strings in the
	#						format "datatype:object" to be pulled
	#					AND/OR
	#					2.	BOTH "datatypes", a vector of character strings of
	#						datatypes AND "objects", a vector of character
	#						strings of objects, along with optionally
	#						"combine", a charcter string which specifies how
	#						datatypes and objects are to be combined. If
	#						combine is not provided (or is provided and equals
	#						"product") then all possible datatype-object
	#						combinations will be pulled. If combine = "pair"
	# 						then for all i in 1:length(datatypes), datatype
	#						i will be pulled for object i, and the length of
	#						datatypes and objects must be the same
	# options		=	a list containing options to be used when pulling the
	#						data. Possible options include:
	#					1.	"start.date", start date for the timeseries. Default
	#						is 30 days ago. Input as "today", or in MM/DD/YYYY,
	#						numeric or date formats
	#					2.	"end.date", end date for the timeseries. Default is
	#						today. Input as "Today", or in MM/DD/YYYY, numeric
	#						or date formats.
	#					3.	"as.of.days.before", the number of days before the
	#						date forecast on which to collect the forecast.
	#						This is required.
	#					4.	"as.of.time", the time of day (in the same
	#						time.zone specified below) at which the forecasts
	#						are to be collected. This is required.
	#					5.	"agg.level", the length of time between consecutive
	#						samples in the timeseries. Possible values are
	#						5min, hour, day, week, month, year, or raw (which
	#						is the native data periodicity). Defaults to day
	#					6.	"stat", the stat returned if multiple samples exist
	#						within the agg.level specified. Can return AVG, MIN
	#						or MAX. Defaults to AVG
	#					7.	"hours", (INCOMPATIBLE WITH PEAKTYPE) a comma
	#						separated list of hours for which to return data.
	#						Defaults to all hours
	#					8.	"days", (INCOMPATIBLE WITH PEAKTYPE) a comma
	#						separated list of days of the week (full or
	#						abbreviated) for which to return data. Defaults
	#						to all days.
	#					9.	"peak.type", (INCOMPATIBLE WITH HOURS/DAYS) set
	#						whether to include peak, offpeak or all data.
	#						Possible values are ONPEAK, OFFPEAK, WDPEAK, WEPEAK
	#						or ALL. Defaults to ALL
	#					10.	"months", a comma separated list of months (full or
	#						abbreviated) for which to return data. Defaults to
	#						all months
	#					11.	"time.zone", the timezone to use. GMT, C_T, M_T,
	#						E_T, and, P_T are valid times where _ is filled
	#						with S or P. The default is EPT.
	# username		=	the username, or a vector of such, used for YES Energy
	# password		=	the password, or a vector of such, the same length as
	#					the username vector, used for YES Energy
	# file.name		= 	a character string containing the name of the file from
	#					which existing YES data should be pulled and which
	#					new data will be saved to.
	# extra.saves	= 	a character string containing the names of additional
	# 					files from which existing YES data should be pulled as
	#					backup and which new data will be saved to.
	
	###########################################################################
	# Set Hard-Coded Lists for Future Validation
	###########################################################################
	# Create the base URL and set max length
	url.base =
		"https://services.yesenergy.com/PS/rest/timeseries/multiple.csv?items="
	url.base.length = nchar(url.base)
	url.max.length = 2000
	
	# Set validation arrays for options
	option.types = c("start.date", "end.date", "as.of.days.before",
		"as.of.time", "agg.level", "stat", "hours", "days", "peak.type",
		"months", "time.zone")
	agg.level.types = c("5min", "hour", "day", "week", "month", "year", "raw")
	stat.types = c("avg", "min", "max")
	hours.types = 1:24
	days.types = list(monday = c("monday", "mon"), tuesday = c("tuesday",
		"tues", "tue"), wednesday = c("wednseday", "wed"), thursday =
		c("thursday", "thurs", "thu"), friday = c("friday", "fri"), saturday =
		c("saturday", "sat"), sunday = c("sunday", "sun"))
	peak.type.types = c("onpeak", "offpeak", "wdpeak", "wepeak")
	months.types = list(january = c("january", "jan"), february =
		c("february", "feb"), march = c("march", "mar"), april = c("april",
		"apr"), may = "may", june = c("june", "jun"), july = c("july", "jul"),
		august = c("august", "aug"), september = c("september", "sept"),
		october = c("october", "oct"), november = c("november", "nov"),
		december = c("december", "dec"))
	time.zone.conversions = list(gmt = "GMT", est = "Etc/GMT+5",
		ept = "EST5EDT", cst = "Etc/GMT+6", cpt = "CST6CDT", mst = "Etc/GMT+7",
		mpt = "MST7MDT", pst = "Etc/GMT+8", ppt = "PST8PDT")
	time.zone.types = names(time.zone.conversions)
	
	###########################################################################
	# Validate Items Submitted
	###########################################################################
	# Validate items provided are of types accepted/required
	tryCatch({
		has.items.list = !is.null(items$items.list)
		has.datatypes = !is.null(items$datatypes)
		has.objects = !is.null(items$objects)
		stopifnot(has.items.list || (has.datatypes && has.objects))
	},
		error = function(e)
			stop(paste0("Either items$datatypes AND items$objects or/and ",
			"items$items.list must be specified"), call. = FALSE)
	)
	
	# Validate items list if that is form provided
	if (has.items.list) {
		tryCatch({
			stopifnot(is.character(items$items.list) &&
				all(grepl("^.+:.+$", items$items.list)))
		},
			error = function(e)
				stop(paste0("Each item in items$items.list must be of the ",
				"form 'datatype:object'"), call. = FALSE)
		)
	}
	
	# Add relevant datatype-object combinations to "items.list$items"
	if (length(items$datatypes) > 0 && length(items$objects) > 0) {
		
		# Check how to combine datatypes and objects
		if (is.null(items$combine) || tolower(items$combine) == "product") {
			
			# If combine implies a product, add every combination to items.list
			for (i in 1:length(items$objects)) {
				for (j in 1:length(items$datatypes)) {
					items$items.list = rbind(items$items.list,
						paste0(items$datatypes[j], ":", items$objects[i]))
				}
			}
			
		} else {
			
			# Otherwise, test that "combine" = "pair" and lengths are as needed
			tryCatch({
				stopifnot(tolower(items$combine) == "pair" &&
					length(items$datatypes) == length(items$objects))
				items$items.list = rbind(items$items.list,
					paste0(items$datatypes, ":", items$objects))
			},
				error = function(e) stop(paste0("\"combine\" must be equal ",
					"to \"pair\" or \"product\", and if \"combine\" = ",
					"\"pair\", items$datatypes and items$objects must be the ",
					"same length"), call. = FALSE)
			)
		}
	}
	items.list = items$items.list
	
	###########################################################################
	# Validate Options Specified
	###########################################################################
	# Validate options and (convert/create if needed) and append to URL tail
	url.tail = ""
	
	# Check that all options are of the type required
	tryCatch({
		stopifnot(all(names(options) %in% option.types))
	},
		error = function(e)
			stop("Unrecognized option specified")
	)
	
	# Parse start and end dates
	if (!is.null(options$start.date)) {
		tryCatch({
				
			# Check start date is one of: numeric, date, or string
			start.date.numeric = is.numeric(options$start.date)
			start.date.datefmt = class(options$start.date) == "Date"
			start.date.string = !start.date.numeric && !start.date.datefmt &&
				!is.na(as.Date(options$start.date, format = "%m/%d/%Y")) &&
				as.Date(options$start.date, format = "%m/%d/%Y") > 0
			stopifnot(length(options$start.date) == 1 && (start.date.numeric ||
				start.date.datefmt || start.date.string))
			
			# Format as "%m/%d/%Y"
			if (start.date.numeric) {
				options$start.date = format(as.Date(options$start.date, origin
					= as.Date("1/1/1970", "%m/%d/%Y")), "%m/%d/%Y")
			} else if (start.date.datefmt) {
				options$start.date = format(options$start.date, "%m/%d/%Y")
			}
			
			# Create POSIX form
			start.date.posix = as.POSIXct(options$start.date, "%m/%d/%Y",
				tz = "GMT")
		},
			error = function(e)
				stop(paste0("start.date must be a number, date, or string in ",
				"format %m/%d/%Y for a date > 1/1/1970"), call. = FALSE)
		)
	} else {
		options$start.date = format(Sys.Date() - 29, "%m/%d/%Y")
	}
	if (!is.null(options$end.date)) {
		tryCatch({
				
			# Check end date is one of: numeric, date, or string
			end.date.numeric = is.numeric(options$end.date)
			end.date.datefmt = class(options$end.date) == "Date"
			end.date.string = !end.date.numeric && !end.date.datefmt &&
				!is.na(as.Date(options$end.date, format = "%m/%d/%Y")) &&
				as.Date(options$end.date, format = "%m/%d/%Y") > 0
			stopifnot(length(options$end.date) == 1 &&
				(end.date.numeric || end.date.datefmt || end.date.string))
		
			# Format as "%m/%d/%Y"
			if (end.date.numeric) {
				options$end.date = format(as.Date(options$end.date, origin
					= as.Date("1/1/1970", "%m/%d/%Y")), "%m/%d/%Y")
			} else if (end.date.datefmt) {
				options$end.date = format(options$end.date, "%m/%d/%Y")
			}
			
			# Create POSIX form
			end.date.posix = as.POSIXct(options$end.date, "%m/%d/%Y",
				tz = "GMT")
		},
			error = function(e)
				stop(paste0("end.date must be a number, date, or string in ",
				"format %m/%d/%Y for a date > 1/1/1970"), call. = FALSE)
		)
	} else {
		options$end.date = format(Sys.Date(), "%m/%d/%Y")
	}
	
	# Check that as.of.days.before is a numeric integer (or converts to one)
	tryCatch({
		as.of.days.before = as.numeric(options$as.of.days.before)
		stopifnot(length(as.of.days.before) == 1 && as.of.days.before >= 0)
		stopifnot(as.of.days.before == floor(as.of.days.before))
	},
		error = function(e)
			stop("as.of.days.before must be a scalar integer", call. = FALSE)
	)
	
	# Check that as.of.time is in "%H:%M:$S" format
	tryCatch({
		as.of.time = as.character(options$as.of.time, format = "%H:%M:$S")
		stopifnot(length(as.of.time) == 1)
		
		# Check that these are valid posix times
		stopifnot(all(!is.na(as.POSIXct(paste0("2016-01-01 ", as.of.time),
			"%Y-%m-%d %H:%M:%S", tz = 'GMT'))))
	},
		error = function(e)
			stop(paste0("as.of.time must be a scalar in a format that can be ",
			"coerced into a \"%H:%M:%S\" string"), call. = FALSE)
	)
	
	# Check that the aggregation level is valid, request daily if NULL
	if (!is.null(options$agg.level)) {
		tryCatch({
			agg.level = tolower(options$agg.level)
			stopifnot(length(agg.level) == 1 &&	agg.level %in% agg.level.types)
		},
			error = function(e)
				stop(paste0("agg.level must be one of 5min, hour, day, week, ",
				"month , year, or raw"), call. = FALSE)
		)
	} else {
		agg.level = "day"
	}
	url.tail = paste0(url.tail, '&agglevel=', options$agg.level)
	
	# Check the stat to be used w/ the aggregation period, request avg if NULL
	if (!is.null(options$stat)) {
		tryCatch({
			options$stat = tolower(options$stat)
			stopifnot(length(options$stat) == 1 &&
				options$stat %in% stat.types)
		},
			error = function(e)
				stop("stat must be one of avg, min, max", call. = FALSE)
		)
	} else {
		options$stat = "avg"
	}
	
	# Find the associated R function with the stat
	agg.func = c("mean", "min", "max")[c("avg", "min", "max") %in%
		options$stat]
	url.tail = paste0(url.tail, '&stat=', options$stat)
	
	# Validate the hours to pull, use 1:24 if NULL
	if (!is.null(options$hours)) {
		tryCatch({
			options$hours = tolower(gsub(", ", ",", options$hours))
			hours.length = nchar(options$hours)
			hours.all = options$hours == "all"
			hours.list = strsplit(options$hours, ",")
			stopifnot(hours.length > 0 && (hours.all ||
				all(hours.list %in% hours.types)))
			if (hours.all) {
				hours.list = 1:24
			}
			url.tail = paste0(url.tail, '&hours=', options$hours)
		},
			error = function(e)
				stop(paste0("hours must be the either 'all' or a comma or ",
				"comma-space separated list of unique numbers from 1-24"),
				call. = FALSE)
		)
	} else {
		hours.list = 1:24
	}
	
	# Validate the days to pull, use all days if NULL
	if (!is.null(options$days)) {
		tryCatch({
			options$days = tolower(gsub(", ", ",", options$days))
			days.length = nchar(options$days)
			days.all = options$days == "all"
			days.list = strsplit(options$days, ",")
			days.match.total = 0
			for (i in 1:length(names(days.types))) {
				days.match.this = sum(days.list %in% days.types[[1]][i])
				days.match.total = days.match.total + days.match.this
				stopifnot(days.match.this <= 1)
				days.list[days.list %in% days.types[[1]][i]] =
					names(days.types)[i]
			}
			stopifnot(days.length > 0 && (days.all || days.match.total ==
				length(days.list)))
			url.tail = paste0(url.tail, '&days=', options$days)
		},
			error = function(e)
				stop(paste0("days must be either 'all' or a comma or ",
				"comma-space separated list of unique days of the week or ",
				"abbreviated days of the week"), call. = FALSE)
		)
	} else {
		days.list = names(days.types)
	}
	
	# Validate the whether to use peak or off peak, use all if NULL
	if (!is.null(options$peak.type)) {
		tryCatch({
			options$peak.type = tolower(options$peak.type)
			stopifnot(is.null(options$hours) && is.null(options$days) &&
				length(options$peak.type) == 1 && (options$peak.type == "all"
				|| options$peak.type %in% peak.type.types))
			if (options$peak.type == "onpeak") {
				hours.list = 7:22
			} else if (options$peak.type == "offpeak") {
				hour.list = c(1:6, 23:24)
			} else if (options$peak.type == "wdpeak") {
				hours.list = 7:22
				days.list = c("monday", "tuesday", "wednesday", "thursday",
					"friday")
			} else if (options$peak.type == "wepeak") {
				hours.list = 7:22
				days.list = c("saturday", "sunday")
			}
			url.tail = paste0(url.tail, '&peaktype=', options$peak.type)
		},
			error = function(e)
				stop(paste0("peak.type must be one of onpeak, offpeak, ",
				"wdpeak, wepeak, or all, and cannot be specified if hours ",
				"or days is"), call. = FALSE)
		)
	}
	
	# Validate the months to return, use all if NULL
	if (!is.null(options$months)) {
		tryCatch({
			options$months = tolower(gsub(", ", ",", options$months))
			months.length = nchar(options$months)
			months.all = options$months == "all"
			months.list = strsplit(options$months, ",")
			months.match.total = 0
			for (i in 1:length(names(months.types))) {
				months.match.this = sum(months.list %in% months.types[[i]])
				months.match.total = months.match.total + months.match.this
				stopifnot(months.match.this <= 1)
				months.list[months.list %in% months.types[[1]][i]] =
					names(months.types)[i]
			}
			stopifnot(months.length > 0 && (months.all || months.match.total ==
				length(months.list)))
			url.tail = paste0(url.tail, '&months=', options$months)
		},
			error = function(e)
				stop(paste0("months must be either 'all' or a comma or ",
				"comma-space separated list of unique months of the year or ",
				"abbreviated months of the year"), call. = FALSE)
		)
	} else {
		months.list = c("january", "february", "march", "april", "may", "june",
			"july", "august", "september", "october", "november", "december")
	}
	
	# Validate the time zone
	if (!is.null(options$time.zone)) {
		tryCatch({
			time.zone = tolower(options$time.zone)
			stopifnot(length(time.zone) == 1 && time.zone %in% time.zone.types)
			url.tail = paste0(url.tail, '&timezone=', time.zone)
		},
			error = function(e)
				stop(paste0("time.zone must be one of gmt, est, ept, cst, ",
				"cpt, mst, mpt, pst, ppt"), call. = FALSE)
		)
	} else {
		time.zone = "default"
	}
	
	# Determine total length of URL tail (with start and end added)
	url.tail.length = nchar(paste0(url.tail, "&startdate=01/01/2016",
		"&enddate=01/01/2016"))

	###########################################################################
	# Determine Dates for Data to be Returned
	###########################################################################
	# Determine as.of.dates and corresponding as.of.datetimes
	as.of.dates = seq(as.Date(options$start.date, "%m/%d/%Y") -
		as.of.days.before, as.Date(options$end.date, "%m/%d/%Y") -
		as.of.days.before, by = 1)
	as.of.posix = as.POSIXct(paste0(as.of.dates, " ", as.of.time), tz = "GMT")
	pull.to.dates = rep(as.Date(NA), length(as.of.dates))
	
	# Create all datetimes
	add.t = function(x, y, u = "day")
		seq(x, length.out = 2, by = paste0(y, " ", u))[2]
	as.of.start.posix = add.t(start.date.posix, - as.of.days.before, "day")
	seq.int = ifelse(agg.level == "5min", "5 min", agg.level)
	if (agg.level == "raw") {
		all.datetimes = as.POSIXct("2000-01-01 00:00:00")[0]
		all.save.times = as.POSIXct("2000-01-01 00:00:00")[0]
	} else {
		
		# Change posix depending on agg.level
		if (agg.level %in% c("5min", "hour", "day")) {
			start.date.posix = add.t(start.date.posix, 1, seq.int)
			as.of.start.posix = add.t(as.of.start.posix, 1, seq.int)
		} else if  (agg.level == "week") {
			start.date.posix = add.t(floor_date(start.date.posix, "week"), 1)
			all.save.times = add.t(floor_date(as.of.start.posix, "week"), 1)
		} else if (agg.level %in% c("month", "year")) {
			start.date.posix = floor_date(start.date.posix, seq.int)
			as.of.start.posix = floor_date(as.of.start.posix, seq.int)
		}
		all.datetimes = seq(start.date.posix, end.date.posix, by = seq.int)
		all.save.times = seq(as.of.start.posix, end.date.posix, by = seq.int)
	}
	max.save.day = as.Date(format(as.Date(now(),
		tz = time.zone.conversions[[time.zone]]) - 1))
	make.df = function(dts, agg) data.frame(DATETIME = yes.convert.datetime(
		format(dts, "%m/%d/%Y %H:%M:%S"), agg), stringsAsFactors = FALSE)
	all.datetimes.df = make.df(all.datetimes, agg.level)
	all.save.times.df = make.df(all.save.times, agg.level)
	
	###########################################################################
	# Pre-Allocate Paramters to Identify Variables/Dates
	###########################################################################
	# Repeat the relevant items and as.of.dates as necessary to pair
	num.items = length(items.list)
	num.dates = length(as.of.dates)
	num.datetimes = nrow(all.datetimes.df)
	num.pulls = num.items * num.dates
	items.list.url = rep(items.list, num.dates)
	as.of.dates.url = rep(as.of.dates, each = num.items)
	as.of.posix.url = rep(as.of.posix, each = num.items)
	
	# Replace characters that cannot appear in urls in the items
	items.list.url = gsub("&", "%26", items.list.url)
	items.list.url = gsub(" ", "%20", items.list.url)
	
	# Isolate the datatypes and objects from the items.list
	dtypes = rep(gsub("^([^:]+):[^:]+[:]?.*$", "\\1", items.list), num.dates)
	objects = rep(gsub("^[^:]+:([^:]+)[:]?.*$", "\\1", items.list), num.dates)
	objs.fix = rep(gsub("^[^:]+:([^:]+[:]?.*)$", "\\1", items.list), num.dates)
	col.names = paste0(dtypes, ".", objs.fix)
	
	# Find regexes for each of these
	obj.regexs = gsub("\\(", "\\\\(", gsub("\\)", "\\\\)", objects))
	patterns = paste0("^", obj.regexs, "( \\(NODE\\))? \\(", dtypes,
		"\\)\\.[[:digit:]]+$")
	
	# Initialize a data.frame to hold the aggregated forecasts
	yes.fc.data = bind_cols(all.datetimes.df, setNames(data.frame(matrix(NA,
		(agg.level != "raw") * num.datetimes, num.items)),
		col.names[1:num.items]))
	
	###########################################################################
	# Check Stored Data
	###########################################################################
	# Read old forecast, checking all potential saves
	i = 1
	files = c(file.name, extra.saves)
	while (i <= length(files) && !"saved.forecasts.list" %in% ls()) {
		
		# Check if the save exists before loading
		if (file.exists(files[i])) {
			tryCatch({
				saved.forecasts.list = readRDS(files[i])
			},
				error = function(e) NULL
			)
		}
		
		# Iterate i
		i = i + 1
	}
	
	# If not successfully pulled, initiate empty list
	if (!"saved.forecasts.list" %in% ls()) {
		saved.forecasts.list = list()
	}
	
	# Find which forecasts are not saved, loop over items/as.of.dates
	pull.idx = rep(TRUE, length(items.list.url))
	for (i in 1:length(items.list.url)) {
	
		# Test for saved data
		saved.data = saved.forecasts.list[[agg.level]][[time.zone]][[as.of.time
			]][[items.list.url[i]]][[as.character(as.of.dates.url[i])]]
		saved.df = saved.data[["df"]]
		pull.idx[i] = is.null(saved.df)
		
		# Determine dates of needed and saved data if possible
		if (!pull.idx[i]) {
			
			# Store required date and saved dates
			req.date = as.of.dates.url[i] + as.of.days.before
			saved.dates = saved.data[["dates"]]
			
			# Check if required date is saved
			pull.idx[i] = !req.date %in% saved.dates
		}
		
		# Extract saved data if able
		if (!pull.idx[i]) {
			
			# Add datetimes to svd.df
			col.idx = names(saved.df)
			temp.datetimes = row.names(saved.df)
			saved.df$DATETIME = temp.datetimes
			
			# Find indices for placing in yes.fc.data
			use.idx = as.Date(substr(temp.datetimes, 1, 10)) == req.date
			if (agg.level == "raw") use.idx = use.idx & !is.na(saved.df[[1]])
			match.idx = match(temp.datetimes[use.idx], yes.fc.data$DATETIME)
			has.match = !is.na(match.idx)
			repl.idx = which(use.idx)[has.match]
			new.idx = which(use.idx)[!has.match]
			
			# Place in yes.fc.data
			yes.fc.data[match.idx[has.match], col.idx] =
				saved.df[repl.idx, col.idx]
			if (length(new.idx)) {
				yes.fc.data %<>% bind_rows(saved.df[new.idx, , drop = FALSE])
			}
		}
	}
	pull.idx = which(pull.idx)
	
	###########################################################################
	# Pull New Data from YES in Batches
	###########################################################################
	# Loop Over Items to Ensure the URL Does not Get Excessively Long
	i = 1
	user.num = ceiling(runif(1) * length(username))
	while (i <= length(pull.idx)) {
		
		# Message the user what item # the API is on
		message(paste0(i, " out of ", length(pull.idx)))
		j = i
		
		# Add All Items to the Url then Trim Trailing Comma and Add Tail
		url = url.base
		per.cond = TRUE
		while (nchar(url) + nchar(items.list.url[pull.idx[i]]) <=
			url.max.length - url.tail.length - url.base.length &&
			i <= length(pull.idx) && per.cond) {
			
			url = paste0(url, items.list.url[pull.idx[i]], ":",
				as.of.dates.url[pull.idx[i]], "%20", as.of.time, ",")
			i = i + 1
			
			# Update the periodicity condition
			max.date = max(as.of.dates.url[pull.idx[j:i]]) + as.of.days.before
			min.date = min(as.of.dates.url[pull.idx[j:i]])
			if (agg.level == "5min") {
				per.cond = j - i + 1 <= 10 || max.date - min.date <= 10 
			} else if (agg.level == "hour") {
				per.cond = j - i + 1 <= 20 || max.date - min.date <= 365 
			} else {
				per.cond = j - i + 1 <= 75
			}
		}
		
		# Find start and end date and append
		pulls = pull.idx[j:(i - 1)]
		fc.start = min(as.of.dates.url[pulls])
		fc.end = max(as.of.dates.url[pulls]) + as.of.days.before
		pull.to.dates[pulls] = fc.end
		url = paste0(substr(url, 1, nchar(url) - 1), url.tail)
		url = paste0(url, "&startdate=", format(fc.start, "%m/%d/%Y"))
		url = paste0(url, "&enddate=", format(fc.end, "%m/%d/%Y"))
		print("a")
		# Pull the table from the API and message the user about it
		message("Hitting Yes...")
		tmp.start.time = Sys.time()
		
		# Extract text, get forecast times
		yes.text = content(GET(url, authenticate(user = username[user.num],
			password = password[user.num])), as = "text")
		fc.times = read.table(text = yes.text, header = TRUE, sep = ",",
			check.names = FALSE, stringsAsFactors = FALSE)
		
		# Get the yes forecast numbers
		if (nrow(fc.times) > 1) {
			temp.yes.data = read.table(text = yes.text, header = FALSE, sep =
				",", stringsAsFactors = FALSE, skip = 2, na.strings = "")
		} else {
			temp.yes.data = setNames(data.frame(matrix(NA, 0, ncol(fc.times))),
				names(fc.times))
			temp.yes.data$DATETIME = as.character(temp.yes.data$DATETIME)
		}
		fc.times = fc.times[1, , drop = FALSE]
		
		tmp.end.time = Sys.time()
		message(paste("Total Elapsed Time:", format(.POSIXct(difftime(
			tmp.end.time, tmp.start.time, units = "secs"), tz = "GMT"),
			"%H:%M:%S")))

		#######################################################################
		# Reformat the Data into the Format Required
		#######################################################################
		# Aggregate by Datetimes for Daylight Savings
		message("Aggregating...")
		tmp.start.time = Sys.time()
	
		# Rename columns as needed
		tryCatch({
				
			# Check for error
			stopifnot(ncol(fc.times) != 1 || names(fc.times) != "error")
				
			# Drop irrelevant columns then rename
			data.idx = !grepl("^DATETIME$", names(fc.times))
			rem.idx = grepl("^(HOURENDING|MARKETDAY|PEAKTYPE|MONTH|YEAR)$",
				names(fc.times))
			temp.col.names = paste0(names(fc.times)[data.idx &
				!rem.idx], ".", pulls)
			names(fc.times)[data.idx & !rem.idx] = temp.col.names
			names(temp.yes.data) = c("DATETIME", temp.col.names)
			fc.times = fc.times[, !rem.idx]
			temp.yes.data = temp.yes.data[, !rem.idx]
			
			# Aggregate the data
			if (nrow(temp.yes.data) > 0) {
				temp.yes.data = aggregate(. ~ DATETIME, temp.yes.data,
					FUN = agg.func, na.action = na.pass, na.rm = TRUE)
			}
			
			# Loop over non-datetime columns and replace NaNs
			for (k in which(names(temp.yes.data) != "DATETIME")) {
				temp.yes.data[is.nan(temp.yes.data[, k]), k] =
					rep(NA, sum(is.nan(temp.yes.data[, k])))
			}
		},
			error = function(e)
				stop(fc.times, call. = FALSE)
		)
		
		tmp.end.time = Sys.time()
		message(paste("Total Elapsed Time:", format(.POSIXct(difftime(
			tmp.end.time, tmp.start.time, units = "secs"), tz = "GMT"),
			"%H:%M:%S")))
		
		# Merge With Existing Data
		temp.yes.data$DATETIME =
			yes.convert.datetime(temp.yes.data$DATETIME, agg.level)
		temp.yes.data = temp.yes.data[order(temp.yes.data$DATETIME), ]
		
		# Turn fc.times into POSIX
		fc.times = Reduce(c, lapply(fc.times[temp.col.names],
			function(x) as.POSIXct(x, "%m/%d/%Y %H:%M:%S", tz = "GMT")))
		
		# Identify index of the item in items.list each references
		col.items.idx = rep(NA, length(temp.col.names))
		for (k in 1:length(temp.col.names)) {
			
			# Find unique patterns for this pull
			uni.patterns = unique(patterns[pulls])
			pattern.idx = match(patterns, uni.patterns)
			
			# Find which patterns it matches, and which item it must come from
			pull.matches = which(sapply(uni.patterns, function(x) grepl(x,
				temp.col.names[k])))[1] == pattern.idx & !is.na(pattern.idx)
			col.item = which(as.of.posix.url == min(as.of.posix.url[
				pull.matches & as.of.posix.url >= fc.times[k]]) & pull.matches)
			if (length(col.item) > 0) {
				col.items.idx[k] = col.item
			}
		}
		
		# Make sure each index in items.list is referenced at most once
		for (k in pulls) {
			match.idx = col.items.idx == k & !is.na(col.items.idx)
			latest.fc.idx = match.idx & fc.times ==
				max(c(fc.times[match.idx], - Inf), na.rm = TRUE)
			older.fc.idx = 1:length(match.idx) != which(latest.fc.idx)[1]
			col.items.idx[match.idx & older.fc.idx] = NA
		}
		
		# We save all datetimes if agg.level is raw
		for (k in which(!is.na(col.items.idx))) {
			
			# Find index for saving data and capture df to store
			item.idx = col.items.idx[k]
			df = setNames(temp.yes.data[c("DATETIME", temp.col.names[k])],
				c("DATETIME", col.names[item.idx]))
			
			# Find indices for placing in yes.fc.data
			temp.datetimes = temp.yes.data$DATETIME
			use.idx = as.Date(substr(temp.datetimes, 1, 10)) == floor_date(
				as.of.dates.url[item.idx] + as.of.days.before, seq.int)
			if (agg.level == "raw") use.idx = use.idx &
				!is.na(df[[col.names[item.idx]]])
			match.idx = match(temp.datetimes[use.idx], yes.fc.data$DATETIME)
			has.match = !is.na(match.idx)
			repl.idx = which(use.idx)[has.match]
			new.idx = which(use.idx)[!has.match]
			
			# Add new data to yes.fc.data
			yes.fc.data[match.idx[has.match], col.names[item.idx]] =
				df[repl.idx, col.names[item.idx]]
			if (length(new.idx)) {
				yes.fc.data %<>% bind_rows(df[new.idx, , drop = FALSE])
			}
			
			# Find which dates to save
			sv.dts = seq(as.of.dates.url[col.items.idx[k]], fc.end, by = 1)
			sv.dts = sv.dts[sv.dts <= max.save.day]
			sv.idx = !is.na(df[[col.names[item.idx]]]) &
				as.Date(substr(df$DATETIME, 1, 10)) %in% sv.dts
			
			# Save df and dates
			sv.df = filter(select(df, - DATETIME), sv.idx)
			row.names(sv.df) = df$DATETIME[sv.idx]
			sv.list = list(df = sv.df, dates = sv.dts)
			saved.forecasts.list[[agg.level]][[time.zone]][[as.of.time]][[
				items.list.url[item.idx]]][[as.character(as.of.dates.url[
				item.idx])]] = sv.list
		}
		
		# Save forecasts that are not available
		for (k in setdiff(pulls, col.items.idx)) {
			
			# Save df
			sv.dts = seq(as.of.dates.url[k], fc.end, by = 1)
			sv.dts = sv.dts[sv.dts <= max.save.day]
			sv.list = list(df = setNames(data.frame(NA[0]), col.names[k]),
				dates = sv.dts)
			saved.forecasts.list[[agg.level]][[time.zone]][[as.of.time]][[
				items.list.url[k]]][[as.character(as.of.dates.url[
				k])]] = sv.list
		}

		# We clear memory and message the user
		gc(reset = T)
		message(paste0(memory.size(F), " Mb / ", memory.size(T), " Mb"))
		
		# Iterate the username
		user.num = (user.num %% length(username)) + 1
	}
	
	###########################################################################
	# Create Single Data Frame from All Data, with Correctly Formatted Datetime
	###########################################################################
	# Make yes.data.list into the yes.data data-frame we all know and love
	if (agg.level == "raw") {
		yes.fc.data = yes.fc.data[order(yes.fc.data$DATETIME), ]
	}
	
	# Only modify if length > 0
	if (nrow(yes.fc.data) > 0) {
		
		# Filter yes.data as required
		hours = as.numeric(substring(yes.fc.data$DATETIME, 14, 15))
		dates = as.Date(substr(yes.fc.data$DATETIME, 1, 10), format = "%Y-%m-%d")
		keep.idx = rep(TRUE, nrow(yes.fc.data))
	
		# The restriction depends on the agg level
		keep.idx[!dates %in% unique(floor_date(as.of.dates + as.of.days.before,
			seq.int))] = FALSE
		if (agg.level %in% c("raw", "5min", "hour")) {
			keep.idx[!hours %in% hours.list] = FALSE
		}
		if (agg.level %in% c("raw", "5min", "hour", "day")) {
			keep.idx[!tolower(weekdays(dates)) %in% days.list] = FALSE
		}
		keep.idx[!tolower(format(dates, "%B")) %in% months.list] = FALSE
		
		# Filter
		yes.fc.data = yes.fc.data[keep.idx, ]
	}
	
	# Add row.names
	row.names(yes.fc.data) = yes.fc.data$DATETIME
	yes.fc.data$DATETIME = NULL
	
	###########################################################################
	# Save data and return
	###########################################################################
	# Save saved.forecasts.list
	if (!is.null(file.name)) {
		saveRDS(saved.forecasts.list, file.name)
	}
	
	# Return the Table
	return(yes.fc.data)
}

###############################################################################
# yes.constraint Function for Search for Binding Constraints
###############################################################################
yes.constraint <- function(isos, period, market = NULL,
	start.date = Sys.Date() - 1, end.date = Sys.Date() - 1,
	username = c("data1@guzmanenergy.com", "data2@guzmanenergy.com"),
	password = rep("Brookdale1", 2)) {
	# isos			=	the isos, as a character string vector, for which
	#					binding constraints are to be returned.
	# period		=	the period of the data one wishes to pull. May be
	#					"fivemin", "fifteenmin" (only for CAISO), or "hourly".
	# market		=	which of the ISOs markets one wishes to pull data for.
	#					This may be "RT", "DA" or "HA (CAISO only), but is only
	# start.date	=	start date for the timeseries. Default is yesterday.
	#					Input as "MM/DD/YYYY", numeric or date formats.
	# end.date		=	end date for the timeseries. Default is yesterday.
	#					Input as "MM/DD/YYYY", numeric or date formats.
	#					used if one chooses "hourly" for period data.
	# username		=	a string, the username used to sign into YES Energy.
	# password		=	a string, the password used to sign into YES Energy.
	
	###########################################################################
	# Set Hard-Coded Lists for Future Validation
	###########################################################################
	# Create the base URL and set max length
	base.url = "https://services.yesenergy.com/PS/rest/constraint/"
	
	# Set validation arrays for inputs
	period.types = c("fivemin", "fifteenmin", "hourly")
	market.types = c("avg", "min", "max")
	
	###########################################################################
	# Validate and Append Non-Date Inputs Provided
	###########################################################################
	# Validate the iso provided
	tryCatch({
		stopifnot(length(isos) >= 1 && is.character(isos))
	},
		error = function(e)
			stop("\"isos\" must be provided as a vector character string",
			call. = FALSE)
	)
	
	# Validate the period provided
	tryCatch({
		stopifnot(length(period) == 1 && is.character(period) &&
			(period %in% c("fivemin", "hourly") || period == "fifteenmin" &&
			all(isos == "CAISO")))
	},
		error = function(e)
			stop(paste0("\"period\" must be provided as a scalar character ",
			"string from the set of: \"fivemin\", \"fifteenmin\" (only for ",
			"CAISO), or \"hourly\""), call. = FALSE)
	)
	
	# Validate the market provided
	tryCatch({
		stopifnot(period == "hourly" && length(market) == 1 &&
		is.character(market) && (market %in% c("DA", "RT") || market == "HA" &&
		all(isos == "CAISO")) || is.null(market))
	},
		error = function(e)
			stop(paste0("If \"period\" is \"hourly\", \"market\" must be ",
			"provided as a scalar character string from the set of: \"DA\", ",
			"\"HA\" (only for CAISO), or \"RT\", and must be NULL otherwise"),
			call. = FALSE)
	)
	
	# Append the above to the url
	base.url = paste0(base.url, period, "/",
		ifelse(period == "hourly", paste0(market, "/"), ""))
	
	###########################################################################
	# Validate and Append Date Inputs Provided
	###########################################################################
	# Parse start date
	tryCatch({
			
		# Check start date is one of: numeric, date, or string
		start.date.numeric = is.numeric(start.date)
		start.date.datefmt = class(start.date) == "Date"
		start.date.string = !start.date.numeric && !start.date.datefmt &&
			!is.na(as.Date(start.date, format = "%m/%d/%Y")) &&
			as.Date(start.date, format = "%m/%d/%Y") > 0
		stopifnot(length(start.date) == 1 &&
			(start.date.numeric || start.date.datefmt || start.date.string))
		
		# Format as a date
		if (start.date.numeric) {
			start.date = as.Date(start.date, as.Date("1970-01-01"))
		} else if (start.date.string) {
			start.date = as.Date(start.date, "%m/%d/%Y")
		}
	},
		error = function(e)
			stop(paste0("start.date must be a number, date, or string in ",
			"format %m/%d/%Y for a date > 1/1/1970"), call. = FALSE)
	)
	
	# Parse end date
	tryCatch({
			
		# Check end date is one of: numeric, date, or string
		end.date.numeric = is.numeric(end.date)
		end.date.datefmt = class(end.date) == "Date"
		end.date.string = !end.date.numeric && !end.date.datefmt &&
			!is.na(as.Date(end.date, format = "%m/%d/%Y")) &&
			as.Date(end.date, format = "%m/%d/%Y") > 0
		stopifnot(length(end.date) == 1 &&
			(end.date.numeric || end.date.datefmt || end.date.string))
		
		# Format as a date
		if (end.date.numeric) {
			end.date = as.Date(end.date, as.Date("1970-01-01"))
		} else if (end.date.string) {
			end.date = as.Date(end.date, "%m/%d/%Y")
		}
	},
		error = function(e)
			stop(paste0("end.date must be a number, date, or string in ",
			"format %m/%d/%Y for a date > 1/1/1970"), call. = FALSE)
	)
	
	###########################################################################
	# Validate Username and Password
	###########################################################################
	tryCatch({
		stopifnot(is.character(username) && is.character(password) &&
			length(username) > 0 && length(username) == length(password))		
	},
		error = function(e)
			stop("username and password must be character vectors of equal ",
				"length", call. = FALSE)
	)
	
	###########################################################################
	# Pull and Process Data
	###########################################################################
	# Set the start and end dates for each pull
	days.per.pull = ifelse(period == "hourly", 365, 95)
	start.dates = seq(start.date, end.date, by = days.per.pull)
	end.dates = c(tail(start.dates, - 1) - 1, end.date)
	
	# Pull data from YES, looping over start.dates
	yes.data = NULL
	row.counter = rep(0, 10)
	user.num = ceiling(runif(1) * length(username))
	for (iso in isos) {
		for (i in seq_along(start.dates)) {
			
			# Append dates to url
			url = paste0(base.url, iso, ".csv?",
				"startdate=", format(start.dates[i], "%m/%d/%Y"),
				"&enddate=", format(end.dates[i], "%m/%d/%Y"))
		
			# Message user
			message(paste0("Pulling constraint YES data for ", iso, " from ",
				start.dates[i], " to ", end.dates[i]))
		 
			# Pull and parse data
			yes.text = content(GET(url, authenticate(user = username[
				user.num], password = password[user.num])), as = "text")
			temp.yes.data = read.table(text = yes.text, header = TRUE,
				sep =",", check.names = FALSE, stringsAsFactors = FALSE,
				comment.char = "")
			
			# Update row counter and make short requests to stop API overload 
			row.counter = c(nrow(temp.yes.data), head(row.counter, - 1))
			while (sum(row.counter) >= 5 * 10 ^ 5) {
				suppressMessages(yes.timeseries(list(items.list =
					"LOAD_FORECAST:ERCOT"), list(start.date = Sys.Date()),
					username = username[user.num],
					password = password[user.num]))
				row.counter = c(1, head(row.counter, - 1))
			}
			
			# Check for error
			tryCatch({
				stopifnot(all(names(temp.yes.data) != "error"))
			},
				error = function(e) stop(temp.yes.data[1, 1], call. = FALSE)
			)
			
			# Append to existing yes.data
			yes.data = bind_rows(yes.data, temp.yes.data)
			
			# Iterate the username
			user.num = (user.num %% length(username)) + 1
		}
	}
	
	# Return data
	return(yes.data)
}

###############################################################################
# yes.ftr.portfolio Function for Finding Participant FTRs in a Given Month
###############################################################################
yes.ftr.portfolio <- function(iso, date, participants,
	username = "mkguzman@guzmanenergy.com", password = "7seesaws") {
	
	# Find the appropriate month start and end dates
	month.date = format(date, "%b %Y")
	
	# Turn entities into list
	participants = paste(participants, collapse = ",")
	
	# Create url
	url = paste0("https://services.yesenergy.com/PS/rest/ftr/pathperformance",
		".csv?participants=", participants, "&iso=", iso,
		"&startdate=", month.date, "&enddate=", month.date)
	url = gsub(" ", "%20", url)
	
	# Pull data
	yes.text = content(GET(url, authenticate(user = username,
		password = password)), as = "text")
	yes.data = read.table(text = yes.text, header = TRUE, sep = ",",
		check.names = FALSE, stringsAsFactors = FALSE)
	
	# Create fake data if none pulled
	if (!nrow(yes.data)) {
		yes.data = yes.ftr.portfolio("PJMISO", as.Date("2016-12-01"),
			"SEVRIV")[0, ]
	}
	
	# Return data
	return(yes.data)
}

yes.pricenode <- function(iso = "", username = "bwu@guzmanenergy.com",
                          password = "Guzman12345") {
  # object		=	the datatype, as a character string, for which objects
  #					with it as an available datatype are to bereturned
  # iso			=	the iso, as a character string, for which objects in it
  #					available for the datatype specified are to be returned
  # username		=	the username used to sign into YES Energy
  # password		=	the password used to sign into YES Energy
  
  url = "https://services.yesenergy.com/PS/rest/objects/pricenodes/"
  
  # Validate Inputs, Append to the URL
  if (nchar(iso) > 0) {
    tryCatch({
      stopifnot(length(iso) == 1 && is.character(iso))
    },
    error = function(e)
      stop("iso must be a single character string", call. = FALSE)
    )
    url = paste0(url, iso,".csv")	
  }
  
  # Validate username and password
  tryCatch({
    stopifnot(is.character(username) && is.character(password) &&
                length(username) == 1 && length(password) == 1)		
  },
  error = function(e)
    stop("username and password must be character scalars",
         call. = FALSE)
  )
  
  message(url)
  # Pull Data from YES and Modify It To Replace Commas and Newlines Intended
  # To Be Part of Text With Other Characters
  yes.data = read.table(text = getURL(url, userpwd = paste0(username, ":",
                                                            password), ssl.verifypeer = FALSE), header = TRUE, sep = ",",
                        comment.char = "", quote = "")
  
  # Return the Table
  return(yes.data)
}

