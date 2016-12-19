


##############################
# Import to XDF
##############################

# The XDF file is the standard way of working with data in MRS
# (although not mandatory).

# First, let's import the flights dataset
flightsCsv <- "flights_q1.csv"

# Create a temporary path for the XDF file
flightsXdf <- "flights.xdf"

# Import it
rxImport(inData = flightsCsv, outFile = flightsXdf)


# Check the results
rxGetInfo(flightsXdf, getVarInfo = TRUE, numRows = 5)




###################
# Exercise 1: Import the Remaining Flights
###################

# Hang on... this is R Server. 300k rows should be trivial.
# (It is for R, too, but never mind that)
# Import the flights_q234.csv records into your *existing*
# flightsXdf file.

rxImport(inData = "flights_q234.csv",
         outFile = flightsXdf,
         append = TRUE)

rxGetInfo(flightsXdf, numRows = 5)



###################
# Exercise 2: Import the Airport Data
###################

# Load airports.csv into a new XDF file

airportsXdf <- tempfile(fileext = ".xdf")

rxImport(inData = "airports.csv",
         outFile = airportsXdf)

rxGetInfo(airportsXdf, numRows = 5)


##############################
# Sorting XDFs
##############################


# Sort by arrival delay
xdfSorted <- tempfile(fileext = ".xdf")

rxSort(inData = flightsXdf,
       outFile = xdfSorted,
       sortByVars = "arr_delay")



# Check the results
rxDataStep(xdfSorted, numRows = 10)





# Sort decreasing
rxSort(inData = flightsXdf,
       outFile = xdfSorted,
       sortByVars = "arr_delay",
       decreasing = TRUE,
       overwrite = TRUE)



# Check the results
rxDataStep(xdfSorted, numRows = 10)





###################
# Exercise 3: Sort an XDF
###################

# Sort flightsXdf by decreasing origin and increasing distance.

rxSort(inData = flightsXdf,
       outFile = xdfSorted,
       sortByVars = c("origin", "distance"),
       decreasing = c(TRUE, FALSE),
       overwrite = TRUE)



# Check the results
rxDataStep(xdfSorted, numRows = 15)




###################
# Deduplication
###################



# rxSort does deduplication, too, with the removeDupKeys argument.
rxSort(inData = flightsXdf,
       outFile = xdfSorted,
       sortByVars = c("origin", "month"),
       removeDupKeys = TRUE,
       dupFreqVar = "freq",
       overwrite = TRUE)



# Check the results
rxDataStep(xdfSorted, numRows = 10)





###################
# Merging XDFs
###################


# Merging
# Examine the two tables
rxDataStep(flightsXdf, numRows = 5)
rxDataStep(airportsXdf, numRows = 5)



# Merge on the origin airport details
flight_origin <- tempfile(fileext = ".xdf")

rxMerge(inData1 = flightsXdf,
        inData2 = airportsXdf,
        outFile = flight_origin,
        overwrite = TRUE,
        
        # Type of join
        type = "left",
        
        # Temporarily rename the key
        newVarNames2 = c(faa = "origin"),
        
        # Name the key variable(s)
        matchVars = "origin"
)



rxDataStep(flight_origin, numRows = 5)





###################
# Exercise 4: Merge on the Destination Airport Details
###################


# Merge on the airport details for the *destination* airport.
# Add one new argument: duplicateVarExt = c("origin", "dest")
# Since we're merging airportXdf on twice, we'll end up with
# duplicate variables. duplicateVarExt will append "origin"
# to the existing variables, and "dest" to the new ones.



# Merge on the destination airport details
flight_dest <- tempfile(fileext = ".xdf")

rxMerge(inData1 = flight_origin,
        inData2 = airportsXdf,
        outFile = flight_dest,
        type = "left",
        newVarNames2 = c(faa = "dest"),
        matchVars = "dest",
        duplicateVarExt = c("origin", "dest"))

# Check the results
rxDataStep(flight_dest, numRows = 5)




#############################
# Subset rows with criteria
#############################

# Maybe we want to create a subset of all the flights
# for a single carrier.

# In MRS, rxDataStep is the workhorse function for that kind
# of data manipulation. Its argument "rowSelection" takes
# a series of criteria and returns matching records.

united_flights <- tempfile(fileext = ".xdf")

rxDataStep(inData = flightsXdf,
           outFile = united_flights,
           rowSelection = carrier == "UA")

rxDataStep(united_flights, numRows = 5)




###################
# Exercise 5: Subset to United Flights Originating at LaGuardia
###################

# Let's try that import again, and this time subset to just
# United flights that originated in LaGuardia (LGA)

# Let's just recreate the united_flights XDF
united_flights <- tempfile(fileext = ".xdf")


# Use rxDataStep to populate it with United's LGA flights:


rxDataStep(inData = flightsXdf,
           outFile = united_flights,
           rowSelection = carrier == "UA" & origin == "LGA")



rxDataStep(united_flights, numRows = 5)






# rxDataStep is also the function to use for creating and
# modifying variables. The key argument there is "transforms".
# transforms is a little weird - it takes a list, which is a 
# highly flexible R data structure. Inside that list, each new
# variable gets an entry like this:
# newVar = someFunction(oldVar)
# Each entry must be named (newVar), and is separated from any
# following entries by a comma.

# Let's convert year-month-day into a proper Date variable.
rxDataStep(inData = flightsXdf,
           outFile = flightsXdf,
           transforms = list(
               
               # First, let's just paste the components
               flightDateString = paste(year, month, day, sep = "-"),
               
               # Then convert it into a Date
               flightDate = as.Date(flightDateString),
               
               # Finally, format it to day of week
               dayOfWeek = format(flightDate, format = "%A")
           ),
           overwrite = TRUE
)





###################
# Exercise 6: Calculate Speed
###################


# Add speed to each flight's records

rxDataStep(inData = flightsXdf,
           outFile = flightsXdf,
           transforms = list(
             
               speed = distance / (air_time / 60)
           ),
           overwrite = TRUE
)

rxDataStep(flightsXdf, numRows = 5)





############
# Perils of Complex Transformations
############


# First, set up a tiny dataset, split into three chunks:
chunks_df <- data.frame(date = seq(Sys.Date(), length.out = 9, by = "1 day"),
                        chunk = (1:9 + 2) %/% 3,
                        open = sample(21:29, size = 9),
                        stringsAsFactors = FALSE
)

chunks_df

chunks <- tempfile(fileext = ".xdf")

for(i in 1:5) {
    rxImport(inData = chunks_df[chunks_df$chunk %in% i, ],
             outFile = chunks,
             append = file.exists(chunks))
}

rxGetInfo(chunks, getVarInfo = TRUE, numRows = 15)





# Now let's transform the variable "open" so that its lowest value
# equals 0, its highest equals one. If we just write it in the 
# classic R style, it might look like this:
rxDataStep(inData = chunks,
           outFile = chunks,
           transforms = list( openScaledNaive = (open - min(open)) / 
                                                (max(open) - min(open)) ),
           overwrite = TRUE
)


# Check the results.
rxDataStep(chunks)







# The MRS approach: calculate the global min and max independently,
# then pass them to rxDataStep for the transformation.
# Conveniently, they're precomputed in the XDF metadata, so we
# can just extract them:
openMin <- rxGetVarInfo(chunks)$open$low
openMax <- rxGetVarInfo(chunks)$open$high

openMin
openMax



# Pass numValueSummary with transformObjects
rxDataStep(inData = chunks,
           outFile = chunks,
           transformObjects = list(xmin = openMin, xmax = openMax),
           transforms = list( openScaledCorrect = (open - xmin) / (xmax - xmin)  ),
           overwrite = TRUE
)


rxDataStep(chunks)








#############################
# Factors
#############################

# In contrast to open-source R, MRS generally won't create factors
# unless you ask it to with rxFactors.
# Its key argument, factorInfo, takes a list.
# Each element in that list is itself a list.
# Each element of THAT list is an argument that specifies one 
# attribute of a factor - the variable to be converted (varName),
# the levels, etc.
rxFactors(inData = flightsXdf,
          outFile = flightsXdf,
          factorInfo = list(
              carrier_F = list(varName = "carrier"),
              origin_F = list(varName = "origin"),
              dest_F = list(varName = "dest"),
              dayOfWeek_F = list(varName = "dayOfWeek",
                                 levels = c("Sunday", "Monday", "Tuesday",
                                            "Wednesday", "Thursday", "Friday",
                                            "Saturday"))
          ),
          overwrite = TRUE
)


# Check the results
rxGetVarInfo(flightsXdf)










