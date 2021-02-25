import Data.List (minimumBy, sortBy)
import Control.Monad (when)
import Data.Ord (comparing)
import Data.Time
import Data.Typeable
import System.IO
import System.Directory


test = [FreeSlot {time = Ten},TakenSlot {time = Twelve, name = "Amy"}]

-- Start function
run :: IO ()
run = 
    do
        hSetBuffering stdin LineBuffering
        putStrLn("")
        putStrLn("************************\n")
        putStrLn("Hello! Welcome to Barber Scheduler.")
        chooseDate


-- choose the booking date
chooseDate :: IO ()
chooseDate = 
    do
        putStrLn("What date do you plan to come to our shop? In form of YYYY-MM-DD")
        putStrLn("~~~NOTE that we are closed every tuesday~~~")
        datePreferedStr <- getLine 
        if (isYearFormat datePreferedStr) 
            then do
                c <- getCurrentTime 
                let (ty,tm,td) = toGregorian $ utctDay c
                let today = (fromGregorian ty tm td)
                let datePrefered = convertToDate datePreferedStr
                let dayoFWeek = dayOfWeek datePrefered
                
                if datePrefered < today
                    then do
                            putStrLn("The date you enter is invalid, please enter from today after") 
                            chooseDate
                else if (dayoFWeek == Tuesday)
                    then do
                            putStrLn("We are not working this day, please enter another valid date") 
                            chooseDate      
                else 
                    do
                        chooseBaber datePreferedStr
        else if (elem datePreferedStr ["quit", "QUIT", "Q"])
            then do 
                putStrLn("You are leaving the booking system....")
        else 
            do
                putStrLn("The date you enter is invalid, please enter another day") 
                chooseDate


-- take the date and check for barber
chooseBaber :: String -> IO ()
chooseBaber date = 
    do
        putStrLn("There are two barbers in our barber shop: Tony and Tom")
        putStrLn("Please enter the name of your perferred barber to see his/her schedule:")
        ans <- getLine
        if (elem ans ["Tony", "tony", "TONY"])
            then do 
                printSchedule "tony" date
        else if (elem ans ["Tom", "tom", "TOM"])
            then do
                printSchedule "tom" date
        else if (elem ans ["quit", "QUIT", "Q"])
            then do 
                putStrLn("You are leaving the booking system....")
        else 
            do
                putStrLn("There are no such barber in our shop, please rechoose your baber") 
                chooseBaber date


-- take the csv file at preferred date then show the slotlist as table
printSchedule :: [Char] -> [Char] -> IO ()
printSchedule name date =
    do
        let fileName = name ++ date ++ ".csv"
        doExist <- (doesFileExist fileName)
        if doExist
            then do
                file <- readCsv (fileName)
                let slotList = readCsvToSlot file
                -- note slotList = [FreeSlot {time = Ten},TakenSlot {time = Twelve, name = "Amy"}, ...]
                let printableSlot = toPrintableString slotList
                putStrLn(name ++ " has the following schedule: ")
                putStrLn("---------------------------------------------")
                putStrLn(printableSlot)
                checkSchedule name date slotList 
                
        else 
            do
                file <- readCsv (name ++ ".csv")
                let slotList = readCsvToSlot file
                -- note slotList = [FreeSlot {time = Ten},TakenSlot {time = Twelve, name = "Amy"}, ...]
                let printableSlot = toPrintableString slotList
                putStrLn(name ++ " has the following schedule: ")
                putStrLn("---------------------------------------------")
                putStrLn(printableSlot)
                checkSchedule name date slotList
                



-- take the slotlist of the asked day by barber's name then process new schedule booking for time
checkSchedule :: [Char] -> [Char] -> [TimeSlot]  -> IO ()
checkSchedule name date slotList= 
    do 
        putStrLn("When whould you like to design your hair (please enter in form of xx:00)")
        inputTimeAsString <- getLine 
        let timeWanted = convertStringtoTime inputTimeAsString
        if checkAva slotList timeWanted
            then do 
                    processBooking name timeWanted slotList date
        else do 
                -- error "the time you pick is not avaliable, please choose another time"
                putStrLn("The time you pick is unavailable today.")
                putStrLn("...")
                let timePick = take 2 inputTimeAsString -- "12"
                let timePicked = read timePick::Integer

                if (timePicked > 10 || timePicked < 17)
                    then do
                        putStrLn("Trying to find the nearest available time today for " ++ name ++ " ...")
                        putStrLn("...")
                        checkNearest name slotList timePicked timePicked date
                else do
                        putStrLn("This is not the working time for " ++ name ++ ", please check other avaliable time.")
                        putStrLn("...")


        
-- find the nearest availble time to schedule, process booking schedule
-- when entered time is not availble for the selected barber
checkNearest :: String -> [TimeSlot] -> Integer -> Integer -> String -> IO ()
checkNearest name slotList earlier later date = 
    if (earlier > 10 || later < 17)
        then do
            let sete = earlier-1
            let setl = later+1
            let se = show sete
            let sl = show setl

            let availableList = [se, sl]
            let availabless = map (\ x -> x++":00") availableList
            let availables = map convertStringtoTime availabless

            let results = existFreeSlot availables slotList
            if length results > 0
                then do
                    putStrLn("The nearest available time is: ")
                    let timeResults = map convertTimetoString results
                    let printResults = toSimpleTimeString timeResults
                    putStrLn("---------------------------------------------")
                    putStrLn(printResults)
                    putStrLn("---------------------------------------------")
                    putStrLn("Please enter the time (in form of xx:00) if you want to schedule the time listed above")
                    inputTimeAsString <- getLine 
                    let timeWanted = convertStringtoTime inputTimeAsString
                    if checkAva slotList timeWanted
                        then do 
                            processBooking name timeWanted slotList date
                    else do 
                        putStrLn("Thank you for using barber shop scheduler. ")
            else do
                putStrLn("No time is available today for " ++ name ++ ", please check other date or baber.")
                
    else do
            putStrLn("No time is available today for " ++ name ++ ", please check other date or barbers.")
            


-- takes a list of time and a schedule csv of a barber then produce a list of time which are available in the csv file
existFreeSlot :: [Time] -> [TimeSlot] -> [Time]
existFreeSlot [] slotList = []
existFreeSlot lst slotList = filter (checkAva slotList) lst

-- takes barber's name, time needed to be scheduled and list of schedules in csv file then save the new schedule to the csv file

processBooking name timeWanted slotList date = 
    do
        putStrLn("What's your prefered name(Do not leave it empty)")
        preferredName <- getLine
        putStrLn ("Adding your booking to the schedule...")
        let newSlot = TakenSlot timeWanted preferredName
        let newSchedule = addNewBooking slotList newSlot
        let newStringSchedule = toCsv newSchedule
        when (length newStringSchedule > 0) $
            writeFile (name ++ date ++ ".csv") newStringSchedule
        putStrLn ("reschedule complete, here's the new schedule for " ++ name ++ " on " ++ date)
        newfile <- readCsv (name ++ date ++ ".csv")
        let newSlotList = readCsvToSlot newfile
        let newPrintableSlot = toPrintableString newSlotList
        putStrLn(newPrintableSlot)



-- convert Time to String which is easier to read
convertTimetoString :: Time -> String
convertTimetoString t 
    | t == Ten = "10:00"
    | t == Eleven = "11:00"
    | t == Twelve = "12:00"
    | t == Thirteen = "13:00"
    | t == Fourteen = "14:00"
    | t == Fifteen = "15:00"
    | t == Sixteen = "16:00"
    | t == Seventeen = "17:00"
    | otherwise = error "not working time"

-- convert String to Time 
convertStringtoTime :: String -> Time
convertStringtoTime str 
    | str == "0:00" = Zero
    | str == "1:00" = One
    | str == "2:00" = Two
    | str == "3:00" = Three
    | str == "4:00" = Four
    | str == "5:00" = Five
    | str == "6:00" = Six
    | str == "7:00" = Seven
    | str == "8:00" = Eight
    | str == "9:00" = Nine
    | str == "10:00" = Ten 
    | str == "11:00" = Eleven 
    | str == "12:00" = Twelve 
    | str == "13:00" = Thirteen 
    | str == "14:00" = Fourteen 
    | str == "15:00" = Fifteen 
    | str == "16:00" = Sixteen 
    | str == "17:00" = Seventeen 
    | str == "18:00" = Eighteen
    | str == "19:00" = Nineteen
    | str == "20:00" = Twenty
    | otherwise = error "not working time"


-- data type for the Time (in order)
data Time = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Eleven
           | Twelve | Thirteen | Fourteen | Fifteen | Sixteen 
           | Seventeen | Eighteen | Nineteen | Twenty 
            deriving (Ord, Eq, Show, Read, Typeable)

data TimeSlot = TakenSlot { time :: Time, name :: [Char]}   
                | FreeSlot { time :: Time }
                deriving (Ord, Eq, Show)


-- take a slot, check if it is a FreeSlot
-- as there are no way to check for the inside constructor, 
-- we choose to check if there are element of "freeslot" when show the slot to string and split by ' '
isFree :: Show a => a -> Bool
isFree ts = (elem "FreeSlot" (splitSep (== ' ') (show ts)))

-- take a list of timeslot and a time, check if the time is a freeslot
checkAva :: [TimeSlot] -> Time -> Bool
checkAva [] _ = False 
checkAva (h:t) newTime  
    | time (h) == newTime = isFree h 
    | otherwise = checkAva t newTime

-- take a list of time slot and produce a string contain all of the elements 
toPrintableString :: [TimeSlot] -> String
toPrintableString [] = ""
toPrintableString lst = concatMap printSlot lst
    where
        printSlot (TakenSlot time name) = (convertTimetoString time) ++ "-----" ++ name ++ "\n"
        printSlot (FreeSlot time) = (convertTimetoString time) ++ "-----"  ++ "\n"

-- takes a list of time of string type then produce a string contains all the times
toSimpleTimeString :: [String] -> String
toSimpleTimeString [] = ""
toSimpleTimeString lst = concatMap convertTimetoStr lst
    where
        convertTimetoStr t = t ++ "\n"

-- take a list of timeslot and a new timeslot to change the original one
addNewBooking :: [TimeSlot] -> TimeSlot -> [TimeSlot]
addNewBooking [] slot = []
addNewBooking (h:t) slot 
    | time (h) == time (slot) = slot : t
    | otherwise = h : addNewBooking t slot

-- take a list of list of string and read it to a list of timeslot
readCsvToSlot :: [[String]] -> [TimeSlot]
readCsvToSlot [[]] = []
readCsvToSlot [] = []
readCsvToSlot lst = map converttoSlot lst

-- take a list of string and convert to a single timeslot
converttoSlot :: [String] -> TimeSlot
converttoSlot (a : b : c) = TakenSlot (read a :: Time) b
converttoSlot (a : b ) = FreeSlot (read a :: Time)

-- take a list of timeslot and convert them into a list of string
toString :: [TimeSlot] -> [String]
toString [] = [""]
toString lst = map printSlot lst 
    where 
        printSlot (TakenSlot time name) = (show time) ++ "," ++  name
        printSlot (FreeSlot time) = (show time) 

--take a list of timeslot and merge tham to a single string for csv file
toCsv :: [TimeSlot] -> String
toCsv [] = ""
toCsv lst = mergeWith "\n" (toString lst)

-- take a list of string with a string to put inbetween each element
mergeWith :: String -> [String] -> String
mergeWith str [a] = a
mergeWith str (h:t) =
    h ++ str ++ (mergeWith str t)


-- convert string inform of "YYYY-MM-DD" to date type (fromGregorian YYYY MM DD)
convertToDate :: String -> Day
convertToDate str = 
    let
        dateList = splitSep (== '-') str
        yearNum = read (dateList!!0) :: Integer 
        monthNum = read (dateList!!1) :: Int
        dayNum = read (dateList!!2) :: Int  
    in (fromGregorian yearNum monthNum dayNum)
    

-- check if the string  is in format of "YYYY-MM-DD" and is a valid date
isYearFormat :: [Char] -> Bool
isYearFormat [] = False 
isYearFormat str 
    | length lst == 3 = validComb year month date 
    | otherwise = False 
    where 
        lst = splitSep (== '-') str
        year = read (lst!!0) :: Integer 
        month = read (lst!!1) :: Int
        date = read (lst!!2) :: Int
        validComb y m d 
            | (elem m [1, 3, 5, 7, 8, 10, 12]) = d >= 1 && d <= 31
            | (elem m [4, 6, 9, 11]) = d >= 1 && d <= 30
            | m == 2 = d >= 1 && d <= 29
            | otherwise = False 



-- credit to David Poole, Homework 3 Question 3
splitSep :: (a -> Bool) -> [a] -> [[a]]
splitSep f [] = [[]]
splitSep f (h:t)
    | f h = [] : splitSep f t
    | otherwise = ((h:t1):t2) where t1:t2 = splitSep f t

-- credit to David Poole, Homework 3 Question 3
readCsv :: FilePath -> IO [[[Char]]]
readCsv fileName = 
    do
        file <- readFile fileName
        return [splitSep (== ',') line | line <- splitSep (== '\n') file]

