import Data.List (minimumBy, sortBy)
import Control.Monad (when)
import Data.Ord (comparing)
import Data.Time
import Data.Typeable
import System.IO


test = [FreeSlot {time = Ten},TakenSlot {time = Twelve, name = "Amy"}]

run = 
    do
        
        hSetBuffering stdin LineBuffering
        openingTerminalText
        putStrLn("There are two barbers in our barber shop: Tony and Tom")
        putStrLn("Enter the name of your perferred barber:")
        ans <- getLine
        if (elem ans ["Tony", "tony", "TONY"])
            then do
                printSchedule "tony"
        else if (elem ans ["Tom", "tom", "TOM"])
            then do
                printSchedule "tom"
        else 
            putStrLn("banana") 



-- take the csv file by barber's name then process new schedule booking
printSchedule :: String -> IO ()
printSchedule name =
    do
        file <- readCsv (name ++ ".csv")
        let slotList = readCsvToSlot file
        -- note slotList = [FreeSlot {time = Ten},TakenSlot {time = Twelve, name = "Amy"}, ...]
        let printableSlot = toPrintableString slotList
        putStrLn(name ++ " has the following schedule: ")
        putStrLn("---------------------------------------------")
        putStrLn(printableSlot)
        putStrLn("When whould you like to design your hair (please enter in form of xx:00)")
        inputTimeAsString <- getLine 
        let timeWanted = convertStringtoTime inputTimeAsString
        if checkAva slotList timeWanted
            then do 
                processBooking name timeWanted slotList
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
                    checkNearest name slotList timePicked timePicked 
            else do
                putStrLn("No time is available today for " ++ name ++ ", please check other barbers.")
                putStrLn("...")

        
-- find the nearest availble time to schedule, process booking schedule
-- when entered time is not availble for the selected barber
checkNearest :: String -> [TimeSlot] -> Integer -> Integer -> IO ()
checkNearest name slotList earlier later = 
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
                            processBooking name timeWanted slotList
                    else do 
                        putStrLn("Thank you for using barber shop scheduler. ")
            else do
                checkNearest name slotList sete setl
    else do
            putStrLn("No time is available today for " ++ name ++ ", please check other barbers.")


-- takes a list of time and a schedule csv of a barber then produce a list of time which are available in the csv file
existFreeSlot :: [Time] -> [TimeSlot] -> [Time]
existFreeSlot [] slotList = []
existFreeSlot lst slotList = filter (checkAva slotList) lst

-- takes barber's name, time needed to be scheduled and list of schedules in csv file then save the new schedule to the csv file
processBooking :: String -> Time -> [TimeSlot] -> IO ()
processBooking name timeWanted slotList = 
    do
        putStrLn("What's your prefered name(Do not leave it empty)")
        preferredName <- getLine
        putStrLn ("Adding your booking to the schedule...")
        let newSlot = TakenSlot timeWanted preferredName
        let newSchedule = addNewBooking slotList newSlot
        let newStringSchedule = toCsv newSchedule
        when (length newStringSchedule > 0) $
            writeFile (name ++ ".csv") newStringSchedule
        putStrLn ("reschedule complete, here's the new schedule for " ++ name)
        newfile <- readCsv (name ++ ".csv")
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

data TimeSlot = TakenSlot { time :: Time, name :: String}   
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
        printSlot (TakenSlot time name) = (show time) ++ "," ++ (show name)
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

-- produce opening welcome text
openingTerminalText :: IO ()
openingTerminalText = 
    do
        putStrLn("")
        putStrLn("************************\n")
        putStrLn("Hello! Welcome to Barber Scheduler.")

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
