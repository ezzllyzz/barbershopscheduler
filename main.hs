import Data.List (minimumBy, sortBy)
import Control.Monad (when)
import Data.Ord (comparing)
import Data.Time
import Data.Typeable
import System.IO

run = 
    do
        hSetBuffering stdin LineBuffering
        openingTerminalText
        putStrLn("There are two barbers in our shop, Tony and Tom")
        putStrLn("Enter the name for your perferred barber:")
        ans <- getLine
        if (elem ans ["Tony", "tony", "TONY"])
            then do
                printSchedule "tony"
        else if (elem ans ["Tom", "tom", "TOM"])
            then do
                printSchedule "tom"
        else 
            putStrLn("banana") 




printSchedule :: String -> IO ()
printSchedule name =
    do
        file <- readCsv (name ++ ".csv")
        let slotList = readCsvToSlot file
        let printableSlot = toPrintableString slotList
        putStrLn(name ++ " has the following schedule: ")
        putStrLn("---------------------------------------------")
        putStrLn(printableSlot)
        putStrLn("When whould you like to design your hair (please enter in form of xx:00)")
        inputTimeAsString <- getLine 
        let timeWanted = convertStringtoTime inputTimeAsString
        if checkAva slotList timeWanted
            then do 
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
        else error "the time you pick is not avaliable, please choose another time"


openingTerminalText :: IO ()
openingTerminalText = 
    do
        putStrLn("")
        putStrLn("************************\n")
        putStrLn("Hello! Welcome to Barber Scheduler.")





-- data type for the Time (in order)
data Time =  Ten | Eleven
           | Twelve | Thirteen | Fourteen | Fifteen
           | Sixteen | Seventeen 
            deriving (Ord, Eq, Show, Read, Typeable)

data TimeSlot = TakenSlot { time :: Time, name :: String}   
                | FreeSlot { time :: Time }
                deriving (Ord, Eq, Show)

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
    | str == "10:00" = Ten 
    | str == "11:00" = Eleven 
    | str == "12:00" = Twelve 
    | str == "13:00" = Thirteen 
    | str == "14:00" = Fourteen 
    | str == "15:00" = Fifteen 
    | str == "16:00" = Sixteen 
    | str == "17:00" = Seventeen 
    | otherwise = error "not working time"

-- take a slot, check if it is a FreeSlot
ifFreeslot :: Typeable a => a -> Bool
-- ifFreeslot ts = typeOf ts == typeOf (FreeSlot Twelve)
ifFreeslot ts = typeOf ts == typeOf FreeSlot

-- take a list of timeslot and a time, check if the time is a freeslot
checkAva :: [TimeSlot] -> Time -> Bool
checkAva (h:t) newTime  
    | time (h) == newTime = ifFreeslot h 
    | otherwise = True

-- take a list of timeslot and a new timeslot to change the original one
addNewBooking :: [TimeSlot] -> TimeSlot -> [TimeSlot]
addNewBooking [] slot = []
addNewBooking (h:t) slot 
    | time (h) == time (slot) = slot : t
--    | otherwise = checkAva t newTime
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

-- take a list of time slot and produce a string contain all of the element 
toPrintableString :: [TimeSlot] -> String
toPrintableString [] = ""
toPrintableString lst = concatMap printSlot lst
    where
        printSlot (TakenSlot time name) = (convertTimetoString time) ++ "-----" ++ name ++ "\n"
        printSlot (FreeSlot time) = (convertTimetoString time) ++ "-----"  ++ "\n"



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
