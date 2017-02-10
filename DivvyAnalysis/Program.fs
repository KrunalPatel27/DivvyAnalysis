#light
//
// <<Krunal Patel>>
// U. of Illinois, Chicago
// CS341, Fall 2016
// HW #04
//
//
// Prompt user for a folder name, and then return a list of all the 
// files in that folder.  Returns empty list if folder doesn't exist
// or cannot be found.
//
let GetFilenames() = 
  printf "Please enter folder name> "
  let folder = System.Console.ReadLine()
  if not (System.IO.Directory.Exists folder) then
    []
  else
    folder
    |> System.IO.Directory.GetFiles
    |> Array.toList


//
// Parse one line of CSV data from a Divvy data file.  The format of each data line
// is as follows:
//
// trip_id,bikeid,tripduration,from_station_id,to_station_id,gender,birthyear
//  8677704,3808,311,283,47,Male,1984
//  ...
//  8677694,170,394,44,44,?,0
//  ...
//  8677645,2452,228,129,274,Female,1987
//  ...
// 
// Right now the function returns a list of two integer elements: [tripID; bikeID].
// You will need to change this.
//
let private ParseLine (line:string) = 
  let elements = line.Split(',')
  let tripID = System.Convert.ToInt32(elements.[0])
  let bikeID = System.Convert.ToInt32(elements.[1])
  let duration = System.Convert.ToInt32(elements.[2])
  let fromStationID = System.Convert.ToInt32(elements.[3])
  let toStationID = System.Convert.ToInt32(elements.[4])
  let gender = elements.[5]
  let birthYear = System.Convert.ToInt32(elements.[6])
  //
  // how do you want to store the data?  you can return the values as 
  // a small list of integers [tripID;bikeID;...], or you can return
  // the values as a tuple (tripID, bikeID, ...).  There's really no 
  // difference other than how you process, and that tuples allow you 
  // to mix types, while a list requires that all values be the same
  // type.  This implies you would need to convert the gender --- "Male",
  // "Female", or "?" to an integer code (which might be a smart move
  // anyway.
  //
  (
  ( tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear)
  )



// 
// Parses 1 file of Divvy data, where the format of each line is 
// discussed above; returns back a list of elements where the format
// of each element is discussed above.
//
// NOTE: the "|>" means pipe the data from one function to
// the next.  The code below is equivalent to letting a var
// hold the value and then using that var in the next line.
//
let private ParseDivvyFile filename = 
  System.IO.File.ReadLines(filename)
  |> Seq.skip 1  // skip header row:
  |> Seq.map ParseLine
  |> Seq.toList
  
let memberList list =
    let membersList = list |> List.filter (fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> (birthYear <> 0 && gender <> "?"))
    membersList

let nonMemberList list =
    let nonmembersList = list |> List.filter (fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> (birthYear = 0 || gender = "?"))
    nonmembersList

let numberOfMen list =
    let numberofmen = list |> List.filter (fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> ( gender = "Male"))
    let foundTotalNumOfMale =  float (List.length numberofmen)
    foundTotalNumOfMale

let numberOfFemale list =
    let numberoffemale = list |> List.filter (fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> ( gender = "Female"))
    let foundTotalNumOfFemale =  float (List.length numberoffemale)
    foundTotalNumOfFemale

let numberOfTotalMOF list =
    let numberofMOF = list |> List.filter (fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> ( gender = "Female" || gender = "Male"))
    let foundTotalNumOfMof =  float (List.length numberofMOF)
    foundTotalNumOfMof

let absFilterMale list =
    let numberofmen = list |> List.filter (fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> (birthYear <> 0 && gender = "Male"))
    numberofmen

let absFilterFemale list =
    let numberoffemale = list |> List.filter (fun (tripID, bikeID, duration, fromStationID, toStationID, gender, birthYear) -> (birthYear <> 0 && gender = "Female"))
    numberoffemale

let averageAgeList list =
    let curYear = System.DateTime.Now.Year
    let averageTotal =  List.map (fun (_,_,_,_,_,_,y) -> (float(curYear - y)) ) list
    let monthlyAverage = float( List.average averageTotal)
    monthlyAverage

let averageTime list =
    let timeForEachRide = List.map (fun (_,_,y,_,_,_,_) -> ((y)) ) list
    timeForEachRide

let Part1Duration list =
    let list1 = List.filter (fun (x) -> (x<=(30*60))) list
    let length = List.length list1
    let lengthList = List.length list
    let Percentage = (((float (length)/float (lengthList))) *100.00)
    printfn"1..30   mins: %A (%A)%%" length Percentage
    list1
let Part2Duration list =
    let list2 = List.filter (fun (x) -> ( (x>(30*60)) && (x<=(60*60)) ) ) list
    let length = List.length list2
    let lengthList = List.length list
    let Percentage = ( ((float(length)/float(lengthList))) *100.00)
    printfn"31..60  mins: %A (%A)%%" length Percentage
    list2
let Part3Duration list =
    let list3 = List.filter (fun (x) ->  (x>(60*60)) && (x<=(120*60))) list
    let length = List.length list3
    let lengthList = List.length list
    let Percentage = (((float(length)/float(lengthList))) *100.00)
    printfn"61..120 mins: %A (%A)%%" length Percentage
    list3
let Part4Duration list =
    let list4 = List.filter (fun (x) -> (x>(120*60))) list
    let length = List.length list4
    let lengthList = List.length list
    let Percentage = (((float(length)/float(lengthList))) *100.00)
    printfn"120+    mins: %A (%A)%%" length Percentage
    list4
   

[<EntryPoint>]
let main argv = 
  printfn ""
  printfn "** Divvy Ride Analysis **"
  printfn ""
  //
  let files = GetFilenames()
  //
  // input the data into a LIST OF LISTS, one per file.  The format
  // of each sub-list is determined by the "ParseDivvyFile" function:
  //
  let data = List.map ParseDivvyFile files
  //
  //printfn "%A"  data  // debugging to see data format:
  //
  printfn ""
  let totalRiders = List.map List.length data
  printfn "** # of rides:   %A" (totalRiders)
  
  let localMembers = List.map memberList data
  printfn "** Members:      %A" (List.map List.length localMembers)

  let localNonMembers = List.map nonMemberList data
  printfn "** Non-members:  %A" (List.map List.length localNonMembers)
  //Number of Male and Female
  let totalNumOfMOF = List.map numberOfTotalMOF data
  let findNumMen = List.map numberOfMen data
  let findNumFemale = List.map numberOfFemale data
  printfn ""
  let percentMale = List.map2 (fun x y -> ((x / y) * 100.00)) findNumMen totalNumOfMOF 
  printfn "** %% of men:     %A" (percentMale)

  let percentFemale = List.map2 (fun x y -> ((x / y) * 100.00)) findNumFemale totalNumOfMOF 
  printfn "** %% of women:   %A" (percentFemale)
  printfn ""

  //Average AGE Part
  printfn "Average age:"

  //men average
  let absFilterfindNumMen = List.map absFilterMale data // filter list a second time to get rid of members with age given as 0
  let listConcatMale = List.concat absFilterfindNumMen //concat list of filtered male list
  let MaleAVG =  averageAgeList listConcatMale
  printfn "Male:%A" MaleAVG

  //female average 
  let absFilterfindNumWomen = List.map absFilterFemale data // filter list a second time to get rid of members with age given as 0
  let listConcatFemale = List.concat absFilterfindNumWomen // concat list of filtered female list
  let FemaleAVG =  averageAgeList listConcatFemale
  printfn "Male:%A" FemaleAVG

  //Time Duration
  let concatData = List.concat data // concat into 1 big list
  printfn""
  let filterDuration = averageTime concatData // extract time info from all tuples inside the list
  let part1= Part1Duration filterDuration
  let part2= Part2Duration filterDuration
  let part3= Part3Duration filterDuration
  let part4= Part4Duration filterDuration




  //
  // done:
  //
  printfn ""
  printfn "** Done **"
  printfn ""
  printfn ""
  0
