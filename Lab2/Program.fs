// For more information see https://aka.ms/fsharp-console-apps

// Define the Trainer record
type Trainer = {
    Name: string
    PriorAthlete: bool
}

// Define the Status record
type Status = {
    Win: int
    Lose: int
}

// Define the Troupe record
type Troupe = {
    Name: string
    Trainer: Trainer
    Status: Status
}

// Function to calculate success percentage
let winRatio (troupe: Troupe) =
    let win = float troupe.Status.Win
    let lose = float troupe.Status.Lose
    (win / (win + lose)) * 100.0

// Create trainers
let trainers = [
    { Name = "Tyronn Lue"; PriorAthlete = false }
    { Name = "Taylor Jenkins"; PriorAthlete = false }
    { Name = "Chauncey Billups"; PriorAthlete = true }
    { Name = "Ime Udoka"; PriorAthlete = false }
    { Name = "Nick Nurse"; PriorAthlete = false }
]

// Create win/loss status
let status = [
    { Win = 1245; Lose = 1520 }
    { Win = 1200; Lose = 1300 }
    { Win = 1452; Lose = 1689 }
    { Win = 1489; Lose = 2356 }
    { Win = 4578; Lose = 3256 }
]

// Create troupes
let troupes = [
    { Name = "Houston Rockets"; Trainer = trainers.[0]; Status = status.[0] }
    { Name = "Orlando Magic"; Trainer = trainers.[1]; Status = status.[1] }
    { Name = "Atlanta Hawks"; Trainer = trainers.[2]; Status = status.[2] }
    { Name = "Charlotte Hornets"; Trainer = trainers.[3]; Status = status.[3] }
    { Name = "Miami Heat"; Trainer = trainers.[4]; Status = status.[4] }
]

// Filter successful troupes
let successTroupes = troupes |> List.filter (fun troupe -> troupe.Status.Win > troupe.Status.Lose)

// Calculate success percentages
let WinRatios = successTroupes |> List.map winRatio

// Print successful troupes and their success percentages
printfn "Successful Teams:"
List.iter2 (fun troupe percentage -> printfn "%s: Success Percentage = %.2f%%" troupe.Name percentage) successTroupes WinRatios


// DISCRIMINATED UNION -------------------------------------------------------------------------------

// Define the Cookery discriminated union
type Cookery =
    | Korean
    | Turkish

// Define the Experience discriminated union
type Experience =
    | Regular
    | IMAX
    | DBOX
    | RegularWithSnacks
    | IMAXWithSnacks
    | DBOXWithSnacks

// Define the Interest discriminated union
type Interest =
    | BoardGame
    | Chill
    | Movie of Experience
    | Restaurant of Cookery
    | LongDrive of int * float

// Function to calculate the budget for an interest
let computeBudget (interest : Interest) =
    match interest with
    | BoardGame | Chill -> 0.0
    | Movie experience ->
        match experience with
        | Regular -> 12.0
        | IMAX -> 17.0
        | DBOX -> 20.0
        | RegularWithSnacks | IMAXWithSnacks | DBOXWithSnacks -> 5.0 + 12.0 // Adding cost of snacks
    | Restaurant cookery ->
        match cookery with
        | Korean -> 70.0
        | Turkish -> 65.0
    | LongDrive (length, pricePerkm) -> float length * pricePerkm

// Test the function with different activities
let PriceforBoardGame = computeBudget BoardGame
let PriceforChill = computeBudget Chill
let PriceforRegularMovie = computeBudget (Movie Regular)
let PriceforIMAXsnacks = computeBudget (Movie IMAXWithSnacks)
let PriceforKorean = computeBudget (Restaurant Korean)
let PriceforlongDrive = computeBudget (LongDrive (100, 0.05))

// Print the results
printfn "Price for playing a board game: %.2f CAD" PriceforBoardGame
printfn "Price for chilling out: %.2f CAD" PriceforChill
printfn "Price for watching a regular movie: %.2f CAD" PriceforRegularMovie
printfn "Price for watching an IMAX movie with snacks: %.2f CAD" PriceforIMAXsnacks
printfn "Price for going to a Korean restaurant: %.2f CAD" PriceforKorean
printfn "Price for a long drive of 100 km: %.2f CAD" PriceforlongDrive
