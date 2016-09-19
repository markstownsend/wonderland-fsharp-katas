// See the file alphabet-cipher.md for detailed information.

type Message = string
type Keyword = string

let charArrayAsString (seed:char[])  = 
    let conv = seed |> Array.map(fun f -> f.ToString())
    conv |> Array.fold(+) ""

let rotatedLine line start = 
    let oldOrder = line |> Array.splitAt start
    fst oldOrder |> Array.append (snd oldOrder)

let substitutionChart (seed:char[]) = 
    let substC = seed |> Array.mapi(fun i c -> rotatedLine seed i)
    substC

// simply cycling round the seed, so whatever the iterator it can't exceed the seed bounds
let adjustSeedIndex current seedLength = 
    let lessThan = seedLength - current
    match lessThan > 0 with
    | true -> current
    | false -> current % seedLength

// pads the seed into the key array
let rec padSeed (filled:char[]) (seed:char[]) (current:int) (max:int) = 
    match (max - current) <= 0 with
    | true -> filled
    | false ->
            let seedIndex = adjustSeedIndex current seed.Length 
            filled.[current] <- seed.[seedIndex]
            padSeed filled seed (current + 1) max

//let adjustKey (key:string) (target:int) = 
//    let lengthAdjustment = target - key.Length
//    match lengthAdjustment <= 0 with
//    | true -> key.Substring(0, key.Length + lengthAdjustment).ToCharArray()
//    | false -> fillArray Array.create 'x' key 0 target 

let alphabetIndex (findChar:char) (findIn:char[]) = 
    findIn |> Array.findIndex (fun x -> x = findChar)

let findMe (substitutionSquare:char[][]) (x:char) (y:char) = 
    let topLine = substitutionSquare.[1]
    let iX = alphabetIndex x topLine
    let iY = alphabetIndex y topLine
    substitutionSquare.[iX].[iY]

let encode (key:Keyword) (message:Message) : Message =
    let cleanMessage = message.Replace(" ","").ToUpper()
    let lenMsg = cleanMessage.Length
    let filledKey = padSeed (Array.create lenMsg 'x') (key.ToCharArray()) 0 lenMsg
    let cMsg = cleanMessage.ToCharArray()
    let cAlphabet = [|'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z'|]
    let cipherSquare = substitutionChart cAlphabet

    let encoded = 
        cMsg 
            |> Array.map2 (fun x y -> (findMe cipherSquare x y) cMsg filledKey)
//            |> Array.map2 (fun x y -> cipherSquare.[(alphabetIndex x cAlphabet)].[(alphabetIndex y cAlphabet)] cMsg filledKey)
    
    charArrayAsString encoded

let decode (key:Keyword) (message:Message) : Message =
    "decodeme"

let decipher (cipher:Message) (message:Message) : Keyword =
    "decypherme"


#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

//    // verify encoding
//    test <@ encode "vigilance" "meetmeontuesdayeveningatseven" = "hmkbxebpxpmyllyrxiiqtoltfgzzv" @>
//    test <@ encode "scones" "meetmebythetree" = "egsgqwtahuiljgs" @>
//
//    // verify decoding
//    test <@ decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" = "meetmeontuesdayeveningatseven" @>
//    test <@ decode "scones" "egsgqwtahuiljgs" = "meetmebythetree" @>
//
//    // verify decyphering
//    test <@ decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" = "vigilance" @>
//    test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" = "scones" @>

    // verify utilities
    test <@ charArrayAsString [|'A';'B';'C'|] = "ABC" @>
    test <@ rotatedLine [|'A';'B';'C'|] 1 = [|'B';'C';'A'|]@>
    test <@ charArrayAsString (rotatedLine [|'A';'B';'C'|] 1) = "BCA" @>
    test <@ substitutionChart [|'A'|] = [|[|'A'|]|]@>
    test <@ substitutionChart [|'A';'B'|] = [|[|'A';'B'|];[|'B';'A'|]|]@>
    test <@ substitutionChart [|'A';'B';'C'|] = [|[|'A';'B';'C'|];[|'B';'C';'A'|];[|'C';'A';'B'|]|]@>
    test <@ adjustSeedIndex 5 5 = 0 @>
    test <@ adjustSeedIndex 5 4 = 1 @>
    test <@ adjustSeedIndex 5 3 = 2 @>
    test <@ adjustSeedIndex 3 1 = 0 @>
    test <@ padSeed [|'x';'x';'x';'x'|] [|'B'|] 0 4 = [|'B';'B';'B';'B'|] @>
    test <@ padSeed [|'x';'x';'x';'x'|] [|'A';'B';'C';'D'|] 0 4 = [|'A';'B';'C';'D'|] @>
    test <@ padSeed [|'x';'x'|] [|'A';'B';'C';'D'|] 0 2 = [|'A';'B';|] @>


// run the tests
tests ()

