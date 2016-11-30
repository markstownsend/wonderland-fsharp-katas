// See the file alphabet-cipher.md for detailed information.

type Message = string
type Keyword = string

// constant of the character array, why doesn't this work with the array comprehension let a = [| 'a' .. 'z' |]
let cAlphabet = [|'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z'|]

// converts the char array to a string, nice use of fold
let charArrayAsString (seed:char[])  = 
    let conv = seed |> Array.map(fun f -> f.ToString())
    conv |> Array.fold(+) ""

// switches the line around to effectively rotate
let rotatedLine line start = 
    let oldOrder = line |> Array.splitAt start
    fst oldOrder |> Array.append (snd oldOrder)

// creates the substitution array
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

// gets the index of the character in an alphabet array
let alphabetIndex (findChar:char) (findIn:char[]) = 
    findIn |> Array.findIndex (fun x -> x = findChar)

// finds the character at the character co-ordinates in the alphabet square
let findMe (substitutionSquare:char[][]) (x:char) (y:char) = 
    let topLine = substitutionSquare.[0]
    let iX = alphabetIndex x topLine
    let iY = alphabetIndex y topLine
    substitutionSquare.[iX].[iY]

// get the first column and look up the the keyword character, then go along 
// that row to find the cipher character, then index that into the first row
// to get the clear text char
let findMeBack (substitutionSquare:char[][]) (x:char) (y:char) = 
    let firstCol = substitutionSquare  |> Array.map(fun a -> a.[0])
    let iX = alphabetIndex x firstCol
    let iY = alphabetIndex y substitutionSquare.[iX]
    substitutionSquare.[0].[iY]

// gets all the permutations of the keyword array, taking each
// number of characters in turn and copying that through the rest 
// of the array such that if the keyword was: 'redre' then the permutations
// would be rrrrr, rerer, redre, redrr, redre
// The order is important because the index of the matching keyword, in this case
// can be either 2 or 4 but the lowest is chosen meaning the secret key is the first
// 'index' + 1 characters out of the keyword array.  In this example the keyword is
// 'red' not 'redre'.  Although both are theoretically valid.
let rec permuteArray(seedArray:char[]) (permutations:char[][]) (current:int) = 
    let iNext = current + 1
    let max = seedArray.Length
    let target = Array.create iNext 'x'
    match current < max with
    | false -> permutations 
    | true -> 
        Array.blit seedArray 0 target 0 iNext 
        let padded = padSeed (Array.create max 'x') target 0 max
        permutations.[current] <- padded
        permuteArray seedArray permutations iNext

// encodes
let encode (key:Keyword) (message:Message) : Message =
    let lenMsg = message.Length
    let filledKey = padSeed (Array.create lenMsg 'x') (key.ToCharArray()) 0 lenMsg
    let cMsg = message.ToCharArray()
    let cipherSquare = substitutionChart cAlphabet
    let encoded = Array.map2 (fun x y -> (findMe cipherSquare x y)) filledKey cMsg
    charArrayAsString encoded

// decodes
let decode (key:Keyword) (message:Message) : Message =
    let lenMsg = message.Length
    let filledKey = padSeed (Array.create lenMsg 'x') (key.ToCharArray()) 0 lenMsg
    let cMsg = message.ToCharArray()
    let cipherSquare = substitutionChart cAlphabet
    let decoded = Array.map2 (fun x y -> (findMeBack cipherSquare x y)) filledKey cMsg
    charArrayAsString decoded

//// decipher gets the keyword out of the message and the cipher, once it
//// arrives back at the padded keyword, has to find the repeating sequence, the first repeat is the keyword
//// presuming the message is longer than the keyword
//// take the plain text letter and find it in the first column
//// go along that column until you find the cipher text letter
//// the letter in the first row of those co-ordinates is the letter in the keyword
let decipher (cipher:Message) (message:Message) : Keyword =
    let cipherSquare = substitutionChart cAlphabet
    let cMsg = message.ToCharArray()
    let cCpr = cipher.ToCharArray() 
    let bigKey = Array.map2 (fun x y -> (findMeBack cipherSquare x y)) cMsg cCpr
    let permutations = permuteArray bigKey (Array.create bigKey.Length (Array.create bigKey.Length 'x'))
    let p = permutations 0  //why do I have to invoke this in this step, I thought I had already done that in the line above
    let matching = p |> Array.mapi(fun i c -> i, c) |> Array.where(fun c -> (snd c) = bigKey)
    let key = (snd matching.[0]) |> Array.take ((fst matching.[0]) + 1)
    charArrayAsString key 


#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =
    
    let perTestArr = 
        let a = Array2D.create 4 4 'x'
        a.[0,0] <- 'a'
        a.[0,1] <- 'a'
        a.[0,2] <- 'a'
        a.[0,3] <- 'a'
        a.[1,0] <- 'a'
        a.[1,1] <- 'b'
        a.[1,2] <- 'a'
        a.[1,3] <- 'b'
        a.[2,0] <- 'a'
        a.[2,1] <- 'b'
        a.[2,2] <- 'a'
        a.[2,3] <- 'a'
        a.[3,0] <- 'a'
        a.[3,1] <- 'b'
        a.[3,2] <- 'a'
        a.[3,3] <- 'b'
        a
 
 //   test <@ permuteArray [|'i';'c';'e';'i';'c'|] [| [|'x';'x';'x';'x';'x'|]; [|'x';'x';'x';'x';'x'|]; [|'x';'x';'x';'x';'x'|]; [|'x';'x';'x';'x';'x'|]; [|'x';'x';'x';'x';'x'|] |] 0 = [|[|'i';'i';'i';'i';'i'|];[|'i';'c';'i';'c';'i'|];[|'i';'c';'e';'i';'c'|];[|'i';'c';'e';'i';'i'|];[|'i';'c';'e';'i';'c'|]|] @>

        
//    // verify encoding
    test <@ encode "vigilance" "meetmeontuesdayeveningatseven" = "hmkbxebpxpmyllyrxiiqtoltfgzzv" @>
    test <@ encode "scones" "meetmebythetree" = "egsgqwtahuiljgs" @>
//
//    // verify decoding
    test <@ decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" = "meetmeontuesdayeveningatseven" @>
    test <@ decode "scones" "egsgqwtahuiljgs" = "meetmebythetree" @>
//
//    // verify decyphering
    test <@ decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" = "vigilance" @>
    test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" = "scones" @>

    // verify utilities
    test <@ charArrayAsString [|'a';'b';'c'|] = "abc" @>
    test <@ rotatedLine [|'a';'b';'c'|] 1 = [|'b';'c';'a'|]@>
    test <@ charArrayAsString (rotatedLine [|'a';'b';'c'|] 1) = "bca" @>
    test <@ substitutionChart [|'a'|] = [|[|'a'|]|]@>
    test <@ substitutionChart [|'a';'b'|] = [|[|'a';'b'|];[|'b';'a'|]|]@>
    test <@ substitutionChart [|'a';'b';'c'|] = [|[|'a';'b';'c'|];[|'b';'c';'a'|];[|'c';'a';'b'|]|]@>
    test <@ adjustSeedIndex 5 5 = 0 @>
    test <@ adjustSeedIndex 5 4 = 1 @>
    test <@ adjustSeedIndex 5 3 = 2 @>
    test <@ adjustSeedIndex 3 1 = 0 @>
    test <@ padSeed [|'x';'x';'x';'x'|] [|'b'|] 0 4 = [|'b';'b';'b';'b'|] @>
    test <@ padSeed [|'x';'x';'x';'x'|] [|'a';'b';'c';'d'|] 0 4 = [|'a';'b';'c';'d'|] @>
    test <@ padSeed [|'x';'x'|] [|'a';'b';'c';'d'|] 0 2 = [|'a';'b';|] @>
    test <@ alphabetIndex 'a' cAlphabet = 0 @>
    test <@ alphabetIndex 'n' cAlphabet = 13 @>
    test <@ (substitutionChart cAlphabet).[0].[0] = 'a'@>
    test <@ (substitutionChart cAlphabet).[0].[12] = 'm'@>
    test <@ (substitutionChart cAlphabet).[3].[0] = 'd'@>
    test <@ (substitutionChart cAlphabet).[3].[4] = 'h'@>
    test <@ (substitutionChart cAlphabet).[(alphabetIndex 'v' cAlphabet)].[(alphabetIndex 'm' cAlphabet)] = 'h'@>
    test <@ (substitutionChart cAlphabet).[(alphabetIndex 'm' cAlphabet)].[(alphabetIndex 'v' cAlphabet)] = 'h'@>
    test <@ charArrayAsString (padSeed (Array.create 20 'c') [|'v';'i';'g';'i';'l';'a';'n';'c';'e'|] 0 20) = "vigilancevigilancevi" @>
//    test <@ permuteArray [|'a';'b';'a';'b'|] [| [|'x';'x';'x';'x'|]; [|'x';'x';'x';'x'|]; [|'x';'x';'x';'x'|]; [|'x';'x';'x';'x'|]|] 0 = [|[|'a';'a';'a';'a'|];[|'a';'b';'a';'b'|];[|'a';'b';'a';'a'|];[|'a';'b';'a';'b'|]|] @>
    test <@ permuteArray [|'i';'c';'e';'i';'c'|] [| [|'x';'x';'x';'x';'x'|]; [|'x';'x';'x';'x';'x'|]; [|'x';'x';'x';'x';'x'|]; [|'x';'x';'x';'x';'x'|]; [|'x';'x';'x';'x';'x'|] |] 0 = [|[|'i';'i';'i';'i';'i'|];[|'i';'c';'i';'c';'i'|];[|'i';'c';'e';'i';'c'|];[|'i';'c';'e';'i';'i'|];[|'i';'c';'e';'i';'c'|]|] @>
    //test <@ permuteArray [|'a';'b';'a';'b'|] (Array2D.create 4 4 'x') 0 = perTestArr @>
    //test <@ permuteArray [|'i';'c';'e';'i';'c'|] [| [|'x';'x';'x';'x';'x'|]; [|'x';'x';'x';'x';'x'|]; [|'x';'x';'x';'x';'x'|]; [|'x';'x';'x';'x';'x'|]; [|'x';'x';'x';'x';'x'|] |] 0 = [|[|'i';'i';'i';'i';'i'|];[|'i';'c';'i';'c';'i'|];[|'i';'c';'e';'i';'c'|];[|'i';'c';'e';'i';'i'|];[|'i';'c';'e';'i';'c'|]|] @>

// run the tests
tests ()

