module IOMonad

type IO<'A> = unit -> Result<'A,string>

let Bind (f: 'A -> IO<'B>) (x: IO<'A>) : IO<'B> =
    fun () ->
        match x () with
        | Result.Ok(a) -> (f a) ()
        | Result.Error(b) -> Result.Error(b)

type IOBuilder ()=
    member this.Bind(x,f) =
        Bind f x

    member this.Return x = 
        fun logger -> Result.Ok(x)

    member this.ReturnFrom x = 
        x

let io = new IOBuilder()

////////////////////////////////////////////

let Read fileName : IO<string> = 
    fun () ->
        try
            System.IO.File.ReadAllText fileName
            |> Result.Ok
        with
            exn -> Result.Error "File Reading Error"
let Print s : IO<unit> =
    fun () ->
        printfn "%s" s
        Result.Ok(())
let Write fileName s : IO<unit> =
    fun () ->
        try
            System.IO.File.WriteAllText(fileName,s)
            |> Result.Ok
        with
            exn -> Result.Error "File Writing Error."

// Please replace the file path below with the actual path.
let x = Read @"TEST1.txt"
let y = Read @"TEST2.txt"

let w1 = 
    io {
        let! text1 = x
        let! text2 = y

        let count1 = text1.Length
        let count2 = text2.Length

        do! Print ("The first file has " + (string count1) + " characters.")
        do! Print ("The second file has " + (string count2) + " characters.")

        let combinedText = text1 + text2
        
        // Please replace the file path below with the actual path.
        do! Write @"TEST3.txt" combinedText

        return count1 + count2
    }

let result1 = w1()

////////////////////////////////////////////////////

let (>>=) x f = Bind f x
let (>>) x y = Bind (fun () -> y) x

let w2 =  
    x >>= fun text1 ->
    y >>= fun text2 ->

    let count1 = text1.Length
    let count2 = text2.Length

    Print ("The first file has " + (string count1) + " characters.") >>
    Print ("The second file has " + (string count2) + " characters.") >> 

    let combinedText = text1 + text2
    
    // Please replace the file path below with the actual path.
    Write @"TEST3.txt" combinedText >> 

    fun () -> Result.Ok (count1 + count2)
    
let result2 = w2()

//////////////////////////////////////////////////////////////

// Please replace the file path below with the actual path.
let w_impure = 
    let text1 = System.IO.File.ReadAllText @"TEST1.txt"
    let text2 = System.IO.File.ReadAllText @"TEST2.txt"

    let count1 = text1.Length
    let count2 = text2.Length

    printfn "The first file has %i characters." count1 
    printfn "The second file has %i characters." count2
    
    let combinedText = text1 + text2

    System.IO.File.WriteAllText(@"TEST3.txt",combinedText)

    count1 + count2

