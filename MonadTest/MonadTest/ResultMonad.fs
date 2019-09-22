module ResultMonad


type ResultBuilder ()=
    member this.Bind(x,f) =
        Result.bind f x

    member this.Return x = 
        Result.Ok x

    member this.ReturnFrom x = 
        x

let res = new ResultBuilder()

//////////////////////////////////

let x = Result.Ok 1
let y = Result.Error "Error2"
let z = Result.Ok 3

let w1 :Result<int,string> = 
    res {
        let! a = x
        let! b = y
        let! c = z
        return a + b + c
    }

//////////////////////////////////

let (>>=) x f = Result.bind f x

let w2 :Result<int,string> =  
    x >>= fun a ->
    y >>= fun b ->
    z >>= fun c ->
        Result.Ok (a + b + c)

///////////////////////////////////

let GetValue x = 
    match x with
    | Result.Ok a -> a
    | Result.Error _ -> failwith "Failure to Get Value"

let w_OO =
    try
        let a = GetValue x
        let b = GetValue y
        let c = GetValue z
        a + b + c
    with
        excep -> 
            printfn "%s" (excep.ToString())
            0