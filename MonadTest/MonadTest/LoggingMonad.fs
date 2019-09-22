module LoggingMonad

let logInfo x = printfn "The value is: %A" x

type LoggingBuilder () =
    member this.Bind(x,f) =
        logInfo x
        f x

    member this.Return x = 
        x
        
let logging = new LoggingBuilder()

///////////////////////////
let x = 1
let y = 2
let z = 3

let w1 = 
    logging {
        let! a = x
        let! b = y
        let! c = z
        let! d = a + b + c
        return d
    }

//////////////////////////

let (>>=) x f = 
    logInfo x
    f x

let w2 = 
    x >>= fun a ->
    y >>= fun b ->
    z >>= fun c ->
    a + b + c >>= fun d ->
        d

///////////////////////////
let w_slow =
    let a = x
    logInfo x
    
    let b = y
    logInfo y
    
    let c = z
    logInfo z

    let d = a + b + c
    logInfo d

    d