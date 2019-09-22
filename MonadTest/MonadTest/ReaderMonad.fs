module ReaderMonad

type Reader<'Z,'A> = 'Z -> 'A

let Bind (f: 'A -> Reader<'L,'B>) (x: Reader<'L,'A>) : Reader<'L,'B> =
    fun logger ->
        let a = x logger
        let y = f a
        y logger

type ReaderBuilder ()=
    member this.Bind(x,f) =
        Bind f x

    member this.Return x = 
        fun logger -> x

    member this.ReturnFrom x = 
        x

let reader = new ReaderBuilder()

/////////////////////////////////////////////

let ConsoleLogger a =
    printfn "The value is: %A" a

let f1 x logger =
    let res = x * 1
    logger res
    res
let f2 x logger =
    let res = x * 2
    logger res
    res
let f3 x logger =
    let res = x * 3
    logger res
    res
let f4 x logger =
    let res = x * 4
    logger res
    res
let f5 x logger =
    let res = x * 5
    logger res
    res

let w1 = 
    reader {
        let! a = f1 10000
        let! b = f2 1000
        let! c = f3 100
        let! d = f4 10
        let! e = f5 1
        return a + b + c + d + e
    }

let result1 = w1 ConsoleLogger

///////////////////////////////////

let (>>=) x f = Bind f x

let w2 = 
    f1 10000 >>= fun a ->
    f2 1000  >>= fun b ->
    f3 100   >>= fun c ->
    f4 10    >>= fun d ->
    f5 1     >>= fun e ->
        fun logger -> a + b + c + d + e

let result2 = w2 ConsoleLogger

////////////////////////////////////

let w3 =
    let a = f1 10000 ConsoleLogger
    let b = f2 1000  ConsoleLogger
    let c = f3 100   ConsoleLogger
    let d = f4 10    ConsoleLogger
    let e = f5 1     ConsoleLogger
    a + b + c + d + e
    