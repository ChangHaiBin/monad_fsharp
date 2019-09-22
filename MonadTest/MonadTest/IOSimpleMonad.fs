module IOSimpleMonad

type IOSimple<'A> = unit -> 'A




let Bind (f: 'A -> IOSimple<'B>) (x: IOSimple<'A>) : IOSimple<'B> =
    fun () ->
        let a = x ()
        let y = f a
        y ()

type IO_Simple_Builder ()=
    member this.Bind(x,f) =
        Bind f x

    member this.Return x = 
        fun logger -> x

    member this.ReturnFrom x = 
        x

let io_simple = new IO_Simple_Builder()

//////////////////////////////////////////

let Print s = fun () -> printfn "%s" s

let x = 
    io_simple {
        do! Print "Console Writing in x."
        return 1
    }
let y = 
    io_simple {
        do! Print "Console Writing in y."
        return 2
    }
let z = 
    io_simple {
        do! Print "Console Writing in z."
        return 3
    }

let w1 = 
    io_simple {
        let! a = x
        let! b = y
        let! c = z
        return a + b + c
    }

let result1 = w1 ()

///////////////////////////////////////////

let (>>=) x f = Bind f x

let w2 = 
    x >>= fun a ->
    y >>= fun b ->
    z >>= fun c ->
        fun () -> a + b + c

let result2 = w2 ()

///////////////////////////////////////////

let x1 () =
    printfn "Printing in x1."
    1
let y1 () =
    printfn "Printing in y1."
    2
let z1 () =
    printfn "Printing in z1."
    3

let w_impure =
    let a = x1()
    let b = y1()
    let c = z1()
    a + b + c
