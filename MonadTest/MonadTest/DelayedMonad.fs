module DelayedMonad

type Delayed<'A> = unit -> 'A

let Bind (f:'A -> Delayed<'B>) (x: Delayed<'A>) : Delayed<'B> =
    fun () -> 
        let a = x ()
        let y = f a
        y ()


type DelayedBuilder ()=
    member this.Bind(x : Delayed<'A>,f : 'A -> Delayed<'B>) =
        Bind f x

    member this.Return x = 
        fun () -> x

    member this.ReturnFrom x = 
        x

let delay = new DelayedBuilder()

/////////////////////////////////////////////

let x = fun () -> [1.0        .. 20000000.0] |> List.sumBy (fun a -> 1.0 / a)
let y = fun () -> [20000000.0 .. 40000000.0] |> List.sumBy (fun a -> 1.0 / a)
let z = fun () -> [40000000.0 .. 60000000.0] |> List.sumBy (fun a -> 1.0 / a)

let w1 = 
    delay {
        let! a = x
        let! b = y
        let! c = z
        return a + b + c
    }

let result1 = w1 ()

////////////////

let (>>=) x f = Bind f x

let w2 = 
    x >>= fun a ->
    y >>= fun b ->
    z >>= fun c ->
        fun () -> a + b + c

let result2 = w2 ()

////////////////////////////////////////////////////////////

let x1 = async { 
    printfn "Task 1 started"
    do! Async.Sleep(200)
    printfn "Task 1 ended"
    return 1 }
let y1 = async { 
    printfn "Task 2 started"
    do! Async.Sleep(3000)
    printfn "Task 2 ended"
    return 2 }
let z1 = async { 
    printfn "Task 3 started"
    do! Async.Sleep(4000)
    printfn "Task 3 ended"
    return 3 }

let test1 = Async.RunSynchronously x1

let w3 = 
    async {
        let! a = x1
        let! b = y1
        let! c = z1
        return a + b + c
    }

let result3 = Async.RunSynchronously w3

let w4 =
    async {
        let! a1 = Async.StartChild x1 
        let! b1 = Async.StartChild y1 
        let! c1 = Async.StartChild z1 
        let! a = a1
        let! b = b1
        let! c = c1
        return a + b + c
    }

let result4 = Async.RunSynchronously w4

////////////////////////////////////////////

open System 

let taski i = async {
    let d1 = System.DateTime.UtcNow
    printfn "Task %i started at %A" i d1
    do! Async.Sleep(20000)
    let d2 = System.DateTime.UtcNow
    printfn "Task %i ended at %A for a job that started at %A" i d2 d1
    return i
}

let qqq = 
    [|1 .. 100000|]
    |> Seq.map taski 
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.sumBy (fun _ -> 1) 
    