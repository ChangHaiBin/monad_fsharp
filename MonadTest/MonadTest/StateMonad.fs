module StateMonad

type State<'A,'S> = 'S -> 'A * 'S

let Bind (f: 'A -> State<'B,'S>) (x: State<'A,'S>) : State<'B,'S> = 
    fun s1 ->
        let (a,s2) = x s1
        let y = f a
        y s2

type StateBuilder ()=
    member this.Bind(x,f) =
        Bind f x

    member this.Return x = 
        fun s -> (x,s)

    member this.ReturnFrom x = 
        x

let state = new StateBuilder()

//////////////////////////////////////////////////

let x = 
    fun s1 -> 
        let rand = float(s1) / 100.0
        let s2 = (s1 * 97) % 100
        printfn "OldSeed: %2i, NewSeed: %2i, ------ RandValue: %f " s1 s2 rand 
        (rand,s2)

let w1 = 
    state {
        let! a = x
        let! b = x
        let! c = x
        let! d = x
        let! e = x
        return a + b + c + d + e
    }

let result1 = w1 97

//////////////////// Show example using Random Number and Queue/Pop
let (>>=) x f = 
    Bind f x

let w2 = 
    x >>= fun a ->
    x >>= fun b ->
    x >>= fun c ->
    x >>= fun d ->
    x >>= fun e ->
        fun s -> (a+b+c+d+e, s)

let result2 = w2 97

///////////////////////////////////////////////////

type Random_Simple (seed: int) =
    let mutable state = seed

    member this.GetDouble() =
        let rand = float(state) / 100.0 
        let newState = (state * 97) % 100
        state <- newState
        rand
        
let result3 =
    let random = new Random_Simple(97)
    let a = random.GetDouble()
    let b = random.GetDouble()
    let c = random.GetDouble()
    let d = random.GetDouble()
    let e = random.GetDouble()
    a + b + c + d + e