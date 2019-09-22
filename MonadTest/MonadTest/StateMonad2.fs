module StateMonad2


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

//////////////////////////////////

let Pop = 
    fun s -> 
        let head = s |> List.head
        let tail = s |> List.tail
        (head,tail)

let Push a =
    fun s ->
        let combined = a :: s
        (() ,  combined)

let w1 = 
    state {
        do! Push 1
        do! Push 20
        let! a = Pop
        let! b = Pop
        let! c = Pop
        return a + b + c 
    }

let result1 = w1 [300]

///////////////////////////////////////

let (>>=) x f = Bind f x
let (>>) x f = 
    fun s1 ->
        let ((),s2) = x s1
        f s2

let w2 =
    Push 20 >>
    Push 1 >>
    Pop >>= fun a ->
    Pop >>= fun b ->
    Pop >>= fun c ->
        fun s -> (a + b + c, s)

let result2 = w2 [300]

/////////////////////////////////////////

type Stack_Simple () =
    let mutable state : int list= []
    member this.Push(x) = 
        state <- x :: state
    member this.Pop() = 
        let value = state |> List.head
        let tail = state |> List.tail
        state <- tail
        value

        
let w_OO =
    let stack = new Stack_Simple()

    stack.Push(300)
    stack.Push(20)
    stack.Push(1)

    let a = stack.Pop()
    let b = stack.Pop()
    let c = stack.Pop()

    a + b + c