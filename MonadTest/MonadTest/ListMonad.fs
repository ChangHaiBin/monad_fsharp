module ListMonad


type ListBuilder ()=
    member this.Bind(x,f) =
        List.collect f x

    member this.Return x = 
        [x]

    member this.ReturnFrom x = 
        x

let list = new ListBuilder()

///////////////////////////////////////
let x = [1;2;3]
let y = [40;50]

let w1 = 
    list {
        let! a = x
        let! b = y
        return a + b 
    }

////////////////////////////////////////

let (>>=) x f = List.collect f x

let w2 = 
    x >>= fun a ->
    y >>= fun b ->
        [a + b]

////////////////////////////////////////

type MutableList () =
    let mutable content = List.Empty
    member this.Add(x) =
        content <- x::content
    member this.ShowResult() =
        content |> List.rev

let w_OO =
    let mut_array = new MutableList()
    for a in x do
        for b in y do
            mut_array.Add(a + b)
    mut_array

let result3 = w_OO.ShowResult()