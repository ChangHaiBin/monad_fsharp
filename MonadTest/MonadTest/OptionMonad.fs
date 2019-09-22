module OptionMonad

type OptionBuilder ()=
    member this.Bind(x,f) =
        Option.bind f x

    member this.Return x = 
        Some x

    member this.ReturnFrom x = 
        x

let opt = new OptionBuilder()

/////////////////////////////////////////////

let x = Some(1)
let y = None
let z = Some(3)

let w1 = 
    opt {
        let! a = x
        let! b = y
        let! c = z
        return a + b + c
    }

//////////////////////////////////////////////

let (>>=) x f = Option.bind f x

let w2 =
    x >>= fun a ->
    y >>= fun b ->
    z >>= fun c ->
        Some(a + b + c)

//////////////////////////////////////////////

let IsNull x = 
    x |> Option.isNone
let GetValue x = 
    x |> Option.get

let w_OO =
    if IsNull x then 
        if IsNull y then
            if IsNull z then
                let a = GetValue x
                let b = GetValue y
                let c = GetValue z
                Some (a + b + c)
            else
                None
        else
            None
    else
        None