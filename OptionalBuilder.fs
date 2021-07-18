module Chess.OptionalBuilder

[<Struct>]
type OptionalBuilder =
    member _.Bind(opt, binder) =
        match opt with
        | Some value -> binder value
        | None -> None

    member _.Return(value) = Some value

let optional = OptionalBuilder()
