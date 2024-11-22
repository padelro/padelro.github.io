// https://www.cambridge.org/core/services/aop-cambridge-core/content/view/14416CB20C4637164EA9F77097909409/S0956796808006758a.pdf/data-types-a-la-carte.pdf
open System

module Interact =
    type Interact<'T> =
        | ReadLine of (string -> 'T)
        | WriteLine of (string * 'T)
    with
        static member Map(x, f) =
            match x with
            | ReadLine reader -> ReadLine (f << reader)
            | WriteLine (message, writer) -> WriteLine (message, f writer)

module Auth =
    type User = { username: string; password: string; permission: string }

    type Auth<'T> =
        | Login of (string * string * (User option -> 'T)) // Auth<User option>
        | HasPermission of (User option * string * (bool -> 'T)) // Auth<bool>
    with
        static member Map(x, f) =
            match x with
            | Login (username, password, loginSvc) -> Login (username, password, f << loginSvc)
            | HasPermission (user, permission, checker) -> HasPermission (user, permission, f << checker)

// [MARK] : not used yet
module Storage =
    type Storage<'T> =
        | Get of (string * (string -> 'T))
        | Set of (string * 'T)
        | Delete of (string * 'T)
    with
        static member Map(x, f) =
            match x with
            | Get (key, reader) -> Get (key, f << reader)
            | Set (key, writerSet) -> Set (key, f writerSet)
            | Delete (key, writerDelete) -> Delete (key, f writerDelete)

module Logger =
    type Logger<'T> =
        | Log of (int * string * 'T)
    with
        static member Map(x, f) =
            match x with
            | Log (lvl, message, logger) -> Log(lvl, message, f logger)

open Interact
open Auth
open Storage
open Logger

module Free =
    let inline mapS_generic< 'FunctorT, 'FunctorU, 'T, 'U when 'FunctorT: (static member Map: (('FunctorT * ('T -> 'U)) -> 'FunctorU)) > (x: ^FunctorT) (f: ('T -> 'U)) : ^FunctorU =
        'FunctorT.Map(x, f) // (!!)

    type Instr<'T> =
        | Interact of Interact<'T>
        | Auth of Auth<'T>
        | Storage of Storage<'T>
        | Logger of Logger<'T>
    with
        static member Map(x, f) =
            match x with
            | Interact inner -> Interact.Interact<_>.Map(inner, f) |> Interact
            | Auth inner -> Auth.Auth<_>.Map(inner, f) |> Auth
            | Storage inner -> Storage.Storage<_>.Map(inner, f) |> Storage
            | Logger inner -> Logger.Logger<_>.Map(inner, f) |> Logger

    type Free<'T> =
        | Pure of 'T
        | Roll of Instr<Free<'T>>
        | Delayed of (unit -> 'T)

    let liftF x = Roll x

    let mapS x f = Instr<Free<_>>.Map(x, f)

    let bind f x =
        let rec loop f x =
            match x with
            | Pure x -> f x
            | Roll x -> Roll (mapS x (loop f))
            | Delayed f -> failwith "??"
        loop f x

    type FreeBuilder () =
        member this.Bind (x, f) = x |> bind f
        member this.Combine (x, f) = this.Bind(x, f)
        member this.ReturnFrom (x) = x
        member this.Zero () = Pure ()
        member this.Delay f = f
    let free = FreeBuilder()

open Free

// #nowarn 40

module Impl =
    let readLine = Free.liftF (ReadLine Pure |> Interact)
    let writeLine s = Free.liftF (WriteLine (s, Pure()) |> Interact)

    let login username password = Free.liftF(Login (username, password, Pure) |> Auth)
    let hasPermission user permission = Free.liftF(HasPermission (user, permission, Pure) |> Auth)

    // storage (!!)

    let log lvl message= Free.liftF((Log (lvl, message, Pure())) |> Logger)

    let rec interpret =
        function
        | Pure x -> x
        | Delayed f -> f()

        | Roll (Interact (ReadLine next))  ->
            (
                Console.ReadLine()
            )
            |> next
            |> interpret
        | Roll (Interact(WriteLine (s, next))) ->
            Console.WriteLine s

            next
            |> interpret

        | Roll (Auth (Login(username, password, next))) ->
            (
                if(username = "admin" && password = "admin")
                then Some { username = password; password = password; permission = "PERM" }
                else None
            )
            |> next
            |> interpret
        | Roll (Auth (HasPermission(user, permission, next))) ->
            (
                user |> Option.map(fun u -> u.permission = permission) |> Option.defaultValue false
            )
            |> next
            |> interpret

        | Roll (Storage (Get(key, next))) ->
            Console.WriteLine("Get key: {0}.", key)

            (
                $"{key}_value"
            )
            |> next
            |> interpret
        | Roll (Storage (Set(key, next))) ->
            Console.WriteLine("Set key: {0}.", key)

            next
            |> interpret
        | Roll (Storage (Delete(key, next))) ->
            Console.WriteLine("Deleted key: {0}.", key)
            next
            |> interpret

        | Roll (Logger (Log(lvl, mesage, next))) ->
            Console.WriteLine("Log[{0}]: {1}", lvl, mesage)

            next
            |> interpret

    let program =
        free {
            do! writeLine "Username:"
            let! username = readLine

            do! writeLine "Password:"
            let! password = readLine

            let! user = login username password
            let! hasPerms = hasPermission user "PERM"

            if hasPerms then
                do! log 0 "OK!"
            else
                do! log 1 "DENIED"

            return! writeLine "DONE"
        }
        |> fun d -> d()

printfn "%A" Impl.program

Impl.program
|> Impl.interpret