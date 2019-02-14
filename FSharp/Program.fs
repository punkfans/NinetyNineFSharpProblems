// Learn more about F# at http://fsharp.org

open System
open NinetyNineFSharpProblems

[<EntryPoint>]
let main argv =
    printfn "%A" <| problem1.f1 ["1"; "3"; "6"; "asd"]
    printfn "%A" <| problem1.f2 ["1"; "3"; "6"; "asd"]
    printfn "%A" <| problem1.f3 ["1"; "3"; "6"; "asd"]
    0 // return an integer exit code
