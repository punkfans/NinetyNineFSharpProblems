module NinetyNineFSharpProblems.p8

/// Eliminate consecutive duplicates of list elements
/// If a list contains repeated elements they should be replaced with a single copy of the 
/// element. The order of the elements should not be changed.
///  
/// Example: 
/// * (compress '(a a a a b c c a a d e e e e))
/// (A B C A D E)
///  
/// Example in F#: 
/// 
/// > compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
/// val it : string list = ["a";"b";"c";"a";"d";"e"]

let f1 xs =
    let rec helper lastPushedElement ys acc =
        match ys with
        | [] -> acc
        | ys1 ->
            let firstElement = List.head ys1
            let remaining = List.tail ys1
            if firstElement = lastPushedElement then
                helper firstElement remaining acc
            else
                helper firstElement remaining (List.append acc (List.init 1 (fun _ -> firstElement)))
        
    helper "" xs []
    
let f2 xs =
    List.foldBack (fun x acc ->
            if List.isEmpty acc then
                [x]
            elif x = List.head acc then
                acc
            else
                x::acc
        ) xs []
    
let a = [1; 2]
let b = 3::a