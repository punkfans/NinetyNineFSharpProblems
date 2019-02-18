module NinetyNineFSharpProblems.p2

/// Find the second to the last element in a list
/// (Note that the Lisp transcription of this problem is incorrect.) 
///
/// Example in F#: 
/// myButLast [1; 2; 3; 4];;
/// val it : int = 3
/// > myButLast ['a'..'z'];;
/// val it : char = 'y'

let rec f1 xs =
    match xs with
    | [x1; _] -> x1
    | _::xs1 -> f1 xs1
    | _ -> "list too short!"
    
let f2 xs =
    xs |> List.rev |> List.tail |> List.head
    
let f3 xs =
    let flip f a b = f b a
    /// xs |> List.rev |> flip List.nth 1
    xs |> List.rev |> List.item 1