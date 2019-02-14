module NinetyNineFSharpProblems.problem1

/// Find the last element of a list
/// Example in F#: 
/// > myLast [1; 2; 3; 4];;
/// val it : int = 4
/// > myLast ['x';'y';'z'];;
/// val it : char = 'z'

let rec f1 xs =
    match xs with
    | [] -> "empty list"
    | [x] -> x
    | _::ys -> f1 ys

let f2 xs = xs |> List.rev |> List.head

let f3 xs = xs |> List.reduce (fun _ x -> x)