module NinetyNineFSharpProblems.p5

/// Reverse a list
/// Example in F#: 
///
/// > reverse <| List.ofSeq ("A man, a plan, a canal, panama!")
/// val it : char list =
///  ['!'; 'a'; 'm'; 'a'; 'n'; 'a'; 'p'; ' '; ','; 'l'; 'a'; 'n'; 'a'; 'c'; ' ';
///   'a'; ' '; ','; 'n'; 'a'; 'l'; 'p'; ' '; 'a'; ' '; ','; 'n'; 'a'; 'm'; ' ';
///   'A']
/// > reverse [1,2,3,4];;
/// val it : int list = [4; 3; 2; 1]


let f1 xs =
    let rec helper acc ys =
        match ys with
        | [] -> acc
        | y::ys1 -> helper (y::acc) ys1
        
    helper [] xs

let f2 xs =
    xs |> List.fold (fun acc x -> x::acc) []