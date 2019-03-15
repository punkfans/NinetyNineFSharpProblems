module NinetyNineFSharpProblems.p17

///Problem 17 : Split a list into two parts; the length of the first part is given.
/// Do not use any predefined predicates. 
/// 
/// Example: 
/// * (split '(a b c d e f g h i k) 3)
/// ( (A B C) (D E F G H I K))
///  
/// Example in F#: 
/// 
/// > split (List.ofSeq "abcdefghik") 3
/// val it : char list * char list =
///   (['a'; 'b'; 'c'], ['d'; 'e'; 'f'; 'g'; 'h'; 'i'; 'k'])

let f17 xs length =
    let rec take n ys =
        match n, ys with
        | 0, _ -> []
        | _, [] -> []
        | m, (y::yss) -> y::(take (m - 1) yss)
        
    let rec drop n ys =
        match n, ys with
        | 0, yss -> yss
        | _, [] -> []
        | n', _::yss -> drop(n - 1) yss
        
    take length xs, drop length xs