module NinetyNineFSharpProblems.p10

/// p10
/// Run-length encoding of a list
/// Use the result of problem P09 to implement the so-called run-length 
/// encoding data compression method. Consecutive duplicates of elements 
/// are encoded as lists (N E) where N is the number of duplicates of the element E.
///  
/// Example: 
/// * (encode '(a a a a b c c a a d e e e e))
/// ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
///  
/// Example in F#: 
/// 
/// encode <| List.ofSeq "aaaabccaadeeee"
/// val it : (int * char) list =
///   [(4,'a');(1,'b');(2,'c');(2,'a');(1,'d');(4,'e')]

/// p9
let p9 xs =
    let collect x acc =
        match acc with
        | (y::ys)::zs when x = y -> (x::y::ys)::zs
        | zss -> [x]::zss
        
    List.foldBack collect xs []
    
let p10 xs =
    xs |> p9 |> List.map (fun x -> ((List.length x), (List.head x)))
    
/// p11
/// Modify the result of problem 10 in such a way that if an element has no duplicates it 
/// is simply copied into the result list. Only elements with duplicates are transferred as
/// (N E) lists.
///  
/// Example: 
/// * (encode-modified '(a a a a b c c a a d e e e e))
/// ((4 A) B (2 C) (2 A) D (4 E))
///  
/// Example in F#: 
/// 
/// > encodeModified <| List.ofSeq "aaaabccaadeeee"
/// val it : char Encoding list =
///   [Multiple (4,'a'); Single 'b'; Multiple (2,'c'); Multiple (2,'a');
///    Single 'd'; Multiple (4,'e')]
///  
type 'a Encoding = Multiple of int * 'a | Single of 'a

let p11 xs =
    xs |> p9 |> List.map (fun x ->
            if List.length x > 1 then
                Multiple ((List.length x), (List.head x))
            else
                Single (List.head x)
        )