module NinetyNineFSharpProblems.p6

/// Find if a list is a panlindrom
/// A palindrome can be read forward or backward; e.g. (x a m a x).
/// 
/// Example in F#: 
/// > isPalindrome [1;2;3];;
/// val it : bool = false
/// > isPalindrome <| List.ofSeq "madamimadam";;
/// val it : bool = true
/// > isPalindrome [1;2;4;8;16;8;4;2;1];;
/// val it : bool = true


let f1 xs =
    (xs |> List.rev) = xs