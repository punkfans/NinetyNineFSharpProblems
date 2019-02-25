module NinetyNineFSharpProblems.p9

/// Pack concecutive duplicates of list elements into sublists
/// If a list contains repeated elements they should be placed 
/// in separate sublists.
///  
/// Example: 
/// * (pack '(a a a a b c c a a d e e e e))
/// ((A A A A) (B) (C C) (A A) (D) (E E E E))
///  
/// Example in F#: 
/// 
/// > pack ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 
///         'a'; 'd'; 'e'; 'e'; 'e'; 'e']
/// val it : char list list =
///  [['a'; 'a'; 'a'; 'a']; ['b']; ['c'; 'c']; ['a'; 'a']; ['d'];
///   ['e'; 'e'; 'e'; 'e']]

let f1 xs =
    let collect x acc =
        match acc with
        | (y::ys)::zs when x = y -> (x::y::ys)::zs
        | zss -> [x]::zss
        
    List.foldBack collect xs []