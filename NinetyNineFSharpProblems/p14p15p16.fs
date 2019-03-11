module NinetyNineFSharpProblems.p14

/// Problem 14 : Duplicate the elements of a list.
/// Example: 
/// * (dupli '(a b c c d))
/// (A A B B C C C C D D)
///  
/// Example in F#: 
/// 
/// > dupli [1; 2; 3]
/// [1;1;2;2;3;3]


let p14 xs =
    xs |> List.collect (fun x ->
            [x; x]
        )
    

/// problem 15 : Replicate the elements of a list a given number of times.
/// Example: 
/// * (repli '(a b c) 3)
/// (A A A B B B C C C)
///  
/// Example in F#: 
/// 
/// > repli (List.ofSeq "abc") 3
/// val it : char list = ['a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'c'; 'c'; 'c']

let p15 xs count =
    xs |> List.collect (fun x ->
            List.replicate count x
        )
    


/// Problem 16 : Drop every N'th element from a list.
/// Example: 
/// * (drop '(a b c d e f g h i k) 3)
/// (A B D E G H K)
///  
/// Example in F#: 
/// 
/// > dropEvery (List.ofSeq "abcdefghik") 3;;
/// val it : char list = ['a'; 'b'; 'd'; 'e'; 'g'; 'h'; 'k']

let p16 xs n =
    xs
    |> List.mapi (fun i x -> (i+1, x))
    |> List.filter (fun (j, _) -> j % 3 <> 0)
    |> List.map snd