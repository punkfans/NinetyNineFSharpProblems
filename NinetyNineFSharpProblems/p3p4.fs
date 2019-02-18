module NinetyNineFSharpProblems.p3p4

/// P3: Find the Kth element in a list, the index starts from 1
/// Example: 
/// * (element-at '(a b c d e) 3)
/// c
/// 
/// Example in F#: 
/// > elementAt [1; 2; 3] 2;;
/// val it : int = 2
/// > elementAt (List.ofSeq "fsharp") 5;;
/// val it : char = 'r'

let f1 xs k =
    xs |> List.item (k - 1)
    
/// p4: Find the number of elements in a list
/// Example in F#: 
/// 
/// > myLength [123; 456; 789];;
/// val it : int = 3
/// > myLength <| List.ofSeq "Hello, world!"
/// val it : int = 13


let f4 xs =
    xs |> List.length

let f4' xs =
    xs |> List.sumBy (fun _ -> 1)