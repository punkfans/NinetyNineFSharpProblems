module NinetyNineFSharpProblems.p12

/// Decode a run-length encoded list

/// Given a run-length code list generated as specified in problem 11. Construct its 
/// uncompressed version.
///  
/// Example in F#: 
/// 
/// > decodeModified 
///     [Multiple (4,'a');Single 'b';Multiple (2,'c');
///      Multiple (2,'a');Single 'd';Multiple (4,'e')];;
/// val it : char list =
///   ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e']

type 'a Encoding = Multiple of int * 'a | Single of 'a

let p12 xs =
    xs |> List.collect (fun x ->
            match x with
            | Multiple (count, e) -> List.replicate count e
            | Single e -> [e]
        )