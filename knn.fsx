open System
printfn "f#"

let euclidean_dist ((A,B): double list * double list)=
    (List.fold (fun acc (x1: double,x2: double) -> acc + ((x1-x2)**2)) 0.0 (List.zip A B))**0.5
let accuracy_score ((T,Y): double list * double list)=
    (List.fold2 (fun acc t y -> if t=y then acc+1.0 else acc) 0.0 T Y) / double T.Length
let knn (x_train : double list array) (y_train : double array) (x_test : double list) (k : int)=
    let distances = Array.fold(fun acc x -> acc @ [euclidean_dist (x, x_test)]) [] x_train
    match k with
    | 1 -> y_train[List.findIndex(fun x -> x = (List.min distances)) distances]
    | _ -> -1

let datafile_train = Seq.toList (System.IO.File.ReadLines("data_train.csv"));
let data_train = List.toArray (List.fold (fun acc (row:string) -> acc @ [(List.fold(fun acc2 x -> acc2 @ [double x]) [] (Array.toList (row.Split ',')))]) [] datafile_train)
let y_train = List.toArray (Array.fold(fun acc (x:double list) -> acc @ [x.Head]) [] data_train)
let x_train = List.toArray (Array.fold(fun acc (x:double list) -> acc @ [x.Tail]) [] data_train)

let datafile_test = Seq.toList (System.IO.File.ReadLines("data_test.csv"));
let data_test = List.toArray (List.fold (fun acc (row:string) -> acc @ [(List.fold(fun acc2 x -> acc2 @ [double x]) [] (Array.toList (row.Split ',')))]) [] datafile_test)
let y_test = List.toArray (Array.fold(fun acc (x:double list) -> acc @ [x.Head]) [] data_test)
let x_test = List.toArray (Array.fold(fun acc (x:double list) -> acc @ [x.Tail]) [] data_test)



let y_predicted = Array.fold(fun acc x -> acc @ [knn x_train y_train x 1]) [] x_test 
printfn "%A" (accuracy_score (Array.toList y_test,y_predicted))