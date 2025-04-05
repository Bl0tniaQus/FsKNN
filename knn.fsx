open System
let euclidean_dist ((A,B): double list * double list)=
    (List.fold (fun acc (x1: double,x2: double) -> acc + ((x1-x2)**2)) 0.0 (List.zip A B))**0.5
let accuracy_score ((T,Y): double list * double list)=
    (List.fold2 (fun acc t y -> if t=y then acc+1.0 else acc) 0.0 T Y) / double T.Length
let knn (x_train : double list array) (y_train : double array) (x_test : double list) (k : int)=
    let classes = Set.toList (Set y_train)
    let mnn (distances : double list)=
        let mutable y_trains = y_train
        let mutable dists = distances
        List.fold(fun ys x -> 
            let idx = List.findIndex(fun x -> x = (List.min dists)) dists
            let elem = y_trains[idx]
            dists <-List.removeAt idx dists
            y_trains <-Array.removeAt idx y_trains
            ys @ [elem]
        ) [] [1..k]
    let count_ys (ys : double list)=
        List.fold(fun acc y -> acc @ [double (List.filter(fun yy -> yy=y) ys).Length]) [] classes

    let distances = Array.fold(fun acc x -> acc @ [euclidean_dist (x, x_test)]) [] x_train
    match k with
    | 1 -> y_train[List.findIndex(fun x -> x = (List.min distances)) distances]
    | _ -> let cys = count_ys (mnn distances)
           let idx = List.findIndex(fun x -> x = List.max cys) cys
           (List.toArray classes)[idx]

let datafile_train = Seq.toList (System.IO.File.ReadLines("data_train.csv"));
let data_train = List.toArray (List.fold (fun acc (row:string) -> acc @ [(List.fold(fun acc2 x -> acc2 @ [double x]) [] (Array.toList (row.Split ',')))]) [] datafile_train)
let y_train = List.toArray (Array.fold(fun acc (x:double list) -> acc @ [x.Head]) [] data_train)
let x_train = List.toArray (Array.fold(fun acc (x:double list) -> acc @ [x.Tail]) [] data_train)

let datafile_test = Seq.toList (System.IO.File.ReadLines("data_test.csv"));
let data_test = List.toArray (List.fold (fun acc (row:string) -> acc @ [(List.fold(fun acc2 x -> acc2 @ [double x]) [] (Array.toList (row.Split ',')))]) [] datafile_test)
let y_test = List.toArray (Array.fold(fun acc (x:double list) -> acc @ [x.Head]) [] data_test)
let x_test = List.toArray (Array.fold(fun acc (x:double list) -> acc @ [x.Tail]) [] data_test)



let y_predicted = Array.fold(fun acc x -> acc @ [knn x_train y_train x 2]) [] x_test 
printfn "%A" (accuracy_score (Array.toList y_test,y_predicted))