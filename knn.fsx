#r "nuget: FSharp.Data"
open FSharp.Data
open System
printfn "f#"

let euclidean_dist ((A,B): double list * double list)=
    (List.fold (fun acc (x1: double,x2: double) -> acc + ((x1-x2)**2)) 0.0 (List.zip A B))**0.5

printfn "%A" (euclidean_dist ([1.0; 2.0; 3.0], [1.0; 2.0; 3.0]))

let csv = CSVFile.load( "./wine.data")
//let knn (x_train, )