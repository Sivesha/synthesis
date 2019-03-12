module Synthesis

open System.Diagnostics

let abelar a= (12 < a && a < 3097 && a%2=0 )
   // failwith "Not implemented"

let area b h =
    match b >= 0.0 && h>=0.0 with
        |true -> (0.5*b)*h
        |_->  failwith " Base and/or height are negative"
   

let zollo x =
    match x > 0  with
        |true-> x*2
        |false->x-x + (-x)
    //failwith "Not implemented"
     
let min x y =
    match x < y with
    |true-> x
    |false -> y
    //failwith "Not implemented"

let max x y =
    match x > y with
    |true -> x
    |false -> y
    //failwith "Not implemented"

let ofTime h m s = (h * 3600 ) + (m * 60 )+ s
    //failwith "Not implemented"

let toTime time =
    match time <= 0 with
    |true -> (0,0,0)
    |false ->
        let h = time/3600
        let r1 = time - (3600*h)
        let m = r1/60
        let s = r1 - ( 60*m)      
        (h,m,s)
 
    //failwith "Not implemented"

let digits n =
    let rec count x c= 
      match x=0 with   
      |true -> c
      |false -> count (x/10)(c+1)
    match n>0 || n<0 with
      |true ->count n 0
      | false -> 1
  

let minmax (n1,n2,n3,n4) =
     let minV  = min n1  n2 |> min n3 |> min n4
     let maxV  = max n1 n2 |> max n3 |> max n4
     minV,maxV

let isLeap y =
     match y < 1582  with
      |true -> failwith " Invalid year"
      |false ->
                match y % 4 = 0 && not (y % 100 = 0)  with 
                |true-> true
                |false -> 
                    match y % 400 = 0 with
                    | true -> true
                    |false -> false
   

let month mm = match mm with
                |1-> ("January" , 31)
                |2-> ("February", 28)
                |3-> ("March",31)
                |4-> ("April",30)
                |5-> ("May",31)
                |6-> ("June",30)
                |7-> ("July",31)
                |8 -> ("August",31)
                |9-> ("September",30)
                |10-> ("October", 31)
                |11-> ("November",30)
                |12->("December",31)
                |_ -> failwith "Invalid number"
  

let toBinary b =
    let rec binString x y =
        match x=0 with
        |true -> y
        |false ->
            match x%2 with
            |0 -> binString (x/2) ("0" + y)
            |_ -> binString (x/2) ("1" + y)
    match b < 0 with
      |true -> failwith "Error, negative integer supplied "
      |_ ->
            match b=0 with
             |true -> "0"
             |false -> binString b ""

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"