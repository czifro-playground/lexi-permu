// Learn more about F# at http://fsharp.org

open System

module Lexer =

  let private getPivot n (arr:int[]) =
    let mutable x = -1
    for i = n-1 downto 0 do
      if i+1 <= n-1 then
        if x = -1 && arr.[i] < arr.[i+1] then
          x <- i
    x

  let private getSwap n (k:int) (arr:int[]) =
    let mutable minDiff = -1
    let mutable y = n-1
    let lb = max k 0
    for i = n-1 downto lb do
      if arr.[i] > arr.[lb] then
        if minDiff = -1 || minDiff > arr.[i] - arr.[lb] then
          minDiff <- arr.[i] - arr.[lb]
          y <- i
    y

  let nextPermutation arr =
    let n = Array.length arr
    let pivot = getPivot n arr
    let swap = getSwap n pivot arr
    if pivot <> -1 then
      let t = arr.[pivot]
      arr.[pivot] <- arr.[swap]
      arr.[swap] <- t
    let mutable i = pivot + 1
    let mutable j = n - 1
    while i <> j && i < j do
      let t = arr.[i]
      arr.[i] <- arr.[j]
      arr.[j] <- t
      i <- i + 1
      j <- j - 1
    arr

module Main =

  [<EntryPoint>]
  let main argv = 
    let mutable arr = [| 1; 2; 3; 4; |] |> Array.rev
    for i in 0..24 do
      printfn "Permutation: %A" arr
      arr <- Lexer.nextPermutation arr
    0

