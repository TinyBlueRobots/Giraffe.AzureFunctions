[<AutoOpen>]
module Giraffe.Routing

open FormatExpressions
open System.Net.Http
open System.Text.RegularExpressions
open ModelBinding

let route (path : string) : HttpHandler =
    fun ctx ->
        match ctx.Request.RequestUri.AbsolutePath = path with
        | true -> finish ctx
        | _ -> abort

let routef (path : PrintfFormat<_, _, _, _, 'T>)
    (routeHandler : 'T -> HttpHandler) : HttpHandler =
    validateFormat path
    fun ctx ->
        tryMatchInput path ctx.Request.RequestUri.AbsolutePath false
        |> function
        | Some args -> routeHandler args ctx
        | _ -> abort

let text (str : string) : HttpHandler =
    fun ctx ->
        ctx.Response.Content <- new StringContent(str)
        finish ctx

let routeBind<'T> (route : string) (routeHandler : 'T -> HttpHandler) : HttpHandler =
    fun ctx ->
        let pattern = route.Replace("{", "(?<").Replace("}", ">[^/\n]+)") |> sprintf "^%s$"
        let regex   = Regex(pattern, RegexOptions.IgnoreCase)
        let result  = regex.Match ctx.Request.RequestUri.AbsolutePath
        match result.Success with
        | true ->
            let groups = result.Groups
            let result =
                regex.GetGroupNames()
                |> Array.skip 1
                |> Array.map (fun n -> n, groups.[n].Value)
                |> dict
                |> ModelParser.tryParse None
            match result with
            | Error _  -> abort
            | Ok model -> routeHandler model ctx
        | _ -> abort
