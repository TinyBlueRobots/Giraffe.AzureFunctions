[<AutoOpen>]
module Giraffe.Core

open System.Net.Http
open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Net
open System.Text
open System.Text.RegularExpressions

type HttpContext =
    { Request : HttpRequestMessage
      Response : HttpResponseMessage }

let createHttpContext defaultResponse request =
    { Request = request
      Response = defaultArg defaultResponse <| new HttpResponseMessage() }

let internal finish ctx = Some ctx |> Task.FromResult
let internal abort = Task.FromResult None

type HttpHandler = HttpContext -> Task<HttpContext option>

let execute (httpHandler : HttpHandler) ctx =
    task {
        let defaultResponse = ctx.Response
        let! result = httpHandler ctx
        match result with
        | Some ctx -> return ctx.Response
        | _ -> return defaultResponse
    }

type HttpContext with

    member this.SetHttpHeader key value =
        this.Response.Headers.Add(string key, string value)

    member this.WriteBytesAsync(bytes : byte []) =
        this.SetHttpHeader HttpResponseHeader.ContentLength bytes.Length
        this.Response.Content <- new ByteArrayContent(bytes)

    member this.WriteStringAsync(str : string) =
        Encoding.UTF8.GetBytes str |> this.WriteBytesAsync

    member this.ReadBodyFromRequestAsync() =
        this.Request.Content.ReadAsStringAsync()

type HttpRequestMessage with
    member this.Query =
        Regex.Matches(this.RequestUri.Query, "([^?=&]+)(=([^&]*))?")
        |> Seq.cast<Match>
        |> Seq.toArray
        |> Array.map
               (fun x ->
               x.Groups.[1].Value.ToLower(),
               WebUtility.UrlDecode x.Groups.[3].Value)
        |> dict

let compose (handler1 : HttpHandler) (handler2 : HttpHandler) ctx =
    task {
        let! result = handler1 ctx
        match result with
        | Some ctx -> return! handler2 ctx
        | _ -> return result
    }

let (>=>) = compose

let setStatusCode (httpStatusCode : HttpStatusCode) : HttpHandler =
    fun ctx ->
        ctx.Response.StatusCode <- httpStatusCode
        finish ctx

let choose (handlers : HttpHandler list) : HttpHandler =
    fun ctx ->
        task {
            let mutable result = None
            let mutable i = 0
            while i < handlers.Length do
                let handler = handlers.[i]
                let! r = handler ctx
                match r with
                | None -> i <- i + 1
                | _ ->
                    result <- r
                    i <- handlers.Length
            return result
        }

let private httpVerb (httpMethod : HttpMethod) : HttpHandler =
    fun ctx ->
        match ctx.Request.Method = httpMethod with
        | true -> finish ctx
        | _ -> abort

let DELETE : HttpHandler = httpVerb HttpMethod.Delete
let GET : HttpHandler = httpVerb HttpMethod.Get
let HEAD : HttpHandler = httpVerb HttpMethod.Head
let OPTIONS : HttpHandler = httpVerb HttpMethod.Options
let POST : HttpHandler = httpVerb HttpMethod.Post
let PUT : HttpHandler = httpVerb HttpMethod.Put
let TRACE : HttpHandler = httpVerb HttpMethod.Trace