module Tests

open Expecto
open System.Net
open System.Net.Http
open System
open System.Threading.Tasks
open Giraffe

[<CLIMutable>]
type Model = {
    Name : string
}

let app =

    choose [

        POST >=> routef "/foo/%s" text

        GET >=> routeBind<Model> "/routebind/{name}" (fun model -> fun ctx -> text model.Name ctx)

        GET >=> route "/bindquery" >=> bindQuery<Model> None (fun model -> fun ctx -> text model.Name ctx)

        GET >=> route "/foo" >=> text "bar"

        GET >=> text "foo"

    ]

let readText (response : HttpResponseMessage) = response.Content.ReadAsStringAsync().Result

let httpRequest (path : string) httpMethod = new HttpRequestMessage(Method = httpMethod, RequestUri = Uri(Uri "http://test", path))

let result (task : Task<_>) = task.Result

[<Tests>]
let tests =

    testList "" [

        test "GET / returns OK foo" {
            let response =
                httpRequest "/" HttpMethod.Get
                |> createHttpContext None
                |> execute app
                |> result
            let text = response |> readText
            Expect.equal text "foo" "unexpected text"
            Expect.equal response.StatusCode HttpStatusCode.OK "unexpected status code"
        }

        test "GET /foo returns OK bar" {
            let response =
                httpRequest "/foo" HttpMethod.Get
                |> createHttpContext None
                |> execute app
                |> result
            let text = response |> readText
            Expect.equal response.StatusCode HttpStatusCode.OK "unexpected status code"
            Expect.equal text "bar" "unexpected text"
        }

        test "GET /routebind/foo returns OK foo" {
            let response =
                httpRequest "/routebind/foo" HttpMethod.Get
                |> createHttpContext None
                |> execute app
                |> result
            let text = response |> readText
            Expect.equal response.StatusCode HttpStatusCode.OK "unexpected status code"
            Expect.equal text "foo" "unexpected text"
        }

        test "GET /bindquery?name=foo returns OK foo" {
            let response =
                httpRequest "/bindquery?name=foo" HttpMethod.Get
                |> createHttpContext None
                |> execute app
                |> result
            let text = response |> readText
            Expect.equal response.StatusCode HttpStatusCode.OK "unexpected status code"
            Expect.equal text "foo" "unexpected text"
        }

        test "POST /foo/bar returns OK bar" {
            let response =
                httpRequest "/foo/bar" HttpMethod.Post
                |> createHttpContext None
                |> execute app
                |> result
            let text = response |> readText
            Expect.equal response.StatusCode HttpStatusCode.OK "unexpected status code"
            Expect.equal text "bar" "unexpected text"
        }

    ]