[<AutoOpen>]
module Giraffe.HttpStatusCodeHandlers

open Core
open System.Net

module Successful =
    let ok x = setStatusCode HttpStatusCode.OK >=> x

module RequestErrors =
    let notFound x = setStatusCode HttpStatusCode.NotFound >=> x
