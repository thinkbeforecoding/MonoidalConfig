
open System
open System.Configuration



// this is a case insensitive string wrapper
[<Struct;CustomComparison;CustomEquality>]
type PropertyName = 
    val name: string
    new(name) = { name = name}
    override this.ToString() = this.name

    static member private comparer = StringComparer.InvariantCultureIgnoreCase

    interface IComparable with
        member this.CompareTo(other) = 
            match other with
            | :? PropertyName as p -> PropertyName.comparer.Compare(this.name, p.name)
            | _ -> -1

    override this.Equals(other) = 
        match other with
        | :? PropertyName as p -> PropertyName.comparer.Equals(this.name, p.name) 
        | _ -> false

    override this.GetHashCode() = PropertyName.comparer.GetHashCode(this.name)


// this is a configuration property definition
// it contains:
// * a name
// * a parser to convert from string
// * a default value
// * a combiner to combine two values
type Prop<'T> = Prop of name: PropertyName * parser: (string -> 'T) * defaultValue: 'T * combine: ('T -> 'T -> 'T)

// construct a configuration property
let prop name parser defaultValue combiner =
    Prop(PropertyName name, parser, defaultValue, combiner)
   
module Prop =
    let name = function Prop(n,_,_,_) -> n
    let defaultValue = function Prop(_,_,d,_) -> d
    let parse = function Prop(_,p,_,_) -> p
    let combiner = function Prop(_,_,_,c) -> c

// box a function of two parameters
let boxf2 f = fun x y -> box (f (unbox x) (unbox y))

// always return the second argument
let replace x y = y




// A configuration map
// each entry contains the value, the source of the value, and the combiner.
type Config = Config of Map<PropertyName, obj * string * (obj -> obj -> obj)>
    with

    // An empty configuration
    static member empty = Config Map.empty

    // Creates configuration from a property definition, a value and a source
    static member property (prop: Prop<'T>) (value:'T) source =
            let name = Prop.name prop
            let combiner = Prop.combiner prop
            Map.empty
            |> Map.add name (box value, source, boxf2 combiner)
            |> Config

    // Creates configuration from a property definition, a value and a source
    // using the property parser
    static member parseProperty prop value source =
        Config.property prop (Prop.parse prop value) source

    // Combines two configuration
    // Keeps value from both maps, and use the combiner when key exist
    // in both config
    static member combine x y =
        match x,y with
        | Config mx, Config my ->
            Map.fold (fun acc name (value, source, combiner) ->
                match Map.tryFind name acc with
                | Some (existingValue, _, _) ->
                    acc
                    |> Map.remove name
                    |> Map.add name (combiner existingValue value, source, combiner)
                | None ->
                    acc
                    |> Map.add name (value, source, combiner) )
                mx my
            |> Config

    // Create a configuration from a property default value.
    static member ofProperty p = 
        let d = Prop.defaultValue p
        Config.property p d "Default"

    static member get (Prop(n,_,_ : 'T,_)) (Config config) = 
        let v,_,_ = config |> Map.find n 
        unbox<'T> v

// converts a property definition to a boxed one
let boxProperty = function 
    | Prop(name, parser, defaultValue, combine) ->
        Prop(name, box << parser, box defaultValue, boxf2 combine) 

let (!@) = boxProperty
let (@@) = Config.combine


// Creates a map containing all default values
let defaultConfig properties =
    properties
    |> List.map (Config.ofProperty)
    |> List.fold Config.combine Config.empty

#r "System.Configuration"
open System.Configuration

// Read all properties from config file AppSettings
let readConfig properties =
    let tryGetProperty = fun p ->
            match ConfigurationManager.AppSettings.[string (Prop.name p)] with
            | null -> None
            | s -> Config.parseProperty p s "App.Config" |> Some
    properties
    |> List.choose(tryGetProperty)
    |> List.fold Config.combine Config.empty 




// Read properties from command line array
let parseCmdLine properties args =
    let (|Cmd|_|) (s:string) =
        if s.StartsWith("-") then
            Some (PropertyName (s.Substring 1))
        else
            None
    let props = properties |> List.map (fun p -> Prop.name p, p) |> Map.ofList
    let rec loop config = function
        | Cmd h :: tail ->
           match Map.tryFind h props with
           | Some prop -> 
                let d = Prop.defaultValue prop
                if d.GetType() = typeof<bool> then
                    let p = Config.parseProperty prop "true" "CmdLine" 
                    loop (config |> Config.combine p) tail
                else
                    match tail with
                    | v :: tail -> 

                        let p = Config.parseProperty prop v "CmdLine"
                        loop (config |> Config.combine p) tail
                    | _ -> failwithf "Command line parameter %s expects a value" (string h)
           | _ -> failwithf "Unknown command line parameter %s" (string h)
        | h :: _ when h.StartsWith("/") -> config
        | h :: _ -> 
            failwithf "Unknown command line parameter %s" h
            
        | [] -> config

    args
    |> Array.toList
    |> loop Config.empty


let print properties = function
    | Config(config) ->
        properties
        |> List.map(fun p -> 
            let n = Prop.name p
            let d,s,_ = Map.find n config
            sprintf "[%-10s] %-25s : %O " s (string n) d)
        |> String.concat "\n"

// sample configuration properties
let name = prop "name" string "Unknown" replace
let age = prop "age" int 42 replace
let maximum = prop "maximum" int 0 max

// creates a list of properties
let properties = 
    [ !@ name
      !@ age
      !@ maximum ]

fsi.AddPrinter (print properties)

// create the final config by combining each part.
// Feel free to get property parts from everywhere !
let config =
    defaultConfig properties
    @@ parseCmdLine properties [|"-name"; "jeremie"; "-maximum"; "13" |]
    @@ Config.property maximum 24 "Custom"

// print the config
config
|> print properties
|> printfn "%s"

// get the typed values 
let n = config |> Config.get name 
let m = config |> Config.get maximum
