class: title

![](./logo.png)

Writing a ray tracer in F#
--------------------------

### Jack Matusiewicz

---

# Aim of the talk

1. To give you the information needed to write a simple ray tracer
1. To show you some F# code to do that. Hopefully showing you why F# was a good choice!

---

# What is a ray tracer?

A way of producing images by generating light rays, firing them into an environment and simulating the reactions.

---

# How will our basic ray tracer work

1. We'll create a 2d array of pixels.
1. For each pixel:
    11. Create a light ray that is perpendicular to the pixel, fire it into the scene
    11. Check if the pixel intersects with an object
    11. If it does, colour the pixel with the shape's colour
    11. If it does not intersect with an object, colour the pixel black

---

# Points

```f#
[<Struct>]
type Point =
    {
        X : float
        Y : float
        Z : float
    } with

    static member (-) (lhs : Point, rhs : Point) : Vector =
        { X = lhs.X - rhs.X; Y = lhs.Y - rhs.Y; Z = lhs.Z - rhs.Z }

    static member (+) (lhs : Vector, rhs : Point) : Point =
        { X = lhs.X + rhs.X; Y = lhs.Y + rhs.Y; Z = lhs.Z + rhs.Z }

    static member (+) (point : Point, vector : Vector) : Point = vector + point
```

---

# Static member operators
This is the only way to overload operators in F#

If you try to do it with modules....
```f#
module FirstModule =
    let (./.) (a : int) (b : int) = a + b

module SecondModule =
    let (./.) (a : float) (b : float) = a + b

module UsageModule =
    open SecondModule
    open FirstModule

    let foo () = 5. ./. 6.
```

You get a...

![](./ShadowingError.png)

---

# Vectors

```f#
[<Struct>]
type Vector =
    {
        X : float
        Y : float
        Z : float
    } with

    static member (+) (lhs : Vector, rhs : Vector) : Vector =
        { X = lhs.X + rhs.X; Y = lhs.Y + rhs.Y; Z = lhs.Z + rhs.Z }

    static member (-) (lhs : Vector, rhs : Vector) : Vector =
        { X = lhs.X - rhs.X; Y = lhs.Y - rhs.Y; Z = lhs.Z - rhs.Z }

    static member (*) (a : Vector, b : Vector) : Vector =
        { X = a.X * b.X; Y = a.Y * b.Y; Z = a.Z * b.Z }

    static member (.*) (a : float, b : Vector) : Vector =
        { X = a * b.X; Y = a * b.Y; Z = a * b.Z }

[<Struct>]
type UnitVector = private UnitVector of Vector
```

---

# Vector functions

```f#
module Vector =

    // Smart constructor for UnitVector
    let normalise (x : Vector) : UnitVector =
        let len = length x
        scalarDivide len x
        |> UnitVector

    // Implementing mathematical operations.
    let scalarMultiply (s : float) (x : Vector) = s .* x

    let dot (x : Vector) (y : Vector) =
        x.X * y.X + x.Y * y.Y + x.Z * y.Z

    let cross (a : Vector) (b : Vector) : Vector =
        {
            X = a.Y * b.Z - a.Z * b.Y
            Y = a.Z * b.X - a.X * b.Z
            Z = a.X * b.Y - a.Y * b.X
        }

    let scalarDivide (s : float) (x : Vector) =
        { X = x.X / s; Y = x.Y / s; Z = x.Z / s }

    let squaredLength (x : Vector) =
        x.X * x.X + x.Y * x.Y + x.Z * x.Z

    let length (x : Vector) =
        squaredLength x |> Math.Sqrt

```

---

# Tiny types with smart constructors

Have a type that is private with only a smart constructor to create it saves you on testing.

All you have to do is test the constructor to ensure you create objects of the right type. After that, you can
assume it's correct in the rest of your codebase.

---

# UnitVector functions

```f#
[<RequireQualifiedAccess>]
module UnitVector =

    let toVector (UnitVector v) = v
```

---

# Light Ray

An infinite line, defined by a point in space, o, and a direction, d.

```f#
[<Struct>]
type Ray =
    {
        Origin : Point
        Direction : UnitVector
    }

[<RequireQualifiedAccess>]
module Ray =

    let getPosition
        (t : float)
        // You can pattern match on parameters to save you
        // explicitly writing boilerplate code in your function
        ({Origin = o; Direction = UnitVector d} : Ray)
        : Point
        =
        o + t .* d
```

---

# Colour

```f#
[<Struct>]
type Colour =
    {
        R : float
        G : float
        B : float
    } with

    static member (+) (l : Colour, r : Colour) : Colour =
        { R = l.R + r.R; G = l.G + r.G; B = l.B + r.B }

    static member (*) (l : Colour, r : Colour) : Colour =
        { R = l.R * r.R; G = l.G * r.G; B = l.B * r.B }

    static member (.*) (l : float, r : Colour) : Colour =
        { R = l * r.R; G = l * r.G; B = l * r.B }
```

---

# Colour functions

```f#
[<RequireQualifiedAccess>]
module Colour =

    let private clampToOne (c : Colour) : Colour =
        let maxValue = Math.Max (c.R, Math.Max (c.G, c.B))
        if maxValue > 1. then
            {
                R = c.R / maxValue
                G = c.G / maxValue
                B = c.B / maxValue
            }
        else c

    // We could well have gone outside of the [0,1] range so we need to ensure we are back
    // in that range before we display the image.
    let toColor (c : Colour) : Color =
        let { R = r; G = g; B = b } = clampToOne c
        (r * 255., g * 255., b * 255.)
        |> (fun (r,g,b) -> Color.FromArgb (255, (int r), (int g), (int b)))
```

---

# Shapes
This ray tracer will support two types of objects, spheres and planes.

Planes can be uniquely defined by a point on the surface and the normal at that point.
Spheres can be uniquely defined by their centre point and a radius.

---

# Shapes

```f#
[<Struct>]
type Sphere =
    {
        Center : Point
        Radius : float
    }

[<Struct>]
type Plane =
    {
        Point : Point
        Normal : UnitVector
    }

type Shape =
    | Sphere of Sphere
    | Plane of Plane

type SceneObject =
    {
        Shape : Shape
        Shader : Colour
    }
```

---

# Keeping track of collisions

When we've found a collision, we need to keep track of some information that'll be used later to colour the pixels

```f#
[<Struct>]
type CollisionRecord =
    {
        /// The parameter to plug into the ray calculation
        T : float
        /// The collision point on the surface of the object
        CollisionPoint : Point
        /// The normal of the object at the collision point
        Normal : UnitVector
        /// The colour of the object
        Material : Colour
    }

module CollisionRecord =
    let private tryMake
        (minT : float)
        (t : float)
        (r : Ray)
        (n : UnitVector)
        (c : Colour)
        : CollisionRecord option
        =
        if t > minT then
            {
                T = t
                CollisionPoint = Ray.getPosition t r
                Normal = n
                Material = c
            } |> Some
        else None
```

---

# Plane collision
    Plane collision is easy to calculate, we know that a point, p, is on our plane (Point, Normal) if:
        (p - Point) . Normal = 0
    This is because if p is on the plane then the vector made by (p - a) will be perpendicular to the normal and the
    dot product will be 0.

    So, we can substitute our ray equation (o + td for any t) to get:
        (o + td - Point) . Normal
    We can rearrange this to solve for t:
    t = (Point - o) . Normal / (d . Normal)
    
    We can then substitute t into our ray equation to find the collision point

---

# Plane collision

```f#
module internal Plane =

    let rayIntersects
        (p : Plane)
        (minT : float)
        (c : Colour)
        (r : Ray)
        : CollisionRecord option
        =
        let (UnitVector planeNormal) = p.Normal
        let dDotN = (Vector.dot (UnitVector.toVector r.Direction) planeNormal)
        // If the ray is parallel to the plane, we definitely won't intersect.
        if dDotN = 0. then
            None
        else
            let t =
                // (a - o) . n / d . n
                (Vector.dot (p.Point - r.Position) planeNormal) / dDotN
            CollisionRecord.tryMake minT t r p.Normal c
```

---

# Sphere intersection

A point, p, lies on the surface of a sphere (c, r) if:
    (p - c) . (p - c) - r^2 = 0
Again, we need to substitute the ray equation, o + td
    (o + td - c) . (o + td - c) - r^2 = 0

Expanding and rearranging, we get:
    t^2 (d.d) + t (2 (o - c) . d) + (o - c) . (o - c) - r^2 = 0

This is a quadratic equation for t.

So, we can find the roots for this equation!
    -b +- sqrt (b^2 - 4ac) / 2a

So, the discriminant, b^2 - 4ac, will tell us how many real roots we have:
    < 0 means we have none,
    = 0 means we have one
    > 0 means we have two

---

# Sphere intersection

```f#
module internal Sphere =

    let rayIntersects
        (minT : float)
        (r : Ray)
        (colour : Colour)
        (s : Sphere)
        =

        let bV = UnitVector.toVector r.Direction
        let aMinusC = r.Position - s.Center

        // Calculating the terms for the quadratic equation formula
        let a = Vector.dot bV bV
        let b = 2. * Vector.dot aMinusC bV
        let c = Vector.dot aMinusC aMinusC - (s.Radius * s.Radius)
        let discriminant = b * b - 4. * a * c
        if discriminant < 0. then
            None
        else
            let firstT = (-b - Math.Sqrt discriminant) / (2. * a)
            let secondT = (-b + Math.Sqrt discriminant) / (2. * a)
            let firstNormal = (Ray.getPosition firstT r) - s.Center |> Vector.normalise
            let secondNormal = (Ray.getPosition secondT r) - s.Center |> Vector.normalise

            CollisionRecord.tryMake minT firstT r firstNormal colour
            |> Option.orElse (CollisionRecord.tryMake minT secondT r secondNormal colour)
```

---

# Putting collisions together

```f#
[<RequireQualifiedAccess>]
module Shape =

    let collides (minT : float) (r : Ray) (s : SceneObject) =
        match s.Shape with
        | Sphere sp ->
            Sphere.rayIntersects minT r s.Shader sp
        | Plane p ->
            Plane.rayIntersects p minT s.Shader r
```

# View plane

This is how we will generate our 2D array of pixels.
We can use it to generate a list of x,y coordinates for the pixels in question.
For the time being, our view plane will be parallel to the xy-plane and perpendicular to the z-axis.
The centre of our view plane will also go through the origin.

We'll view our pixels as unit squares, to keep things simple
(Take screen shot from RTFGU!)

![](./viewplane.jpg)

---

# View plane

```f#
[<Struct>]
type ViewPlane =
    {
        HorizontalResolution : int
        VerticalResolution : int
    }

[<RequireQualifiedAccess>]
module ViewPlane =

    let getXY (row : int) (col : int) (vp : ViewPlane) : float * float =
        let x = ((float col) - 0.5 * (float vp.HorizontalResolution) + 0.5)
        let y = ((float row) - 0.5 * (float vp.VerticalResolution) + 0.5)
        x,y

    let getRays (vp : ViewPlane) : Ray[,] =
        Array2D.init
            vp.VerticalResolution
            vp.HorizontalResolution
            (fun row col ->
                // Because we 0,0 in the bottom left corner of our 2D array.
                let row = vp.VerticalResolution - row - 1
                let x,y = getXY row col vp
                {
                    Position = { X = x; Y = y; Z = 0. }
                    Direction = Vector.normalise {X = 0.; Y = 0.; Z = -1.}
                }
            )
```

---

# RECAP: How will our basic ray tracer work

1. We'll create a 2d array of pixels.
1. For each pixel:
    11. Create a light ray that is perpendicular to the pixel, fire it into the scene
    11. Check if the pixel intersects with an object
    11. If it does, colour the pixel with the shape's colour
    11. If it does not intersect with an object, colour the pixel black

---

# Constructing our scene
Our first scene will just consist of some objects and a view plane

```f#
type Scene =
    {
        Objects : SceneObject list
        ViewPlane : ViewPlane
    }
module Scene =

    let rec private getColourForRay
        (shapes : SceneObject list)
        (r : Ray)
        : Colour
        =
        let collisionPoints =
            List.map (Shape.collides 0. r) shapes
            |> List.choose id
        match collisionPoints with
        | [] ->
            { R = 0.; G = 0.; B = 0. }
        | vs ->
            let v =
                List.sortBy (fun hr -> hr.T) vs
                |> List.head
            {R = 1.; G = 0.; B = 0.}
        
    let toImage (scene : Scene) : unit =
        Pinhole.getRays scene.Camera
        |> Array2D.map (getColourForRay scene.Objects)
        |> Image.save "foo.png"


class: middle, center

![](./sad-panda.jpg)

---

class: middle

.right-image[
![](./tony-hoare.jpg)
]

> "I call it my billion-dollar mistake. It was the invention of the null reference in 1965.

--

> My goal was to ensure that **all use of references should be absolutely safe**, with checking performed automatically by the compiler.

--

> But I couldn't resist the temptation to put in a null reference, simply because it was so easy to implement."
>
> -- <cite>Tony Hoare<cite>

---

# What's wrong with `null`?

`null` is a value used to represent the lack of a value. This is the problem.

--

1. subverts types .muted[(can be used for any reference type)]
1. is a special case .muted[(has to checked for everywhere)]
1. makes poor APIs .muted[(e.g. `String.IsNullOrEmpty`)]
1. difficult to debug .muted[(how did this become null?)]
1. plus many, many more

--

When your type system (e.g. Java, or C#) allows `null` everywhere, you cannot exclude the possibility of `null`.

It's inevitable it will wind up conflated somewhere.

--

**We can do better!** 💪

---

name: nulls-in-fsharp

# Nulls in F#&nbsp;

Definition of a simple *record* type in F#:

```f#
type MyLittleType = {
    Foo : string
    Bar : int
}
```

---

template: nulls-in-fsharp

The above is similar to this C# (and essentially compiles down to):

```c#
public class MyLittleType { // IEquatable, IComparable, etc.
    public MyLittleType(string foo, int bar) {
        this.Foo = foo;
        this.Bar = bar;
    }

    public readonly string Foo { get; }
    public readonly int Bar { get; }

    // GetHashCode, ToString, Equals, etc.
}
```

---

template: nulls-in-fsharp

<hr>

```f#
let baz = { Foo = "Hello world"; Bar = 5 }
```

.success[OK!]

---

template: nulls-in-fsharp

<hr>

```f#
let baz : MyLittleType = null
```

--

```c#
MyLittleType baz = null;
```

---

template: nulls-in-fsharp

<hr>

```f#
let baz : MyLittleType = null
```

.error[
`error FS0043: The type 'MyLittleType' does not have 'null' as a proper value`
]

.important[
`null` is not a proper value for F# types (not just records)
]

---

class: bold

# So how do you represent `null`?

---

# Why do we need `null`?

`null` in OO languages represents the lack of an instance.

**So why don't we encode that in the type system?**

---

name: discriminated-unions

# Discriminated unions

```f#
type Option<'T> =
    | None                  (* parameterless case *)
    | Some of value: 'T     (* case with a single generic parameter *)
```

--

(also known as _tagged unions_.)

<hr>

`Option<'T>` is part of the F# core library and represents a missing value, or a value.

--

.important[
Use an option instead of null to represent "no value".
]

---

template: discriminated-unions

<hr>

```f#
let nothing = None  (* This can satisfy any 'T in Option<'T> *)
val nothing : Option<'T>

let instance = Some { Foo = "Hello world"; Bar = 5 }
val instance : Option<MyLittleType>
```

---

template: discriminated-unions

<hr>

```f#
let instance = Some { Foo = "Hello world"; Bar = 5 }
```

--

.merged[
```f#
match instance with
| None ->
    printfn "There is nothing here 😢"
```
]
--
.merged[
```f#

| Some underlyingValue ->
    printfn "Foo is '%s'" underlyingValue.Foo
    (* ==> Foo is 'Hello world' *)
```
]

--

This is called _pattern matching_.

Can also pattern match on strings, integers, records, lists, arrays - very powerful.

---

# Exhaustive pattern matching

```f#
let printBar (foo : Option<MyLittleType>) =
    match foo with
    | Some underlyingValue ->
        printfn "Bar is '%d'" underlyingValue.Bar
```

--

.error[
    `error FS0025: Incomplete pattern matches on this expression. For example, the value 'None' may indicate a case not covered by the pattern(s).`
]

.important[
Pattern matching must handle every possible case.
]

---

class: bold

# We've hit Tony Hoare's goal! 🎉

> "My goal was to ensure that all use of references should be absolutely safe, with **checking performed automatically by the compiler**."

---

# Aside

`None` compiles down to `null` in the IL.

This is a performance optimisation under the hood.

This is fine because the **dev never has to care about nulls** - the compiler takes care of that for us.

---

# What `Option` compiles to

![](option.png)

---

# What `Option` compiles to

We can reproduce that in C#.

But we don't get the most important feature:

--

> Exhaustive pattern matching enforced by the compiler.

---

# Coming to C# 8.0

C# is moving towards 'non-nullable by default' in C# 8.0:

```c#
class Person
{
    public string First;   // Not null
    public string? Middle; // May be null
    public string Last;    // Not null
}
```

---

# Coming to C# 8.x

Also implementing records in C# 8.x:

```c#
// Syntax TBD, first draft is like this:
class Person(string First, string Last);
```

Translates to

```c#
class Person : IEquatable<Person>
{
    public string First { get; }
    public string Last { get; }

    public Person(string First, string Last);

    public void Deconstruct(out string First, out string Last);

    public bool Equals(Person other);

    public override bool Equals(object obj);
    public override int GetHashCode();
}
```

---

# Coming to C# 8.x

Also implementing pattern matching _expressions_:

```c#
class Person { /* ... */ };
class Professor : Person { /* ... */ };
class Student : Person { /* ... */ };

var someResult =
    person switch
    {
        Professor p => $"Prof. {p.First} {P.Last}";
        Student p => $"{p.First} {P.Last} (Student)";
        _ => $"Unknown";
    };
```

--

Catch-all cases (like `_ => ...`) are optional in F# pattern matching.

When a new case is added to a discriminated union, it must be handled everywhere.

---

# Nullability

Key takeaways:

.important[
`null` is not a proper value for F# types (not just records)
]

.important[
Use an option instead of null to represent "no value".
]

.important[
Pattern matching must handle every possible case.
]

---

template: agenda
class: agenda-3

---

# (Mis)using built-in types

Built-in types, like ints and strings, are often used to represent values that must fit some constraints.

These constraints are not apparent from the type system.

---

# (Mis)using built-in types

For example:

```c#
public bool SendEmail(string from, string to, string body)
```

--

`string` can represent an arbitrary sized sequence of UTF-16 code points.

**There are definitely some strings which are not valid email addresses.**

--

The example above is _stringly typed_.

.important[
Use the type system to represent different types of values
]

---

# Single-case discriminated unions

A solution to the _stringly typed_ problem is to use _tiny types_ (aka single-case discriminated unions).

```f#
type EmailAddress = EmailAddress of string
```

This is another instance of a _discriminated union_ introduced earlier, but with just a **single case**.

--

```f#
module EmailAddress =
    let tryParse (email : string) =
        (* Yes, this is quite weak! *)
        if Regex.IsMatch(email, @"[a-z]+@[a-z]+.[a-z]+") then
            Some (EmailAddress email)
        else
            None
```

---

# Single-case discriminated unions

```f#
type EmailAddress = EmailAddress of string
```

<hr>

```f#
let myEmail = EmailAddress.tryParse "saul.rennison@gresearch.co.uk"

match myEmail with
| None -> printfn "Could not parse"
| Some email ->
    let emailParts = email.Split '@'
    (* ... *)
```

--

.error[
    `error FS0039: The field, constructor or member 'Split' is not defined.`
]

--

.important[
There are no implicit conversions. We can't implicitly access the underlying string.
]

The only methods/functions available for `EmailAddress` are ones we write.

---

# Hiding the constructor

There's nothing to stop someone doing this:

```f#
let userEmail = readInput()
let myEmail = EmailAddress userEmail
```

--

👎 This hasn't gone through `tryParse` - what if the input is invalid?

We'll probably find out further down the line - difficult to debug.

---

# Hiding the constructor

```f#
(* In EmailAddress.fsi *)
type EmailAddress
```

<hr>

Hiding the implementation of the type forces you to put your domain logic in the same place:

```f#
type EmailAddress = EmailAddress of string

module EmailAddress =
    let tryParse (email : string) = (* ... *)
    let localPart (EmailAddress email) =
        email.Split('@').[0]
```

.important[
The only way to construct or act on an `EmailAddress` is via the functions on its module.

It is an **opaque type**.
]

---

# Hiding the pattern

```f#
(* In EmailAddress.fsi *)
type EmailAddress
```

<hr>

In some other file:

```f#
let isGmailAddress (email : EmailAddress) =
    match email with
    | EmailAddress underlyingString -> (* error FS0039 *)
        underlyingString.EndsWith "@gmail.com"
```

.error[
`error FS0039: The pattern discriminator 'EmailAddress' is not defined.`
]

--

TomD did a great talk on domain modelling last Thursday.

---

# Single-case discriminated unions

```c#
public bool SendEmail(string from, string to, string body)
```

Can now be re-written as:

```f#
let sendEmail
    (from: EmailAddress)
    (to: EmailAddress)
    (body: string)
    : bool
```

This is more **strongly typed**.

It is impossible for us to pass an invalid email address to `sendEmail`.

---

# Single-case discriminated unions

Key takeaways:

.important[
Use the type system to represent different types of values
]

.important[
No implicit conversions
]

.important[
Opaque types allow us to hide the underlying value (e.g a string)
]

---

template: agenda
class: agenda-4

---

# What the GADT?

Typically used to represent _domain-specific languages_ (DSLs).

We want to make invalid expressions _compile-time failures_.

--

<hr>

Imagine a basic arithmetic language which can represent:

- Addition
- Integer constants
- Comparing to zero
- If expressions

---

# Basic Example

```f#
type Expr =
    | Const of int
    | Add of Expr * Expr
    | IsZero of Expr
    | If of Expr * Expr * Expr
```

--

☑️ Allows us to express:

```f#
let valid = Add (Const 1) (Const 1)
```

--

👎 Also allows:

```f#
let addBoolToInt = Add (IsZero (Const 1)) (Const 1)
```
--
```f#
let ifOnInt = If (Const 1) (Const 2) (Const 3)
```
--
```f#
let differentThenElse = If (Const 1) (IsZero (Const 2)) (Const 3)
```

These don't make any sense!

---

# What we're missing

We couldn't differentiate between expressions of different types.

--

Can generics help us?

```f#
type Expr<'T> =
    | Const of int
    | Add of Expr<int> * Expr<int>
    | IsZero of Expr<int>
    | If of Expr<bool> * Expr<'T> * Expr<'T>
```

--

👎 But we can still do:

```f#
let bad : Expr<bool> = Const 1
```

`Const 1` is an `int` expression, not a `bool` expression.

---

# The problem with generics

Each case constructor is essentially a function:

```f#
val Const : int -> Expr<'T>
val Add : Expr<int> -> Expr<int> -> Expr<'T>
val IsZero : Expr<int> -> Expr<'T>
val If : Expr<bool> -> Expr<'T> -> Expr<'T> -> Expr<'T>
```

Only the `If` constructor has the return type we want.

---

# In an ideal world

```f#
type Expr<'T> =
    | Const of int when 'T: int
    | Add of Expr<int> * Expr<int> when 'T: int
    | IsZero of Expr<int> when 'T: bool
    | If of Expr<bool> * Expr<'T> * Expr<'T>
```

--

.important[
This syntax doesn't exist in .NET
]

---

class: center, middle

![](./sad-panda.jpg)

---

# The workaround

In the ideal world, we used equations to constrain `'T` to a particular type.

To encode this in F#, we need a _type equality_ (called a _Teq_ for short).

--

<hr>

`Teq<'T,'U>` has the following properties:

1. Can only construct `Teq<'T,'U>` if `'T` and `'U` are the same type

2. When we have a `Teq<'T,'U>`, we can treat `'T` as `'U` and vice versa.

.important[
Teq acts as evidence that two types are equal.
]

---

# Teq module

```f#
type Teq<'T, 'U> (* Opaque type *)

module Teq =
    val refl<'T> : Teq<'T,'T>
    val cast<'T,'U> : Teq<'T,'U> -> 'T -> 'U
```

We have used an **opaque type** again to expose only our constructor.

--

<hr>

We can only instantiate a `Teq<'T, 'U>` through `Teq.refl`.<br>
(refl is short for reflexive)

The type signature of `Teq.refl` ensures that `'T` is the same as `'U`

--

<hr>

`Teq.cast` allows us to cast a `'T` to a `'U`.

This allows us to treat `'T` as `'U` and vice versa.

---

class: middle, center

![](what.jpg)

---

# Using Teqs to achieve type safety

```f#
type Expr<'T> =
    | Const of Teq<int, 'T> * int
    | Add of Teq<int, 'T> * Expr<int> * Expr<int>
    | IsZero of Teq<bool, 'T> * Expr<int>
    | If of Expr<bool> * Expr<'T> * Expr<'T>
```

--

Let's try our previous example again:

```f#
let bad : Expr<bool> = Const (Teq.refl, 1)
```

--

🎉️ This won't compile!

.error[
```
error FS0001: Type mismatch. Expecting a
    'Teq<int,bool>'
but given a
    'Teq<int,int>'
The type 'bool' does not match the type 'int'
```
]

---

# Using Teqs

Let's write an interpreter for `Expr`:

```f#
let rec eval<'T> (e: Expr<'T>) : 'T =
    match e with
    | Const (teq, value) ->
        value (* Error! *)
```

The error here is because `value` is `int`, but our return type is `'T`.

How can we tell the compiler that we _know_ `'T` and `int` are the same type?

--

<hr>

Remember our Teq! We have `teq` of type `Teq<int, 'T>`.

We can use `Teq.cast teq value` to cast `value` to `'T`.

The compiler is now 😃. Completely type safe!

---

# The whole hog

.merged[
```f#
let rec eval<'T> (e: Expr<'T>) : 'T =
    match e with
    | Const (teq, value) ->
        Teq.cast teq value
```
]
--
.merged[
```f#

    | Add (teq, left, right) ->
        let result = eval left + eval right
        Teq.cast teq result
```
]
--
.merged[
```f#

    | IsZero (teq, expr) ->
        let result = eval expr = 0
        Teq.cast teq result
```
]
--
.merged[
```f#

    | If (condition, thenExpr, elseExpr) ->
        if eval condition then 
            eval thenExpr
        else
            eval elseExpr
```
]

---

# Implementing Teq

So I haven't mentioned how this magical `Teq` is implemented.

It's not as complex as you're imagining 🙏

--

```f#
type Teq<'T, 'U> = Refl of ('T -> 'U) * ('U -> 'T)
```

--

.merged[
```f#
module Teq =
    let refl<'T> : Teq<'T,'T> = Refl (id, id)
```
]
--
.merged[
```f#

    let cast<'T,'U>
        (teq : Teq<'T, 'U>)
        (value : 'T)
        : 'U =

        match teq with
        | Refl (tToU, uToT) ->
            tToU value
```
]

--

<hr>

SimonC will talk more about Teqs tomorrow 😃

---

# GADTs

Key takeaways:

.important[
Use Teqs for type safe GADTs (this is their only use case)
]

.important[
Teq acts as evidence that two types are equal.
]

See Simon's talk tomorrow for real world examples.

---

template: agenda
class: agenda-5

---

# Example: writing a parser

.merged[
```
parseList
    "[ 1 ; 2 ; 3 ]"
```
]
--
.merged[
```
        = [ 1 ; 2 ; 3 ]
```
]
--
.merged[
```

parseList
    "[ true ; false ]"
```
]
--
.merged[
```
        = [ true ; false ]
```
]

---

# Example: writing a parser

.merged[
```f#
parseList "[ 1 ; 2 ; 3 ]" = [ 1 ; 2 ; 3 ]
parseList "[ true ; false ]" = [ true ; false ]

val parseList : string -> ?
```
]
--
.merged[
```f#
val parseList : string -> obj 😭
```
]
--
.merged[
```f#
val parseList : string -> List<obj> 😠
```
]
--
.merged[
```f#
val parseList : string -> ∃ 'a . List<'a> 🤪
```
]

--

"There exists some type `'a`, for which I am a list of `'a`"

.important[
Existentials allow us to express at the type level that we do not know `'a` statically.
]

---

# Universal Quantification

```f#
Ɐ 'a . (List<'a> -> int)
```

--

```f#
module List =

    val length : List<'a> -> int
```

---

# Universal Quantification vs. Generics

```f#
let sumLengths
    (xs : List<int>)
    (ys : List<string>)
    (getLength : ??)
    : int =

    getLength xs + getLength ys
```

--

We want the type of `getLength` to be `List<anything> -> int`.

--

Again - this isn't possible in .NET 😪

---

class: bold

# Polymorphism in .NET

☑️ Generics

--

❌ First-class universals

---

name: emulating-universal-quantification

# Emulating Universal Quantification

```f#
type IGetListLength =
    abstract member Invoke<'a> : List<'a> -> int
```

--

Equivalent to:

```c#
interface IGetListLength
{
    int Invoke<'a>(List<'a> list);
}
```

---

template: emulating-universal-quantification

<hr>

```f#

let sumLengths
    (xs : List<int>)
    (ys : List<string>)
    (getLength : IGetListLength)
    : int =

    getLength.Invoke xs + getLength.Invoke ys
```

---

class: bold

# ∃ ⇄ Ɐ

---

class: bold

# We want to emulate:

### `∃ 'a . List<'a>`

---

# Continuation Passing Style

```
             'a ≅ Ɐ 'ret . (('a  -> 'ret) -> 'ret)
```
--
```
            int ≅ Ɐ 'ret . ((int -> 'ret) -> 'ret)
```
--
```f#
type ContinuationPassingStyleInt =
    abstract member Eval<'ret> : (int -> 'ret) -> 'ret
```

Equivalent to:

```c#
interface ContinuationPassingStyleInt
{
    TRet Eval<TRet>(Func<int, TRet> f);
}
```

???

We can represent a type as a universally quantified function which takes in something that acts on the type and returns you the value.

Bit weird.

Let's look at a more concrete example.

The way that I can represent an integer is by a universally quantified function that takes in whatever you want and returns that 'whatever you want' type.

You hand me the function, and I pass in the value of the int that I represent.

So if I'm 5, I call your function with 5 and then hand you back the result you calculated in that function.

So these two ways of representing the type are absolutely equivalent.

So we're going to use this in our definition of an existential.

Because we know how to represent universal quantification in F#, we can actually write that type.

This is everything that we need to represent an existential.

---

# Implementing our existential

```
             'a ≅ Ɐ 'ret . (('a  -> 'ret) -> 'ret)
```

---

# Implementing our existential

<div>
<pre><div class="remark-code"><div class="remark-code-line">             <span style="background: #ff7979">'a</span> ≅ Ɐ 'ret . ((<span style="background: #ff7979">'a</span>  -> 'ret) -> 'ret)
</div></div><pre>
</div>

<div class="merged">
<pre><div class="remark-code"><div class="remark-code-line"><span style="background: #ff7979">∃ 'a . List<'a></span> ≅ Ɐ 'ret . ((<span style="background: #ff7979">∃ 'a . List<'a></span>  -> 'ret) -> 'ret)
</div></div><pre>
</div>

--
.merged[
```
                ≅ Ɐ 'ret . ( Ɐ 'a . (List<'a> -> 'ret) -> 'ret)
```
]

???

Here's our formula for CPS. 

I'm going to substitute in the type we want to emulate. I've replaced 'a on the left and the right with `∃ 'a . List<'a>`.

This next line is interesting and needs a little bit of explanation.

If you have a look inside of this function, what I've got is a function that takes in an existentially quantified list and returns a 'ret.

Have a think about that.

What types of list does that function take?

Given that it's taken an existentially quantified list, which could be any list, we can represent that as a universally quantified function that works over all lists.

This is the duality of universal and existential quantification.

What we've done here is pull the existential quantifier out of the left-hand side of the function signature and in doing so it becomes a universal quantifier.

And how were we able to get the existential on the left-hand side of a function signature? And that's because we used the CPS.

We're able to use these things together to write our type now as two universally quantified functions.

---

# Implementing our existential

```
             'a ≅ Ɐ 'ret . (('a  -> 'ret) -> 'ret)
```

.merged[
```
∃ 'a . List<'a> ≅ Ɐ 'ret . ((∃ 'a . List<'a>  -> 'ret) -> 'ret)
```
]

<div class="merged">
<pre><div class="remark-code"><div class="remark-code-line">                ≅ Ɐ 'ret . (<span style="background: #badc58"> Ɐ 'a . (<span style="background: #7ed6df">List<'a> -> 'ret</span>) -> 'ret</span>)
</div></div><pre>
</div>

```f#
type ListCrateEvaluator<'ret> =
    abstract member Eval<'a> : List<'a> -> 'ret

type ListCrate =
    abstract member Apply<'ret> : ListCrateEvaluator<'ret> -> 'ret
```

---

# Using Crates

```f#
let makeListCrate (list : List<'a>) : ListCrate =
    { new ListCrate with 
        member this.Apply e = e.Eval list
    }
```

--

.important[
We're hiding the generic type. The return value is not generic.
]

--

Equivalent to:

```c#
class AutoGeneratedNameHere<T> : ListCrate {
    private List<T> _list;
    public AutoGeneratedNameHere(List<T> list) { _list = list; }

    public override TRet Apply<TRet>(ListCrateEvaluator<TRet> e) {
        return e.Eval(_list);
    }
}
```

---

# Using Crates

```f#
let getLength (listCrate : ListCrate) : int =
    listCrate.Apply
        { new ListCrateEvaluator<int> with
            member this.Eval (list : List<'a>) = List.length list
        }
```

.important[
In our evaluator, we are in a strongly typed context.
]

--

Equivalent to:

```c#
class AutoGeneratedNameHere : ListCrateEvaluator<int> {
    public AutoGeneratedNameHere() {};
    public int ListCrateEvaluator<int>.Eval<T>(List<T> list) {
        return List.length(list);
    }
}
```

---

# Existential Types

Key takeaways:

.important[
Existentials allow us to express at the type level that we do not know `'a` statically.
]

.important[
Crates hide the generic type.
]

.important[
In our evaluator, we are in a strongly typed context.
]

---

template: agenda
class: agenda-6

---

# Background: F# lists

```f#
type List<'T> =
    | Empty
    | Cons of Head: 'T * Tail: List<'T>
```

--

<pre>
<span style="background-color: #ff7979">Cons(42, <span style="background-color: #badc58">Cons (69, <span style="background-color: #7ed6df">Cons (613, Empty)</span>)</span>)</span>
</pre>

.half-width[
![](./cons.png)
]

--

👎 This syntax is pretty unwieldy.

---

# Background: F# lists

```f#
type List<'T> =
    | ([])
    | (::) of Head: 'T * Tail: List<'T>
```

F# actually uses *in-fix* notation.

--

```f#
Cons(42, Cons(69, Cons(613, Empty)))
```

Becomes:

```f#
42::69::613::[]
```

--

There is a syntax sugar which is identical to the above:

```f#
[42; 69; 613]
```

---

# HLists

Imagine a standard list:

```f#
let standardList = [1; 2; 3; 4]
```

--

Contrast this with a HList - _heterogenous_ list.

```f#
let myHList = [1234; true; "Hello"]
```

P.S. This is not valid F#!

--

<hr>

How do we represent a HList in the type system?

--

.merged[
```f#
val myHList : obj 😭
```
]
--
.merged[
```f#
val myHList : List<obj> 😠
```
]
--
.merged[
```f#
val myHList : HList<int * (bool * (string * unit))> 🤪
```
]
--
.merged[
```f#
val myHList : HList<int -> bool -> string -> unit> 😅
```
]

???

Nick's HList example: https://pastebin.com/a4BJrA64

---

name: hlists

# HLists

The types of the elements are represented in the type of the HList.

```f#
let myList =
    HList.empty
    |> HList.cons "Hello"
    |> HList.cons true
    |> HList.cons 1234

val myList : HList<int -> bool -> string -> unit>
```

---

template: hlists

<hr>

```f#
example |> HList.tail |> HList.tail |> HList.tail |> HList.tail
```

.error[
<pre>
error FS0001: Type mismatch. Expecting a
    'unit HList -> 'a'
but given a
    '('head -> 'tail) HList -> 'tail HList'
The type 'unit' does not match the type ''head -> 'tail'
</pre>
]

---

template: hlists

We use functions as they are syntactically _convenient_.

```f#
val myList :
    HList<
        FSharpFunc<int,
            FSharpFunc<bool,
                FSharpFunc<string, unit>>>>
```

This is a similar structure to the `Cons(..., Cons(..., Empty))` from before.

---

template: hlists

.important[
The elements of HList are represented in the generic type argument.
]

---

name: what-is-hlist

# What is `HList`?

.merged[
```f#
type HList<'a> =
    | Empty of Teq<'a, unit>
    | Cons of 'a HListConsCrate
```
]
--
.merged[
```f#

and HListConsCrate<'hlist> =
    abstract member Apply : HListConsEvaluator<'hlist,'ret> -> 'ret

and HListConsEvaluator<'hlist, 'ret> =
    abstract member Eval : 'head
                        -> 'tail HList
                        -> Teq<'hlist, 'head -> 'tail>
                        -> 'ret
```
]

--

Contrast this to a standard F# list:

```f#
type List<'T> =
    | Empty
    | Cons of Head: 'T * Tail: List<'T>
```

---

# Constructing a HList

.merged[
```f#
module HList =

    let empty : HList<unit> = Empty Teq.refl
```
]
--
.merged[
```f#

    let cons
        (element : 'head)
        (list : HList<'tail>)
        : HList<'head -> 'tail>
        =
```
]
--
.merged[
```f#

        Cons { new HListConsCrate<'head -> 'tail> with
            member this.Apply e =
                e.Eval element list Teq.refl<'head -> 'tail>
        }
```
]

---

# More Teq functions

We're going to have to introduce more Teq functions for deconstruction.

P.S. the implementation is unimportant and trivial.

--

```f#
module Teq =
    val castFrom : Teq<'a,'b> : 'b -> 'a

    val domain : Teq<'a -> 'b, 'c -> 'd> -> Teq<'a, 'c>
```

---

# Deconstructing a HList

.merged[
```f#
let head<'head, 'tail> (list : HList<'head -> 'tail>)) : 'head =
    match list with
    | Empty teq -> failwith "Impossible"
```
]
--
.merged[
```f#
    | Cons consCrate ->
        consCrate.Apply
            { new HListConsEvaluator<'head -> 'tail,_> with
```
]
--
.merged[
```f#
                member this.Eval<'headX, 'tailX>
                    (element : 'headX)
                    (rest : HList<'tailX>)
                    (teq : Teq<'head->'tail, 'headX->'tailX>) =
```
]
--
.merged[
```f#

                    let domainTeq = Teq.domain teq
                    (* Teq<'head, 'headX> *)
```
]
--
.merged[
```f#

                    Teq.castFrom domainTeq element
                    (* 'head *)
            }
```
]

--

.important[
Deconstruction is completely type safe.
]

---

# Working with HLists

.merged[
```f#
module HList =

    let rec length (list : HList<'a>) : int =
        match list with
        | Empty teq -> 0
        | Cons consCrate ->
            consCrate.Apply { new HListConsEvaluator<_,_> with
                member this.Eval element rest teq =
                    1 + length rest
            }
```
]

---

# HLists

Key takeaways:

.important[
The elements of HList are represented in the generic type argument.
]

.important[
Deconstruction is completely type safe.
]

---

template: agenda
class: agenda-7

---

# Further Reading/Viewing

- **The future of C# | Microsoft Build 2018**<br>
    https://channel9.msdn.com/Events/Build/2018/BRK2155

- **DogeConf 2018: Domain Modelling** - _Tom Davies_<br>
    Video should be available soon

- **Type-safe domain-specific languages in F#** - _Remco Bras_<br>
    Waiting to be published on the GR blog

- **Existentials: Playing Hide and Seek With Your Types** - _Nicholas Cowle_<br>
    https://skillsmatter.com/skillscasts/11633-lightning-talk-existentials-playing-hide-and-seek-with-your-types

- **Complete HList Example** - _Nicholas Cowle_<br>
    https://pastebin.com/a4BJrA64

---

class: bold

# Thanks for listening 👍

### Questions?
