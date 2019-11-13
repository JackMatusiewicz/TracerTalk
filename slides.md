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

    override this.ToString () =
        sprintf "%.3f, %.3f, %.3f" this.X this.Y this.Z
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
[<RequireQualifiedAccess>]
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

# UnitVector functions

```f#
[<RequireQualifiedAccess>]
module UnitVector =

    let toVector (UnitVector v) = v
```

---

# Tiny types with smart constructors

Have a type that is private with only a smart constructor to create it saves you on testing.

All you have to do is test the constructor to ensure you create objects of the right type. After that, you can
assume it's correct in the rest of your codebase.

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
type Sphere = { Center : Point; Radius : float }

[<Struct>]
type Plane = { Point : Point; Normal : UnitVector }

type Shape =
    | Sphere of Sphere
    | Plane of Plane

type SceneObject = { Shape : Shape; Shader : Colour }
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
        (minT : float) (t : float) (r : Ray)
        (n : UnitVector) (c : Colour)
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
type Scene = { Objects : SceneObject list; ViewPlane : ViewPlane }

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
            v.Material
        
    let toImage (scene : Scene) : unit =
        ViewPlane.getRays scene.ViewPlane
        |> Array2D.map (getColourForRay scene.Objects)
        |> Image.save "foo"

---

# And the result is

![](.redSphere.jpg)

---

# Problem!

We finally have something rendered, which is great.
How can we see the green sphere?

---

# Movable camera

---

class: bold

# Thanks for listening!

### Questions?
