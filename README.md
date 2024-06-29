# Verity Solver

This simple web app is used as a helper tool for the outside team in fourth encounter in the Salvations Edge raid in Destiny 2. A brief explantion as to how it works:

We have three 2D shapes:
* Circle
* Square
* Triangle

When we combine two 2D shapes, we end up with a 3D shape:
* Circle + Circle = Sphere
* Square + Sqaure = Cube
* Triangle + Triangle = Pyramid
* Circle + Square = Cylinder
* Circle + Triangle = Cone
* Square + Triangle = Prism

For this encounter, there is room in the beginning with three statues each holding a 3D shape. Three out of the six players will get teleported into a similar room by themselves where the statues are now hoding 2D shapes while the remaining three players stay in the original room with the 3D shapes. To complete this puzzle, the outside statues need to be holding a 3D shape that does not contain the 2D shape that the statue is holding in the inside.

For example, is the left statue is holding a Triangle in the inside rooms, then that means the left outside statue needs to be holding a Cylinder (Circle + Square), and if the middle statue is holding a Circle in the inside room, then the outside statue needs to be holding a Prism (Triangle + Square), and whatever inside statue is holding a Square needs its outside statue to be holding a Cone (Circle + Triangle).

In order for the three players in the outside room to change the 3D shapes that the stautes are holding, they need to "dissect" a 2D shape off of a 3D shape for two statues. This essentialy "swaps" two 2D shapoes from eachother between statues. For example, if I dissect a Square off a statue holding a Cube and dissect a Circle off of a statue holding a Cone, the new 3D shapes would be a Cylinder and a Prism. 

This application aims to help the outside team solve the outside statues in the least amount of swaps possible by simply inputting the starting inside and outside shapes at the beginning of each round of the encounter, you can it in action at [nokynokes.github.io/veritySolver/](https://nokynokes.github.io/veritySolver/).

## Installation
1. Make sure you have [Elm 0.19.1](https://guide.elm-lang.org/install/elm.html) installed 
2. Clone the repo
   ```sh
   git clone https://github.com/nokynokes/veritySolver.git
   ```
3. Run ```elm-reactor``` to start a development server
4. Open up [http://localhost:8000](http://localhost:8000) and navigate to `src/Main.elm`
