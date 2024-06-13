module Shapes exposing (..)

type Shape2D = Circle | Square | Triangle 

type Shape3D = Combine Shape2D Shape2D

sphere : Shape3D
sphere = Combine Circle Circle

cube : Shape3D
cube = Combine Square Square

pyramid : Shape3D
pyramid = Combine Triangle Triangle

cylinder : Shape3D
cylinder = Combine Circle Square

cone : Shape3D
cone = Combine Circle Triangle

prism : Shape3D
prism = Combine Square Triangle