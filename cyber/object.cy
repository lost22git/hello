#!/usr/bin/env cyber

use math

-- Trait
--
type IShape trait:
  func area() float

type Circle:
  with IShape
  radius float
  func area() float:
    return math.pi * radius

type Rectangle:
  with IShape
  width float
  height float
  func area() float:
    return width * height
  
-- ADT 
--
type Shape enum:
  case circle Circle
  case rectangle Rectangle

func printShape(shape Shape):
  switch shape
  case .circle -> c:
    print "Circle | radius: $(c.radius) | area: $(c.area())"
  case .rectangle -> r:
    print "Rectangle | width: $(r.width) | height: $(r.height) | area: $(r.area())"
  else:
    throw error.UnsupportedShapeToPrint

var circle = Shape.circle Circle{radius=10}
printShape circle

var rectangle = Shape.rectangle Rectangle{width=10,height=12}
printShape rectangle

