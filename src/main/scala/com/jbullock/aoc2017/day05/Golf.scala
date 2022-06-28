package com.jbullock.aoc2017.day05;class S(n:Int,p:Int,i:Vector[Int]){final def j(r:Int=>Int):Int=if i.isDefinedAt(p)
then S(n+1,p+i(p),i.updated(p,r(i(p)))).j(r) else n};@main def g=List(S(0,0,io.Source.fromResource("g/17-05")
  .getLines.toVector.map(_.toInt))).flatMap(x=>List(x.j(_+1),x.j(x=>if x>=3 then x-1 else x+1))).foreach(println)
