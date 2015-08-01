//: ----------------------------------------------------------------------------
//: Copyright (C) 2015 Verizon.  All Rights Reserved.
//:
//:   Licensed under the Apache License, Version 2.0 (the "License");
//:   you may not use this file except in compliance with the License.
//:   You may obtain a copy of the License at
//:
//:       http://www.apache.org/licenses/LICENSE-2.0
//:
//:   Unless required by applicable law or agreed to in writing, software
//:   distributed under the License is distributed on an "AS IS" BASIS,
//:   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//:   See the License for the specific language governing permissions and
//:   limitations under the License.
//:
//: ----------------------------------------------------------------------------
package quiver

object Test extends App {
  import json.bundling._
  import argonaut._, Argonaut._

  val one = LNode[String,Unit]("bar", ())
  val two = LNode[String,Unit]("qux", ())
  val three = LNode[String,Unit]("foo", ())

  val nodes: Seq[LNode[String,Unit]] = Seq(one,two,three)

  val edges: Seq[LEdge[String,String]] = Seq(
    LEdge(one.vertex,two.vertex,"get-qux"),
    LEdge(one.vertex,three.vertex,"get-foo"),
    LEdge(two.vertex,three.vertex,"get-foo")
  )

  implicit val ledge: EncodeJson[LEdge[String,String]] =
    EncodeJson((e: LEdge[String,String]) => e.to.asJson)

  implicit val lnode: EncodeJson[LNode[String,Unit]] =
    EncodeJson((n: LNode[String,Unit]) => n.vertex.asJson)

  val temp: Graph[String,Unit,String] = mkGraph(nodes,edges)

  println(temp.asJson.spaces2)

}
