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

object json {
  import argonaut._, Argonaut._

  object general {
    implicit def ledge[N: CodecJson, A: CodecJson]: CodecJson[LEdge[N,A]] =
      casecodec3(LEdge.apply[N,A], LEdge.unapply[N,A])("from", "to", "label")

    implicit def lnode[N: CodecJson, A: CodecJson]: CodecJson[LNode[N,A]] =
      casecodec2(LNode.apply[N,A], LNode.unapply[N,A])("vertex", "label")

    implicit def graph[N: CodecJson, A: CodecJson, B: CodecJson]: EncodeJson[Graph[N,A,B]] =
      EncodeJson((g: Graph[N,A,B]) =>
        ("edges" := g.labEdges) ->:
        ("nodes" := g.labNodes) ->:
        jEmptyObject
      )
  }

  /**
   * [
   *   {
   *     "edges" : [
   *       "qux",
   *       "foo"
   *     ],
   *     "node" : "bar"
   *   },
   *   {
   *     "edges" : [
   *       "foo"
   *     ],
   *     "node" : "qux"
   *   },
   *   {
   *     "edges" : [],
   *     "node" : "foo"
   *   }
   * ]
   */
  object bundling {
    type Bundled[N,A,B] = (LNode[N,A],Vector[LEdge[N,B]])

    private def convert[N,A,B](g: Graph[N,A,B]): Vector[Bundled[N,A,B]] =
      g.labNodes.map { case n@LNode(v,l) =>
        n -> g.outEdges(v)
      }

    private def bundledAsJson[N,A,B](implicit n: EncodeJson[LNode[N,A]], e: EncodeJson[LEdge[N,B]]): EncodeJson[Bundled[N,A,B]] =
      EncodeJson((b: Bundled[N,A,B]) =>
        ("node" := n.encode(b._1)) ->:
        ("edges" := b._2.map(e.encode(_))) ->:
        jEmptyObject
      )

    implicit def graphAsJson[N,A,B](implicit n: EncodeJson[LNode[N,A]], e: EncodeJson[LEdge[N,B]]): EncodeJson[Graph[N,A,B]] =
      EncodeJson((g: Graph[N,A,B]) =>
        convert(g).map(bundledAsJson(n,e).encode(_)).asJson
      )

  }
}