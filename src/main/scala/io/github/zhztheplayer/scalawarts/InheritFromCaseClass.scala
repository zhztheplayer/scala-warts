/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.github.zhztheplayer.scalawarts

import org.wartremover.{WartTraverser, WartUniverse}

/**
 * Forbid a class from inheriting from a case class.
 */
object InheritFromCaseClass extends WartTraverser {
  override def apply(u: WartUniverse): u.Traverser = {
    import u.universe._
    new u.Traverser {
      override def traverse(tree: u.universe.Tree): Unit = {
        tree match {
          // Ignore trees marked by SuppressWarnings
          case t if hasWartAnnotation(u)(t) =>
          case d @ ClassDef(mods, _, _, _) =>
            val c = d.symbol.asClass
            val caseBases = c.baseClasses
              .filter(_.isClass)
              .filter(base => base != c)
              .filter(base => base.asClass.isCaseClass)
            if (caseBases.nonEmpty) {
              error(u)(
                tree.pos,
                s"case class is not inheritable: ${c.name} is trying to inherit from $caseBases")
            }
          case t => super.traverse(tree)
        }
      }
    }
  }
}
