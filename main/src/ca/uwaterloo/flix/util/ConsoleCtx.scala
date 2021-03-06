/*
 * Copyright 2015-2016 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.util

trait ConsoleCtx {

  def blue(s: String): String

  def green(s: String): String

  def red(s: String): String

}

class WindowsConsole extends ConsoleCtx {
  def blue(s: String): String = s

  def green(s: String): String = s

  def red(s: String): String = s
}

class AnsiConsole extends ConsoleCtx {
  def blue(s: String): String = Console.BLUE + s + Console.RESET

  def green(s: String): String = Console.GREEN + s + Console.RESET

  def red(s: String): String = Console.RED + s + Console.RESET

  def cyan(s: String): String = Console.CYAN + s + Console.RESET
}
