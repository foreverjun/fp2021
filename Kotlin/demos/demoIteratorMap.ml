open Kotlin_lib.Interpreter
open Stdio
open Kotlin_lib.Utils

(**
  Пример программы, реализующей класс IteratorMap, который позволяет обойти всю коллекцию (в нашем случае IntArrayList) и выдать каждый элемент с некоторым преобразованием над ним

  Данный пример должен продемонстрировать, что тут есть наследование
*)
let iterator_map_algorithm =
  {|

  open class Iterator(list: IntArrayList) {

    protected val list: IntArrayList = list
    protected var curIndex: Int = 0

    public fun hasNext(): Boolean {
      return list.get(curIndex) != null
    }

    public open fun next(): Int? {
      val cur: Int? = list.get(curIndex)
      if(cur != null) {
        curIndex = curIndex + 1
        return cur
      }
      else return null
    }

  }

  class IteratorMap(list: IntArrayList, mapf: (Int, Int) -> Int): Iterator(list) {

    private val mapf: (Int, Int) -> Int = mapf

    override fun next(): Int? {
      val cur: Int? = list.get(curIndex)
      if(cur != null) {
        curIndex = curIndex + 1
        return mapf(curIndex - 1, cur)
      }
      else return null
    }

  }

  fun main() {
    val list: IntArrayList = IntArrayList(5)
    list.map {index: Int, old: Int -> index + 1}
    val it: Iterator = Iterator(list)

    println("Initial collection")
    while(it.hasNext()) println(it.next())

    val itmap: IteratorMap = IteratorMap(list) {index: Int, old: Int -> old * old}

    println("IteratorMap-ed collection")
    while(itmap.hasNext()) println(itmap.next())
  }

|}
;;

let () =
  match parse_and_run iterator_map_algorithm with
  | Ok _ -> ()
  | Error err -> printf "Test failed: %s" (show_error err)
;;
