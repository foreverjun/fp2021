(*
  Пример программы, реализующей алгоритм поиска в ширину над графом с матрицей смежности
  [
    [0, 1, 1, 0],
    [1, 0, 0, 1],
    [1, 0, 0, 1],
    [0, 1, 1, 0]
  ]
  Функция init_matrix "захардкожена" чтобы возвращать данную матриу смежности

  Данный пример должен продемонстрировать, что тут есть какое никакое ООП и анонимные функции
*)
let bfs_algorithm =
  {|

  class IntQuery() {
    private val list: IntArrayList = IntArrayList(0)
    private var lastIndex: Int = -1

    public fun push(value: Int) {
      this.lastIndex = this.lastIndex + 1
      this.list.append(value)
    }

    public fun pop(): Int? {
      this.lastIndex = this.lastIndex - 1
      return this.list.get(this.lastIndex + 1)
    }

    public fun isEmpty(): Boolean {
      return this.lastIndex < 0
    }
  }

  class IntMatrix(n: Int, m: Int) {
    private val n: Int = n
    private val m: Int = m
    private val list: IntArrayList = IntArrayList(n * m)

    public fun get(i: Int, j: Int): Int? {
      if(j < this.n && j >= 0) return this.list.get(i * this.m + j)
      else return null
    }

    public fun set(i: Int, j: Int, value: Int): Int? {
      val oldValue: Int? = this.list.get(i * this.m + j)
      if(oldValue == null) return null
      else {
        this.list.set(i * this.m + j, value)
        return oldValue
      }
    }
  }

  fun init_matrix(): IntMatrix {
    val mat: IntMatrix = IntMatrix(4, 4)

    mat.set(0, 0, 0)
    mat.set(0, 1, 1)
    mat.set(0, 2, 1)
    mat.set(0, 3, 0)
    mat.set(1, 0, 1)
    mat.set(1, 1, 0)
    mat.set(1, 2, 0)
    mat.set(1, 3, 1)
    mat.set(2, 0, 1)
    mat.set(2, 1, 0)
    mat.set(2, 2, 0)
    mat.set(2, 3, 1)
    mat.set(3, 0, 0)
    mat.set(3, 1, 1)
    mat.set(3, 2, 1)
    mat.set(3, 3, 0)

    return mat
  }

  fun checkInList(list: IntArrayList, value: Int): Boolean {
    var i: Int = 0
    var cur: Int? = list.get(i)
    while(cur != null) {
      if(cur == value) return true
      else {
        i = i + 1
        cur = list.get(i)
      }
    }
    return false
  }

  fun main() {
    val infinity: Int = 1000
    val mat: IntMatrix = init_matrix()
    val query: IntQuery = IntQuery()
    val visited: IntArrayList = IntArrayList(0)
    val distances: IntArrayList = IntArrayList(4)
    distances.map({index: Int, old: Int -> 
      infinity
    })

    query.push(0)
    distances.set(0, 0)
    while(!query.isEmpty()) {
      val cur: Int? = query.pop()
      
      if(cur != null && !checkInList(visited, cur)) {
        distances.map({index: Int, old: Int -> 
          val weight: Int? = mat.get(cur, index)
          val curDistance: Int? = distances.get(cur)
          if(weight != null && curDistance != null && weight != 0 && weight + curDistance < old) {
            query.push(index)
            weight + curDistance
          }
          else old
        })
        visited.append(cur)
      }
    }

    println("answer")
    distances.map({index: Int, old: Int ->
      println(old)
      old
    })
  }

|}
;;

(*
  Пример программы, реализующей класс IteratorMap, который позволяет обойти всю коллекцию (в нашем случае IntArrayList) и выдать каждый элемент с некоторым преобразованием над ним

  Данный пример должен продемонстрировать, что тут есть наследование
*)
let iterator_map_algorithm =
  {|

  open class Iterator(list: IntArrayList) {

    protected val list: IntArrayList = list
    protected var curIndex: Int = 0

    public fun hasNext(): Boolean {
      return this.list.get(this.curIndex) != null
    }

    public open fun next(): Int? {
      val cur: Int? = this.list.get(this.curIndex)
      if(cur != null) {
        this.curIndex = this.curIndex + 1
        return cur
      }
      else return null
    }

  }

  class IteratorMap(mapf: (Int, Int) -> Int, list: IntArrayList): Iterator(list) {

    private val mapf: (Int, Int) -> Int = mapf

    override fun next(): Int? {
      val cur: Int? = this.list.get(this.curIndex)
      if(cur != null) {
        this.curIndex = this.curIndex + 1
        return this.mapf(this.curIndex - 1, cur)
      }
      else return null
    }

  }

  fun main() {
    val list: IntArrayList = IntArrayList(5)
    list.map({index: Int, old: Int -> index + 1})
    val it: Iterator = Iterator(list)

    println("Initial collection")
    while(it.hasNext()) println(it.next())

    val itmap: IteratorMap = IteratorMap({index: Int, old: Int -> old * old}, list)

    println("IteratorMap-ed collection")
    while(itmap.hasNext()) println(itmap.next())
  }

|}
;;

(*
  Пример программы с рекурсивным факториалом
*)
let factorial_algorithm =
  {|

  fun fact(n: Int): Int {
    if(n > 1) return n * fact(n - 1)
    else return 1
  }

  fun main() {
    println(fact(5))
  }

|}
;;
