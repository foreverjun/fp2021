let any_class = {|
  open class Any() {

  }
|}

let array_list_cell_class =
  {|
  class ArrayListCell(value: Int) {
    public var value: Int = value
    public var next: ArrayListCell? = null
  }
|}
;;

let int_array_list_class =
  {|
  class IntArrayList(initLength: Int) {
    private var head: ArrayListCell? = null

    public fun append(value: Int): Unit {
      val cell: ArrayListCell = ArrayListCell(value)
      cell.next = this.head
      this.head = cell
    }

    init {
      var i: Int = 0
  
      while(i < initLength) {
        this.append(0)
        i = i + 1
      }
    }

    public fun get(index: Int): Int? {
      if(index < 0) return null

      var i: Int = 0
      var cur: ArrayListCell? = this.head
      while(i < index && cur != null) {
        cur = cur.next
        i = i + 1
      }
      return cur?.value
    }

    public fun set(index: Int, value: Int): Int? {
      var i: Int = 0
      var cur: ArrayListCell? = this.head
      while(i < index && cur != null) {
        cur = cur.next
        i = i + 1
      }
      if(cur == null) return null
      else {
        val oldValue: Int = cur.value
        cur.value = value
        return oldValue
      }
    }

    public fun map(f: (Int, Int) -> Int) {
      var i: Int = 0
      var cur: ArrayListCell? = this.head
      while(cur != null) {
        cur.value = f(i, cur.value)
        cur = cur.next
        i = i + 1
      }
    }
  }
|}
;;

let standard_classes = [ any_class; array_list_cell_class; int_array_list_class ]
