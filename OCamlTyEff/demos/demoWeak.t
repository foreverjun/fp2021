  $ cat << EOF | ./demo.exe -
  > let r = ref []
  > ;;
  > EOF
  val r : '_weak_t0 list ref = {contents = []}

  $ cat << EOF | ./demo.exe -
  > let r = ref []
  > let o = r := ( [], [] ) :: []
  > ;;
  > EOF
  val r : ('_weak_t0 list * '_weak_t1 list) list ref = {contents = [([], [])]}
  val o : unit = ()

  $ cat << EOF | ./demo.exe -
  > let r = ref []
  > let o = r := ( ( 1 :: [], [] ) ) :: []
  > ;;
  > EOF
  val r : (int list * '_weak_t0 list) list ref = {contents = [([1], [])]}
  val o : unit = ()

  $ cat << EOF | ./demo.exe -
  > let r = ref []
  > let o = r := ( [], [] ) :: []
  > let o = r := ( ( 1 :: [], [] ) ) :: []
  > ;;
  > EOF
  val r : (int list * '_weak_t0 list) list ref = {contents = [([1], [])]}
  val o : unit = ()

  $ cat << EOF | ./demo.exe -
  > let fake_id =
  >   let r = ref [] in
  >   fun x ->
  >     match !r with
  >     | [] ->
  >       let o = r := x :: [] in
  >       x
  >     | hd :: tl -> hd
  > ;;
  > EOF
  val fake_id : ('_weak_t0 -[Asgmt]-> '_weak_t0) = <fun>

  $ cat << EOF | ./demo.exe -
  > let fake_id =
  >   let r = ref [] in
  >   fun x ->
  >     match !r with
  >     | [] ->
  >       let o = r := x :: [] in
  >       x
  >     | hd :: tl -> hd
  > ;;
  > let x = fake_id 1
  > ;;
  > EOF
  val fake_id : (int -[Asgmt]-> int) = <fun>
  val x : int = 1

  $ cat << EOF | ./demo.exe -
  > let fake_id =
  >   let r = ref [] in
  >   fun x ->
  >     match !r with
  >     | [] ->
  >       let o = r := x :: [] in
  >       x
  >     | hd :: tl -> hd
  > ;;
  > let x = fake_id 1
  > let y = fake_id 2
  > ;;
  > EOF
  val fake_id : (int -[Asgmt]-> int) = <fun>
  val x : int = 1
  val y : int = 1

  $ cat << EOF | ./demo.exe -
  > let fake_id =
  >   let r = ref [] in
  >   fun x ->
  >     match !r with
  >     | [] ->
  >       let o = r := x :: [] in
  >       x
  >     | hd :: tl -> hd
  > ;;
  > let x = fake_id 1
  > let y = fake_id "s"
  > ;;
  > EOF
  Error: Unification failed on string and int

  $ cat << EOF | ./demo.exe -
  > let fake_snd = fun o ->
  >   let r = ref [] in
  >   fun x ->
  >     match !r with
  >     | [] ->
  >       let o = r := x :: [] in
  >       x
  >     | hd :: tl -> hd
  > ;;
  > 
  > let x = fake_snd 1 2
  > let y = fake_snd "2" false
  > let fake_id = fake_snd 1
  > ;;
  val fake_snd : ('t0 --> ('t1 -[Asgmt]-> 't1)) = <fun>
  val x : int = 2
  val y : bool = false
  val fake_id : ('_weak_t0 -[Asgmt]-> '_weak_t0) = <fun>
