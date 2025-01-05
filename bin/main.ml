open Raylib

type player = {
  body : Rectangle.t;
  color : Color.t;
}

let p = {
  body = Rectangle.create 200.0 300.0 20.0 20.0;
  color = Color.black;
}

let create_enemies max_x max_y n =
  List.init n (fun _ ->
    let x = Random.float max_x in
    let y = Random.float max_y in
    {body = (Rectangle.create x y 20.0 20.0); color = Color.red})

let p_move p =
  let p_move_x =
    if is_key_down Key.A || is_key_down Key.Left then
      Rectangle.create ((Rectangle.x p.body) -. 5.0) (Rectangle.y p.body) (Rectangle.width p.body) (Rectangle.height p.body)
    else if is_key_down Key.D || is_key_down Key.Right then
      Rectangle.create ((Rectangle.x p.body) +. 5.0) (Rectangle.y p.body) (Rectangle.width p.body) (Rectangle.height p.body)
    else p.body
  in

  let p_move_y px =
    if is_key_down Key.W || is_key_down Key.Up then
      Rectangle.create (Rectangle.x px) ((Rectangle.y px) -. 5.0) (Rectangle.width p.body) (Rectangle.height p.body)
    else if is_key_down Key.S || is_key_down Key.Down then
      Rectangle.create (Rectangle.x px) ((Rectangle.y px) +. 5.0) (Rectangle.width p.body) (Rectangle.height p.body)
    else px
  in

  { body = p_move_x |> p_move_y; color = p.color }

let lerp a b s =
  let distance = abs_float (a -. b) in
  if distance < s then b
  else if a < b then a +. s
  else a -. s

let e_move e p = {
    body = Rectangle.create (lerp (Rectangle.x e.body) (Rectangle.x p.body) 1.0) (lerp (Rectangle.y e.body) (Rectangle.y p.body) 1.0) (Rectangle.width e.body) (Rectangle.height e.body);
    color = e.color;
  }

let setup () =
  init_window 800 600 "game";
  set_target_fps 60
let e = create_enemies 800.0 600.0 5

let rec loop p e () =
  if window_should_close () then close_window ()
  else begin
    let e = List.map (
      fun e -> if check_collision_recs (p_move p).body e.body then exit 0 else e_move e p
    ) e in

    let p =
      let p = p_move p in
      if List.exists (
      fun e -> check_collision_recs (p_move p).body e.body
    ) e then exit 0 else p
    in

  let distance = List.fold_left (
    fun acc ei ->
    let d = (abs_float ((Rectangle.x p.body) -. (Rectangle.x ei.body))) ** 2.0 +. (abs_float ((Rectangle.y p.body) -. (Rectangle.y ei.body))) ** 2.0 in
    if d < acc then d else acc
  ) max_float e in

  let p = if sqrt distance < 100.0
    then { body = p.body; color = Color.magenta; }
    else { body = p.body; color = Color.black; }
  in

  begin_drawing ();
  clear_background Color.skyblue; (*draw background*)
  List.iter (fun e -> draw_rectangle_rec e.body e.color) e;
  draw_rectangle_rec p.body p.color; (*player player*)
  end_drawing ();
  loop p e ()
end

let () = setup () |> loop p e
