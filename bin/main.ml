open Raylib

type player = {
  body : Rectangle.t;
  color : Color.t;
}

let p = {
  body = Rectangle.create 200.0 300.0 20.0 20.0;
  color = Color.black;
}

let e = {
  body = Rectangle.create 400.0 200.0 20.0 20.0;
  color = Color.red;
}

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

let setup () =
  init_window 800 600 "game";
  set_target_fps 60

let rec loop p () =
  if window_should_close () then close_window ()
  else begin
  let p = if check_collision_recs (p_move p).body e.body == false then p_move p else p in
  begin_drawing ();
  clear_background Color.skyblue; (*draw background*)
  draw_rectangle_rec e.body e.color; (*enemy player*)
  draw_rectangle_rec p.body p.color; (*player player*)
  draw_text "Fuck yeah we doing an OCaml Game!" 400 300 20 Color.black;
  end_drawing ();
  loop p ()
end

let () = setup () |> loop p
