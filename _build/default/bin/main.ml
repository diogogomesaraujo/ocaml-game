open Raylib

type player = {
  pos : Vector2.t;
  size : Vector2.t;
  color : Color.t;
}

let p = {
  pos = Vector2.create 400.0 300.0;
  size = Vector2.create 20.0 20.0;
  color = Color.black;
}
let p_move p =
  let p_move_x =
    if is_key_down Key.A || is_key_down Key.Left then
      Vector2.create ((Vector2.x p.pos) -. 5.0) (Vector2.y p.pos)
    else if is_key_down Key.D || is_key_down Key.Right then
      Vector2.create ((Vector2.x p.pos) +. 5.0) (Vector2.y p.pos)
    else p.pos
  in

  let p_move_y px =
    if is_key_down Key.W || is_key_down Key.Up then
      Vector2.create (Vector2.x px) ((Vector2.y px) -. 5.0)
    else if is_key_down Key.S || is_key_down Key.Down then
      Vector2.create (Vector2.x px) ((Vector2.y px) +. 5.0)
    else px
  in

  { pos = p_move_x |> p_move_y ; size = p.size; color = p.color }

let setup () =
  init_window 800 600 "game";
  set_target_fps 60

let rec loop p () =
  if window_should_close () then close_window ()
  else begin
  let p = p_move p in
  begin_drawing ();
  clear_background Color.skyblue; (*draw background*)
  draw_rectangle_v p.pos p.size p.color;
  draw_text "Fuck yeah we doing an OCaml Game!" 400 300 20 Color.black;
  end_drawing ();
  loop p ()
end

let () = setup () |> loop p
