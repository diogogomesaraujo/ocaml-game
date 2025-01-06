open Raylib

type bullet = {
  body : Rectangle.t;
  color : Color.t;
  direction : Vector2.t;
  (*lifespan : int;*)
}

type player = {
  body : Rectangle.t;
  color : Color.t;
}

let player = {
  body = Rectangle.create 200.0 300.0 20.0 20.0;
  color = Color.black;
}

let create_enemies max_x max_y n =
  List.init n (fun _ ->
    let x = Random.float max_x in
    let y = Random.float max_y in
    {body = (Rectangle.create x y 20.0 20.0); color = Color.red})

let create_bullet (bullets : bullet list) target player =
  let magnitude dx dy = (sqrt (dx *. dx +. dy *. dy)) in
  let normalize dx dy = Vector2.create (dx /. magnitude dx dy) (dy /. magnitude dx dy) in
  {
    body = Rectangle.create (Rectangle.x player.body) (Rectangle.y player.body) 5.0 5.0;
    color = Color.white;
    direction = normalize (Vector2.x target -. Rectangle.x player.body) (Vector2.y target -. Rectangle.y player.body);
  } :: bullets

let player_move player s =
  let player_move_x =
    if is_key_down Key.A || is_key_down Key.Left then
      Rectangle.create ((Rectangle.x player.body) -. s) (Rectangle.y player.body) (Rectangle.width player.body) (Rectangle.height player.body)
    else if is_key_down Key.D || is_key_down Key.Right then
      Rectangle.create ((Rectangle.x player.body) +. s) (Rectangle.y player.body) (Rectangle.width player.body) (Rectangle.height player.body)
    else player.body
  in

  let player_move_y px =
    if is_key_down Key.W || is_key_down Key.Up then
      Rectangle.create (Rectangle.x px) ((Rectangle.y px) -. s) (Rectangle.width player.body) (Rectangle.height player.body)
    else if is_key_down Key.S || is_key_down Key.Down then
      Rectangle.create (Rectangle.x px) ((Rectangle.y px) +. s) (Rectangle.width player.body) (Rectangle.height player.body)
    else px
  in

  ({ body = player_move_x |> player_move_y; color = player.color }, if player.body == player_move_y player_move_x then false else true)

let lerp a b s =
  let distance = abs_float (a -. b) in
  if distance < s then b
  else if a < b then a +. s
  else a -. s

let bullet_move (bullet : bullet) : bullet = {
  body = Rectangle.create (Rectangle.x bullet.body +. (Vector2.x bullet.direction *. 10.0)) (Rectangle.y bullet.body +. (Vector2.y bullet.direction *. 10.0)) 5.0 5.0;
  color = Color.white;
  direction = bullet.direction;
}

let enemy_move enemy player = {
    body = Rectangle.create (lerp (Rectangle.x enemy.body) (Rectangle.x player.body) 1.) (lerp (Rectangle.y enemy.body) (Rectangle.y player.body) 1.) (Rectangle.width enemy.body) (Rectangle.height enemy.body);
    color = enemy.color;
  }

let cam_update player =
  Camera2D.create (Vector2.create 400.0 300.0) (Vector2.create (Rectangle.x player.body) (Rectangle.y player.body)) 0.0 1.0

let setup () =
  init_window 800 600 "game";
  set_target_fps 60
let enemies = create_enemies 800.0 600.0 10
let (bullets : bullet list) = []

let mouse_pos cam =
  get_screen_to_world_2d
    (Vector2.create
      (get_mouse_x () |> float_of_int)
      (get_mouse_y () |> float_of_int))
    cam

let rec loop player enemies (bullets : bullet list) player_speed () =
  if window_should_close () then close_window ()
  else begin
    if List.length enemies == 0 then exit 0;
    let cam = cam_update player in

    let (player_move, player_is_moving) = player_move player player_speed in

    let player_speed =
      match player_is_moving with
      | true when player_speed < 2.0 -> player_speed +. 0.2
      | true -> player_speed
      | false -> 0.0
    in

    let enemies = List.map (
      fun enemy -> if check_collision_recs player_move.body enemy.body then exit 0 else enemy_move enemy player
    ) enemies in

    let new_bullets = ref bullets in (*think of a way without mutability*)

    let enemies = List.filter (
      fun enemy ->
      new_bullets := List.filter (
        fun (bullet : bullet) ->
        not (
          List.exists (
          fun enemy -> check_collision_recs enemy.body bullet.body
        )
        enemies
        )
      ) bullets;
      not (List.exists (
        fun (bullet: bullet) -> check_collision_recs enemy.body bullet.body
      ) bullets)
    ) enemies in

    let (bullets: bullet list) = !new_bullets in

    let player =
      let player = player_move in
      if List.exists (
      fun enemy -> check_collision_recs player_move.body enemy.body
    ) enemies then exit 0 else player
    in

  let distance = List.fold_left (
    fun acc enemy ->
    let d = (abs_float ((Rectangle.x player.body) -. (Rectangle.x enemy.body))) ** 2.0 +. (abs_float ((Rectangle.y player.body) -. (Rectangle.y enemy.body))) ** 2.0 in
    if d < acc then d else acc
  ) max_float enemies in

  let mouse_pos = mouse_pos cam in

  let bullets = if is_key_pressed Key.Space then create_bullet bullets mouse_pos player else bullets in

  let bullets = List.map (fun bullet -> bullet_move bullet) bullets in

  let player = if sqrt distance < 100.0
    then { body = player.body; color = Color.magenta; }
    else { body = player.body; color = Color.black; }
  in

  begin_drawing ();
  begin_mode_2d cam;
  clear_background Color.skyblue; (*draw background*)
  List.iter (fun enemy -> draw_rectangle_rec enemy.body enemy.color) enemies;
  draw_rectangle_rec player.body player.color; (*player player*)
  List.iter (fun (bullet : bullet) -> draw_rectangle_rec bullet.body bullet.color) bullets;
  end_drawing ();
  loop player enemies bullets player_speed()
end

let () = setup () |> loop player enemies bullets 0.0
