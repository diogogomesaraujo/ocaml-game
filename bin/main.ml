[@@@warning "-37-32"]
open Raylib

let sprite_size = 32.
let screen_size = Vector2.create 800. 600.

type player_state =
  | Idle
  | Move
  | Shoot

type player_x_side =
  | Right
  | Left

type player_y_side =
  | Front
  | Back

type bullet = {
  body : Rectangle.t;
  color : Color.t;
  direction : Vector2.t;
  (*lifespan : int;*)
}

type player = {
  body : Rectangle.t;
  color : Color.t;
  count : int;
}

let player = {
  body = Rectangle.create (Vector2.x screen_size /. 2.) (Vector2.y screen_size /. 2.)  (sprite_size /. 2.) sprite_size;
  color = Color.black;
  count = 0;
}

let create_enemies max_x max_y n =
  List.init n (fun _ ->
    let x = Random.float max_x in
    let y = Random.float max_y in
    {body = (Rectangle.create x y sprite_size sprite_size); color = Color.red; count = 0;})

let create_bullet (bullets : bullet list) target player =
  let magnitude dx dy = (sqrt (dx *. dx +. dy *. dy)) in
  let normalize dx dy = Vector2.create (dx /. magnitude dx dy) (dy /. magnitude dx dy) in
  {
    body = Rectangle.create (Rectangle.x player.body) (Rectangle.y player.body) 5.0 5.0;
    color = Color.white;
    direction = normalize (Vector2.x target -. Rectangle.x player.body) (Vector2.y target -. Rectangle.y player.body);
  } :: bullets

let player_move player s =
  let facing_x = ref Right in
  let facing_y = ref Front in
  let player_move_x =
    if is_key_down Key.A || is_key_down Key.Left then
    begin
      facing_x := Left;
      Rectangle.create ((Rectangle.x player.body) -. s) (Rectangle.y player.body) (Rectangle.width player.body) (Rectangle.height player.body)
    end
    else if is_key_down Key.D || is_key_down Key.Right then
    begin
      facing_x := Right;
      Rectangle.create ((Rectangle.x player.body) +. s) (Rectangle.y player.body) (Rectangle.width player.body) (Rectangle.height player.body)
    end
    else player.body
  in

  let player_move_y px =
    if is_key_down Key.W || is_key_down Key.Up then
    begin
      facing_y := Back;
      Rectangle.create (Rectangle.x px) ((Rectangle.y px) -. s) (Rectangle.width player.body) (Rectangle.height player.body)
    end
    else if is_key_down Key.S || is_key_down Key.Down then
    begin
      facing_y := Front;
      Rectangle.create (Rectangle.x px) ((Rectangle.y px) +. s) (Rectangle.width player.body) (Rectangle.height player.body)
    end
    else px
  in

  (
    {
      body = player_move_x |> player_move_y;
      color = player.color;
      count = begin if player.body == player_move_y player_move_x || player.count >= 3 * 8 then 0 else player.count + 1 end;
    },
    !facing_x,
    !facing_y,
    begin if player.body == player_move_y player_move_x then false else true end
  )

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
    count = 0;
  }

let cam_update player =
  Camera2D.create (Vector2.create (Vector2.x screen_size /. 2.)  (Vector2.y screen_size /. 2.) ) (Vector2.create (Rectangle.x player.body) (Rectangle.y player.body)) 0.0 3.0

let mouse_pos cam =
  get_screen_to_world_2d
    (Vector2.create
      (get_mouse_x () |> float_of_int)
      (get_mouse_y () |> float_of_int))
    cam

let draw_player player_state player_texture player =
  let count = player.count / 8 |> float_of_int in
  draw_texture_rec
    player_texture
    ( match player_state with
      | Idle -> Rectangle.create 0. 0. sprite_size sprite_size
      | Move -> Rectangle.create (sprite_size *. (1. +. count)) 0. sprite_size sprite_size
      | Shoot -> Rectangle.create (sprite_size *. (4. +. count)) 0. sprite_size sprite_size
    )
    (Vector2.create (Rectangle.x player.body -. sprite_size /. 4.) (Rectangle.y player.body))
    Color.white

let setup =
  init_window (Vector2.x screen_size |> int_of_float)  (Vector2.y screen_size |> int_of_float)  "game";
  set_target_fps 60;
  Random.self_init ();
  let player_texture_front_right = load_texture "public/persona-front-right.png" in
  let player_texture_front_left = load_texture "public/persona-front-left.png" in
  let player_texture_back_right = load_texture "public/persona-back-right.png" in
  let player_texture_back_left = load_texture "public/persona-back-left.png" in
  (player_texture_front_right, player_texture_front_left, player_texture_back_right, player_texture_back_left)

let enemies = create_enemies (Vector2.x screen_size) (Vector2.y screen_size) 2

let (bullets : bullet list) = []

let rec loop player enemies (bullets : bullet list) player_speed player_texture_front_right player_texture_front_left player_texture_back_right player_texture_back_left  () =
  if window_should_close () then close_window ()
  else begin
    if List.length enemies == 0 then exit 0;
    let cam = cam_update player in

    let (player_move, facing_x, facing_y, player_is_moving) = player_move player player_speed in

    let player_state = if player_is_moving then Move else Idle in

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
    then { body = player.body; color = Color.magenta; count = player.count;}
    else { body = player.body; color = Color.black; count = player.count;}
  in

  begin_drawing ();
  begin_mode_2d cam;
  clear_background Color.skyblue; (*draw background*)
  List.iter (fun enemy -> draw_rectangle_rec enemy.body enemy.color) enemies;
  draw_player player_state (
    match (facing_x, facing_y) with
    | (Right, Front) -> player_texture_front_right
    | (Right, Back) -> player_texture_back_right
    | (Left, Front) -> player_texture_front_left
    | (Left, Back) -> player_texture_back_left
  ) player; (*player player*)
  List.iter (fun (bullet : bullet) -> draw_rectangle_rec bullet.body bullet.color) bullets;
  end_drawing ();
  loop player enemies bullets player_speed player_texture_front_right player_texture_front_left player_texture_back_right player_texture_back_left ()
end

let () =
  let (player_texture_front_right, player_texture_front_left, player_texture_back_right, player_texture_back_left) = setup in
  loop player enemies bullets 0.0 player_texture_front_right player_texture_front_left player_texture_back_right player_texture_back_left ()
