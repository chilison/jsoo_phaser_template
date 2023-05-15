open Js_of_ocaml
open Firebug
open Js
open Dungeon
open TurnManager
open Player

class type twist = object end

class type spritesheetConfig =
  object
    method frameWidth : int readonly_prop
    method frameHeight : int readonly_prop
    method spacing : int readonly_prop
  end

class type loader_plugin =
  object
    method spritesheet :
      js_string t -> js_string t -> spritesheetConfig t -> unit meth
  end

let caml_scene =
  object%js (this)
    method init = ()

    method preload =
      let loader : loader_plugin t =
        (Js.Unsafe.eval_string {|x => x.load |} : _ -> _) this
      in
      let spritesheet_config : spritesheetConfig Js.t =
        object%js
          val frameWidth = 16
          val frameHeight = 16
          val spacing = 1
        end
      in

      loader##spritesheet (Js.string "tiles")
        (Js.string "assets/colored.png")
        spritesheet_config

    method create () =
      let a = (Dungeon.dungeon this)##initialize () in

      let player = new Player.character in
      player#make this 15 15;

      Dungeon.player_character := player;

      let monster_1 = new Monster.basicMonster in
      monster_1#make this 15 20;
      let monster_2 = new Monster.basicMonster in
      monster_2#make this 20 10;
      let monster_3 = new Monster.basicMonster in
      monster_3#make this 76 10;
      let monster_4 = new Monster.basicMonster in
      monster_4#make this 29 24;
      let monster_5 = new Monster.basicMonster in
      monster_5#make this 29 20;

      TurnManager.tm##addEntity player;
      TurnManager.tm##addEntity monster_1;
      TurnManager.tm##addEntity monster_2;
      TurnManager.tm##addEntity monster_3;
      TurnManager.tm##addEntity monster_4;
      TurnManager.tm##addEntity monster_5;
      a

    method update () =
      if TurnManager.tm##over () == true then TurnManager.tm##refresh ();
      
      TurnManager.tm##turn (this :> twist t)
  end

type game

let make_game : ('config -> game Js.t) Js.constr =
  Js.Unsafe.global##._Phaser##._Game

let config =
  object%js (self)
    val type_ = Js.Unsafe.eval_string {| Phaser.AUTO |}
    val width = 80 * 16
    val height = 50 * 16
    val backgroundColor = Js.string "#000"
    val parent = Js.string "game"
    val pixelArt = Js._true
    val zoom = 1
    val scene = caml_scene

    val physics =
      object%js
        val default = Js.string "arcade"

        val arcade =
          object%js
            val gravity =
              object%js
                val y = 0
              end
          end
      end
  end

let _game = new%js make_game config