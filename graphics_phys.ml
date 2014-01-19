
type color = float * float * float
type point = float * float
let objects = ref [(1.0,0.0,0.0),[(0.0,0.0);(1.0,0.0);(1.0,1.0)]]
let cb = ref (fun a -> !objects)

type gobject = point list * color


let rs ~w:x ~h:y =
  ()

let init_graphics update_shapes () =
  cb := update_shapes;
  ignore( Glut.init Sys.argv );
  Glut.initDisplayMode ~double_buffer:true ~multisample:true ();
  ignore (Glut.createWindow ~title:"OpenGL Demo");
  ()

let _ =
  init_graphics Model.time_step ();
  let pi = 4.0 *. atan 1.0 in
  GlMat.rotate ~angle: (pi/.2.) ~z:1. ();
  let render () =
    GlClear.clear [ `color ];
    objects := !cb (Sys.time());
    let drawobj ((red,green,blue),pl) =
      GlMat.load_identity ();
      GlDraw.color (red,green,blue);
      GlDraw.polygon_mode `both `fill;
      GlDraw.begins `polygon;
      let ps =pl in
      List.iter GlDraw.vertex2 ps;
      GlDraw.ends ();
    in
    List.iter drawobj !objects;
    Glut.swapBuffers () in
  GlMat.mode `modelview;
  Glut.displayFunc ~cb:render;
  Glut.idleFunc ~cb:(Some Glut.postRedisplay);
  Glut.mainLoop ();;

